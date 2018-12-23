package one.mir.it.sync.matcher

import com.typesafe.config.Config
import one.mir.it.ReportingTestName
import one.mir.it.api.SyncHttpApi._
import one.mir.it.api.SyncMatcherHttpApi._
import one.mir.it.sync._
import one.mir.it.sync.matcher.config.MatcherPriceAssetConfig._
import one.mir.it.transactions.NodesFromDocker
import one.mir.transaction.assets.exchange.OrderType.BUY
import org.scalatest._

class TradingMarketsTestSuite
    extends FreeSpec
    with Matchers
    with BeforeAndAfterAll
    with CancelAfterFailure
    with NodesFromDocker
    with ReportingTestName {
  override protected def nodeConfigs: Seq[Config] = Configs

  private def matcher = dockerNodes().head
  private def alice   = dockerNodes()(1)

  val (amount, price) = (1000L, 1000000000L)

  val wctTxId = matcher.signedIssue(createSignedIssueRequest(IssueWctTx)).id
  matcher.waitForTransaction(wctTxId)

  "When some orders were placed and matcher was restarted" - {
    val order = matcher.placeOrder(alice.privateKey, wctMirPair, BUY, amount, price, matcherFee).message.id
    matcher.waitOrderStatus(wctMirPair, order, "Accepted")

    docker.restartNode(matcher)

    "Trading markets have info about all asset pairs" in {
      val markets = matcher.tradingMarkets().markets
      markets.size shouldBe 1
      markets.head.amountAssetName shouldNot be("Unknown")
      markets.head.priceAssetName shouldNot be("Unknown")
    }
  }
}
