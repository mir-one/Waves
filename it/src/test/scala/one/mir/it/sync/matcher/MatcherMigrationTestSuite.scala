package one.mir.it.sync.matcher

import com.typesafe.config.Config
import one.mir.it.api.SyncHttpApi._
import one.mir.it.api.SyncMatcherHttpApi._
import one.mir.it.matcher.MatcherSuiteBase
import one.mir.it.sync._
import one.mir.it.sync.matcher.config.MatcherDefaultConfig._
import one.mir.state.ByteStr
import one.mir.transaction.assets.exchange.{AssetPair, OrderType}

import scala.concurrent.duration._

class MatcherMigrationTestSuite extends MatcherSuiteBase {
  override protected def nodeConfigs: Seq[Config] = Configs

  "issue asset and run migration tool inside container" - {
    // Alice issues new asset
    val aliceAsset =
      aliceNode
        .issue(aliceAcc.address,
               "DisconnectCoin",
               "Alice's coin for disconnect tests",
               1000 * someAssetAmount,
               8,
               reissuable = false,
               smartIssueFee,
               2)
        .id
    nodes.waitForHeightAriseAndTxPresent(aliceAsset)

    val aliceBalance = aliceNode.accountBalances(aliceAcc.address)._2
    val t1           = aliceNode.transfer(aliceAcc.address, matcherAcc.address, aliceBalance - minFee - 250000, minFee, None, None, 2).id
    nodes.waitForHeightAriseAndTxPresent(t1)

    val aliceMirPair = AssetPair(ByteStr.decodeBase58(aliceAsset).toOption, None)

    "place order and run migration tool" in {
      // Alice places sell order
      val aliceOrder = matcherNode
        .placeOrder(aliceAcc, aliceMirPair, OrderType.SELL, 3000000, 3000000, matcherFee)
      aliceOrder.status shouldBe "OrderAccepted"
      val firstOrder = aliceOrder.message.id

      // check order status
      matcherNode.waitOrderStatus(aliceMirPair, firstOrder, "Accepted")

      // sell order should be in the aliceNode orderbook
      matcherNode.fullOrderHistory(aliceAcc).head.status shouldBe "Accepted"

      val tbBefore = matcherNode.tradableBalance(aliceAcc, aliceMirPair)
      val rbBefore = matcherNode.reservedBalance(aliceAcc)

      // stop node, run migration tool and start node again
      docker.runMigrationToolInsideContainer(dockerNodes().head)
      Thread.sleep(60.seconds.toMillis)

      val height = nodes.map(_.height).max

      matcherNode.waitForHeight(height + 1, 40.seconds)
      matcherNode.orderStatus(firstOrder, aliceMirPair).status shouldBe "Accepted"
      matcherNode.fullOrderHistory(aliceAcc).head.status shouldBe "Accepted"

      val tbAfter = matcherNode.tradableBalance(aliceAcc, aliceMirPair)
      val rbAfter = matcherNode.reservedBalance(aliceAcc)

      assert(tbBefore == tbAfter)
      assert(rbBefore == rbAfter)

    }
  }
}
