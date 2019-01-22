package one.mir.it.sync.matcher

import com.typesafe.config.Config
import one.mir.it.api.SyncHttpApi._
import one.mir.it.api.SyncMatcherHttpApi._
import one.mir.it.matcher.MatcherSuiteBase
import one.mir.it.sync._
import one.mir.it.sync.matcher.config.MatcherDefaultConfig._
import one.mir.it.util._
import one.mir.state.ByteStr
import one.mir.transaction.assets.exchange.{AssetPair, Order, OrderType}

import scala.concurrent.duration._
import scala.util.Random

class MatcherRestartTestSuite extends MatcherSuiteBase {
  override protected def nodeConfigs: Seq[Config] = Configs
  private def orderVersion                        = (Random.nextInt(2) + 1).toByte

  "check order execution" - {
    // Alice issues new asset
    val aliceAsset =
      aliceNode.issue(aliceAcc.address, "DisconnectCoin", "Alice's coin for disconnect tests", someAssetAmount, 0, reissuable = false, issueFee, 2).id
    matcherNode.waitForTransaction(aliceAsset)

    val aliceMirPair = AssetPair(ByteStr.decodeBase58(aliceAsset).toOption, None)
    // check assets's balances
    matcherNode.assertAssetBalance(aliceAcc.address, aliceAsset, someAssetAmount)
    matcherNode.assertAssetBalance(matcherAcc.address, aliceAsset, 0)

    "make order and after matcher's restart try to cancel it" in {
      // Alice places sell order
      val aliceOrder = matcherNode
        .placeOrder(aliceAcc, aliceMirPair, OrderType.SELL, 500, 2.mir * Order.PriceConstant, matcherFee, orderVersion)
      aliceOrder.status shouldBe "OrderAccepted"
      val firstOrder = aliceOrder.message.id

      matcherNode.waitOrderStatus(aliceMirPair, firstOrder, "Accepted")

      // check that order is correct
      val orders = matcherNode.orderBook(aliceMirPair)
      orders.asks.head.amount shouldBe 500
      orders.asks.head.price shouldBe 2.mir * Order.PriceConstant

      // sell order should be in the aliceNode orderbook
      matcherNode.fullOrderHistory(aliceAcc).head.status shouldBe "Accepted"

      // reboot matcher's node
      docker.killAndStartContainer(dockerNodes().head)
      Thread.sleep(60.seconds.toMillis)

      val height = nodes.map(_.height).max

      matcherNode.waitForHeight(height + 1, 40.seconds)
      matcherNode.waitOrderStatus(aliceMirPair, firstOrder, "Accepted")
      matcherNode.fullOrderHistory(aliceAcc).head.status shouldBe "Accepted"

      val orders1 = matcherNode.orderBook(aliceMirPair)
      orders1.asks.head.amount shouldBe 500
      orders1.asks.head.price shouldBe 2.mir * Order.PriceConstant

      val aliceSecondOrder =
        matcherNode.placeOrder(aliceAcc, aliceMirPair, OrderType.SELL, 500, 2.mir * Order.PriceConstant, matcherFee, orderVersion, 5.minutes)
      aliceSecondOrder.status shouldBe "OrderAccepted"

      val orders2 = matcherNode.orderBook(aliceMirPair)
      orders2.asks.head.amount shouldBe 1000
      orders2.asks.head.price shouldBe 2.mir * Order.PriceConstant

      val cancel = matcherNode.cancelOrder(aliceAcc, aliceMirPair, firstOrder)
      cancel.status should be("OrderCanceled")

      val orders3 = matcherNode.orderBook(aliceMirPair)
      orders3.asks.head.amount shouldBe 500

      matcherNode.waitOrderStatus(aliceMirPair, firstOrder, "Cancelled")
      matcherNode.fullOrderHistory(aliceAcc).head.status shouldBe "Accepted"
    }
  }
}
