package one.mir.it.sync.matcher

import com.typesafe.config.Config
import one.mir.account.PrivateKeyAccount
import one.mir.it.api.SyncHttpApi._
import one.mir.it.api.SyncMatcherHttpApi._
import one.mir.it.matcher.MatcherSuiteBase
import one.mir.it.sync._
import one.mir.it.sync.matcher.config.MatcherDefaultConfig._
import one.mir.state.ByteStr
import one.mir.transaction.assets.exchange.{AssetPair, Order, OrderType}

import scala.concurrent.duration._
import scala.util.Random

class MatcherMassOrdersTestSuite extends MatcherSuiteBase {
  override protected def nodeConfigs: Seq[Config] = Configs

  private def orderVersion = (Random.nextInt(2) + 1).toByte

  "Create orders with statuses FILL, PARTIAL, CANCELLED, ACTIVE" - {
    // Alice issues new assets
    val aliceAsset =
      aliceNode.issue(aliceAcc.address, "AliceCoin", "AliceCoin for matcher's tests", someAssetAmount, 0, reissuable = false, smartIssueFee, 2).id

    val aliceSecondAsset = aliceNode
      .issue(aliceAcc.address, "AliceSecondCoin", "AliceSecondCoin for matcher's tests", someAssetAmount, 0, reissuable = false, smartIssueFee, 2)
      .id
    Seq(aliceAsset, aliceSecondAsset).foreach(matcherNode.waitForTransaction(_))

    val aliceMirPair       = AssetPair(ByteStr.decodeBase58(aliceAsset).toOption, None)
    val aliceSecondMirPair = AssetPair(ByteStr.decodeBase58(aliceSecondAsset).toOption, None)

    // Check balances on Alice's account
    aliceNode.assertAssetBalance(aliceAcc.address, aliceAsset, someAssetAmount)
    aliceNode.assertAssetBalance(aliceAcc.address, aliceSecondAsset, someAssetAmount)
    matcherNode.assertAssetBalance(matcherAcc.address, aliceAsset, 0)

    val transfer1ToBobId = aliceNode.transfer(aliceAcc.address, bobAcc.address, someAssetAmount / 2, minFee, Some(aliceAsset), None, 2).id
    matcherNode.waitForTransaction(transfer1ToBobId)

    val transfer2ToBobId = aliceNode.transfer(aliceAcc.address, bobAcc.address, someAssetAmount / 2, minFee, Some(aliceSecondAsset), None, 2).id
    matcherNode.waitForTransaction(transfer2ToBobId)

    matcherNode.assertAssetBalance(bobAcc.address, aliceAsset, someAssetAmount / 2)
    matcherNode.assertAssetBalance(bobAcc.address, aliceSecondAsset, someAssetAmount / 2)

    // Alice places sell orders
    val aliceOrderIdFill = matcherNode
      .placeOrder(aliceAcc, aliceSecondMirPair, OrderType.SELL, 3, Order.PriceConstant, matcherFee, orderVersion, 10.minutes)
      .message
      .id

    val alicePartialOrderId = matcherNode
      .placeOrder(aliceAcc, aliceSecondMirPair, OrderType.SELL, 3, Order.PriceConstant, matcherFee, orderVersion, 10.minutes)
      .message
      .id

    val aliceOrderToCancelId =
      matcherNode
        .placeOrder(aliceAcc, aliceSecondMirPair, OrderType.SELL, 3, Order.PriceConstant, matcherFee, orderVersion, 70.seconds)
        .message
        .id

    val aliceActiveOrderId = matcherNode
      .placeOrder(aliceAcc, aliceSecondMirPair, OrderType.SELL, 3, Order.PriceConstant + 100000000, matcherFee, orderVersion, 10.minutes)
      .message
      .id

    matcherNode.waitOrderStatus(aliceSecondMirPair, aliceOrderToCancelId, "Cancelled", 2.minutes)

    //Bob orders should partially fill one Alice order and fill another
    ordersRequestsGen(2, bobAcc, aliceSecondMirPair, OrderType.BUY, 2)

    //check orders after filling
    matcherNode.waitOrderStatus(aliceSecondMirPair, alicePartialOrderId, "PartiallyFilled")

    orderStatus(aliceAcc, aliceSecondMirPair, aliceOrderIdFill, "Filled")
    orderStatus(aliceAcc, aliceSecondMirPair, alicePartialOrderId, "PartiallyFilled")

    "Mass orders creation with random lifetime. Active orders still in list" in {
      matcherNode.ordersByAddress(aliceAcc, activeOnly = false).length shouldBe 4
      matcherNode.ordersByAddress(aliceAcc, activeOnly = true).length shouldBe 2

      matcherNode.ordersByAddress(bobAcc, activeOnly = false).length shouldBe 2
      matcherNode.ordersByAddress(bobAcc, activeOnly = true).length shouldBe 0

      val orderIds = matcherNode.fullOrderHistory(aliceAcc).map(_.id)

      orderIds should contain(aliceActiveOrderId)

      ordersRequestsGen(orderLimit + 1, aliceAcc, aliceMirPair, OrderType.SELL, 3)

      //wait for some orders cancelled
      Thread.sleep(5000)
      val bobsOrderIds = ordersRequestsGen(orderLimit + 1, bobAcc, aliceMirPair, OrderType.BUY, 2)
      Thread.sleep(5000)

      // Alice check that order Active order is still in list
      val orderIdsAfterMatching = matcherNode.fullOrderHistory(aliceAcc).map(_.id)

      orderIdsAfterMatching should contain(aliceActiveOrderId)
      orderIdsAfterMatching should contain(alicePartialOrderId)

      matcherNode.waitOrderStatus(aliceSecondMirPair, aliceActiveOrderId, "Accepted")
      matcherNode.waitOrderStatus(aliceSecondMirPair, alicePartialOrderId, "PartiallyFilled")

      matcherNode.fullOrderHistory(bobAcc).map(_.id) should equal(bobsOrderIds.drop(1).reverse)
      matcherNode.orderHistoryByPair(bobAcc, aliceMirPair).map(_.id) should equal(bobsOrderIds.drop(1).reverse)
    }

    "Filled and Cancelled orders should be after Partial And Accepted" in {
      val lastIdxOfActiveOrder =
        matcherNode.fullOrderHistory(aliceAcc).lastIndexWhere(o => o.status.equals("Accepted") || o.status.equals("PartiallyFilled"))
      val firstIdxOfClosedOrder = matcherNode.fullOrderHistory(aliceAcc).indexWhere(o => o.status.equals("Filled") || o.status.equals("Cancelled"))
      lastIdxOfActiveOrder should be < firstIdxOfClosedOrder
    }

    "Accepted and PartiallyFilled orders should be sorted by timestamp." in {
      val activeAndPartialOrders =
        matcherNode.fullOrderHistory(aliceAcc).filter(o => o.status.equals("Accepted") || o.status.equals("PartiallyFilled")).map(_.timestamp)
      activeAndPartialOrders.reverse shouldBe sorted
    }

    "Filled and Cancelled orders should be sorted by timestamp." in {
      val filledAndCancelledOrders =
        matcherNode.fullOrderHistory(aliceAcc).filter(o => o.status.equals("Filled") || o.status.equals("Cancelled")).map(_.timestamp)
      filledAndCancelledOrders.reverse shouldBe sorted
    }

    "check order history orders count after fill" in {
      val aliceOrderHistory = matcherNode.fullOrderHistory(aliceAcc)
      aliceOrderHistory.size shouldBe orderLimit
      val aliceOrderHistoryByPair = matcherNode.orderHistoryByPair(aliceAcc, aliceMirPair)
      aliceOrderHistoryByPair.size shouldBe orderLimit
    }

  }

  private def ordersRequestsGen(n: Int, sender: PrivateKeyAccount, assetPair: AssetPair, orderType: OrderType, amount: Long): Seq[String] = {
    val orderIds = 1 to n map (_ => {
      matcherNode
        .placeOrder(sender, assetPair, orderType, amount, Order.PriceConstant, matcherFee, orderVersion, (120 + Random.nextInt(70)).seconds)
        .message
        .id
    })
    orderIds
  }

  private def orderStatus(sender: PrivateKeyAccount, assetPair: AssetPair, orderId: String, expectedStatus: String) =
    matcherNode.waitOrderStatus(assetPair, orderId, expectedStatus)
}
