package one.mir.it.sync.matcher

import com.typesafe.config.Config
import one.mir.it.api.SyncHttpApi._
import one.mir.it.api.SyncMatcherHttpApi._
import one.mir.it.matcher.MatcherSuiteBase
import one.mir.it.sync._
import one.mir.it.sync.matcher.config.MatcherPriceAssetConfig._
import one.mir.it.util._
import one.mir.transaction.assets.exchange.{AssetPair, OrderType}

import scala.concurrent.duration._

class CancelOrderTestSuite extends MatcherSuiteBase {

  override protected def nodeConfigs: Seq[Config] = Configs

  private val mirBtcPair = AssetPair(None, Some(BtcId))

  override protected def beforeAll(): Unit = {
    super.beforeAll()

    Seq(IssueUsdTx, IssueBtcTx).map(createSignedIssueRequest).map(matcherNode.signedIssue).foreach { tx =>
      matcherNode.waitForTransaction(tx.id)
    }

    Seq(
      aliceNode.transfer(IssueUsdTx.sender.toAddress.stringRepr, aliceAcc.address, defaultAssetQuantity, 100000, Some(UsdId.toString), None, 2),
      bobNode.transfer(IssueBtcTx.sender.toAddress.stringRepr, bobAcc.address, defaultAssetQuantity, 100000, Some(BtcId.toString), None, 2)
    ).foreach { tx =>
      matcherNode.waitForTransaction(tx.id)
    }
  }

  "Order can be canceled" - {
    "by sender" in {
      val orderId = matcherNode.placeOrder(bobAcc, mirUsdPair, OrderType.SELL, 100.mir, 800, matcherFee).message.id
      matcherNode.waitOrderStatus(mirUsdPair, orderId, "Accepted", 1.minute)

      matcherNode.cancelOrder(bobAcc, mirUsdPair, orderId)
      matcherNode.waitOrderStatus(mirUsdPair, orderId, "Cancelled", 1.minute)

      matcherNode.orderHistoryByPair(bobAcc, mirUsdPair).collectFirst {
        case o if o.id == orderId => o.status shouldEqual "Cancelled"
      }
    }
    "with API key" in {
      val orderId = matcherNode.placeOrder(bobAcc, mirUsdPair, OrderType.SELL, 100.mir, 800, matcherFee).message.id
      matcherNode.waitOrderStatus(mirUsdPair, orderId, "Accepted", 1.minute)

      matcherNode.cancelOrderWithApiKey(orderId)
      matcherNode.waitOrderStatus(mirUsdPair, orderId, "Cancelled", 1.minute)

      matcherNode.fullOrderHistory(bobAcc).filter(_.id == orderId).head.status shouldBe "Cancelled"
      matcherNode.orderHistoryByPair(bobAcc, mirUsdPair).filter(_.id == orderId).head.status shouldBe "Cancelled"

      val orderBook = matcherNode.orderBook(mirUsdPair)
      orderBook.bids shouldBe empty
      orderBook.asks shouldBe empty
    }
  }

  "Cancel is rejected" - {
    "when request sender is not the sender of and order" in {
      val orderId = matcherNode.placeOrder(bobAcc, mirUsdPair, OrderType.SELL, 100.mir, 800, matcherFee).message.id
      matcherNode.waitOrderStatus(mirUsdPair, orderId, "Accepted", 1.minute)

      matcherNode.expectCancelRejected(matcherNode.privateKey, mirUsdPair, orderId)

      // Cleanup
      matcherNode.cancelOrder(bobAcc, mirUsdPair, orderId)
      matcherNode.waitOrderStatus(mirUsdPair, orderId, "Cancelled")
    }
  }

  "Batch cancel" - {
    "works for" - {
      "all orders placed by an address" in {
        matcherNode.fullOrderHistory(bobAcc)

        val usdOrderIds = 1 to 5 map { i =>
          matcherNode.placeOrder(bobAcc, mirUsdPair, OrderType.SELL, 100.mir + i, 400, matcherFee).message.id
        }

        matcherNode.assetBalance(bobAcc.toAddress.stringRepr, BtcId.base58)

        val btcOrderIds = 1 to 5 map { i =>
          matcherNode.placeOrder(bobAcc, mirBtcPair, OrderType.BUY, 100.mir + i, 400, matcherFee).message.id
        }

        (usdOrderIds ++ btcOrderIds).foreach(id => matcherNode.waitOrderStatus(mirUsdPair, id, "Accepted"))

        matcherNode.cancelAllOrders(bobAcc)

        (usdOrderIds ++ btcOrderIds).foreach(id => matcherNode.waitOrderStatus(mirUsdPair, id, "Cancelled"))
      }

      "a pair" in {
        val usdOrderIds = 1 to 5 map { i =>
          matcherNode.placeOrder(bobAcc, mirUsdPair, OrderType.SELL, 100.mir + i, 400, matcherFee).message.id
        }

        val btcOrderIds = 1 to 5 map { i =>
          matcherNode.placeOrder(bobAcc, mirBtcPair, OrderType.BUY, 100.mir + i, 400, matcherFee).message.id
        }

        (usdOrderIds ++ btcOrderIds).foreach(id => matcherNode.waitOrderStatus(mirUsdPair, id, "Accepted"))

        matcherNode.cancelOrdersForPair(bobAcc, mirBtcPair)

        btcOrderIds.foreach(id => matcherNode.waitOrderStatus(mirUsdPair, id, "Cancelled"))
        usdOrderIds.foreach(id => matcherNode.waitOrderStatus(mirUsdPair, id, "Accepted"))
      }
    }
  }
}
