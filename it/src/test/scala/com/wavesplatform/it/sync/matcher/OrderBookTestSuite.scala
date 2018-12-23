package one.mir.it.sync.matcher

import com.typesafe.config.Config
import one.mir.account.PrivateKeyAccount
import one.mir.it.api.SyncHttpApi._
import one.mir.it.api.SyncMatcherHttpApi._
import one.mir.it.matcher.MatcherSuiteBase
import one.mir.it.sync._
import one.mir.it.sync.matcher.config.MatcherPriceAssetConfig._
import one.mir.transaction.assets.exchange.Order.PriceConstant
import one.mir.transaction.assets.exchange.OrderType._

class OrderBookTestSuite extends MatcherSuiteBase {

  override protected def nodeConfigs: Seq[Config] = Configs

  Seq(IssueUsdTx, IssueWctTx).map(createSignedIssueRequest).map(matcherNode.signedIssue).foreach { tx =>
    matcherNode.waitForTransaction(tx.id)
  }

  Seq(
    aliceNode.transfer(IssueUsdTx.sender.toAddress.stringRepr, aliceAcc.address, defaultAssetQuantity, 100000, Some(UsdId.toString), None, 2),
    bobNode.transfer(IssueWctTx.sender.toAddress.stringRepr, bobAcc.address, defaultAssetQuantity, 100000, Some(WctId.toString), None, 2)
  ).foreach { tx =>
    matcherNode.waitForTransaction(tx.id)
  }

  case class ReservedBalances(wct: Long, usd: Long, mir: Long)
  def reservedBalancesOf(pk: PrivateKeyAccount): ReservedBalances = {
    val reservedBalances = matcherNode.reservedBalance(pk)
    ReservedBalances(
      reservedBalances.getOrElse(WctId.toString, 0),
      reservedBalances.getOrElse(UsdId.toString, 0),
      reservedBalances.getOrElse("MIR", 0)
    )
  }

  val (amount, price) = (1000L, PriceConstant)

  "When delete order book" - {
    val buyOrder        = matcherNode.placeOrder(aliceAcc, wctUsdPair, BUY, 2 * amount, price, matcherFee).message.id
    val anotherBuyOrder = matcherNode.placeOrder(aliceAcc, wctUsdPair, BUY, amount, price, matcherFee).message.id

    val submitted = matcherNode.placeOrder(bobAcc, wctUsdPair, SELL, amount, price, matcherFee).message.id

    val sellOrder = matcherNode.placeOrder(bobAcc, wctUsdPair, SELL, amount, 2 * price, matcherFee).message.id

    matcherNode.waitOrderStatus(wctUsdPair, buyOrder, "PartiallyFilled")
    matcherNode.waitOrderStatus(wctUsdPair, submitted, "Filled")

    val (aliceRBForOnePair, bobRBForOnePair) = (reservedBalancesOf(aliceAcc), reservedBalancesOf(bobAcc))

    val buyOrderForAnotherPair = matcherNode.placeOrder(aliceAcc, wctMirPair, BUY, amount, price, matcherFee).message.id
    val sellOrderForAnotherPair =
      matcherNode.placeOrder(bobAcc, wctMirPair, SELL, amount, 2 * price, matcherFee).message.id

    matcherNode.waitOrderStatus(wctMirPair, buyOrderForAnotherPair, "Accepted")
    matcherNode.waitOrderStatus(wctMirPair, sellOrderForAnotherPair, "Accepted")

    val (aliceRBForBothPairs, bobRBForBothPairs) = (reservedBalancesOf(aliceAcc), reservedBalancesOf(bobAcc))

    val marketStatusBeforeDeletion = matcherNode.marketStatus(wctUsdPair)

    matcherNode.deleteOrderBook(wctUsdPair)

    "orders by the pair should be canceled" in {
      matcherNode.waitOrderStatus(wctUsdPair, buyOrder, "Cancelled")
      matcherNode.waitOrderStatus(wctUsdPair, anotherBuyOrder, "Cancelled")
      matcherNode.waitOrderStatus(wctUsdPair, sellOrder, "Cancelled")
    }

    "orderbook was really deleted" in {
      val orderBook = matcherNode.orderBook(wctUsdPair)
      orderBook.bids shouldBe empty
      orderBook.asks shouldBe empty
    }

    "reserved balances should be released for the pair" in {
      val (aliceReservedBalances, bobReservedBalances) = (reservedBalancesOf(aliceAcc), reservedBalancesOf(bobAcc))
      aliceReservedBalances.usd shouldBe 0
      aliceReservedBalances.mir shouldBe (aliceRBForBothPairs.mir - aliceRBForOnePair.mir)
      bobReservedBalances.wct shouldBe (bobRBForBothPairs.wct - bobRBForOnePair.wct)
      bobReservedBalances.mir shouldBe (bobRBForBothPairs.mir - bobRBForOnePair.mir)
    }

    "it should not affect other pairs and their orders" in {
      matcherNode.orderStatus(buyOrderForAnotherPair, wctMirPair).status shouldBe "Accepted"
      matcherNode.orderStatus(sellOrderForAnotherPair, wctMirPair).status shouldBe "Accepted"

      val orderBook = matcherNode.orderBook(wctMirPair)
      orderBook.bids shouldNot be(empty)
      orderBook.asks shouldNot be(empty)
    }

    "it should not affect market status" in {
      matcherNode.marketStatus(wctUsdPair) shouldEqual marketStatusBeforeDeletion
    }
  }

}
