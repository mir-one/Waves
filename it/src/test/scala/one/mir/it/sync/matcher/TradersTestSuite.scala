package one.mir.it.sync.matcher

import com.typesafe.config.Config
import one.mir.it.api.SyncHttpApi._
import one.mir.it.api.SyncMatcherHttpApi._
import one.mir.it.matcher.MatcherSuiteBase
import one.mir.it.sync._
import one.mir.it.sync.matcher.config.MatcherPriceAssetConfig._
import one.mir.it.util._
import one.mir.matcher.market.MatcherActor
import one.mir.matcher.model.MatcherModel.Price
import one.mir.state.ByteStr
import one.mir.transaction.assets.exchange.{AssetPair, Order, OrderType}

import scala.util.Random

class TradersTestSuite extends MatcherSuiteBase {
  private val exTxFee                             = 300000
  private def orderVersion                        = (Random.nextInt(2) + 1).toByte
  override protected def nodeConfigs: Seq[Config] = Configs

  "Verifications of tricky ordering cases" ignore {
    // Alice issues new asset
    val aliceAsset =
      aliceNode.issue(aliceAcc.address, "AliceCoin", "AliceCoin for matcher's tests", someAssetAmount, 0, reissuable = false, smartIssueFee, 2).id
    matcherNode.waitForTransaction(aliceAsset)

    // val aliceMirPair = AssetPair(ByteStr.decodeBase58(aliceAsset).toOption, None)

    // Wait for balance on Alice's account
    matcherNode.assertAssetBalance(aliceAcc.address, aliceAsset, someAssetAmount)
    matcherNode.assertAssetBalance(matcherAcc.address, aliceAsset, 0)
    matcherNode.assertAssetBalance(bobAcc.address, aliceAsset, 0)

    // Bob issues a new asset
    val bobAssetQuantity = 10000
    val bobNewAsset      = bobNode.issue(bobAcc.address, "BobCoin3", "Bob's asset", bobAssetQuantity, 0, false, smartIssueFee, 2).id
    matcherNode.waitForTransaction(bobNewAsset)

    val bobAssetId   = ByteStr.decodeBase58(bobNewAsset).get
    val aliceAssetId = ByteStr.decodeBase58(aliceAsset).get

    val bobMirPair = AssetPair(
      amountAsset = Some(bobAssetId),
      priceAsset = None
    )

    val twoAssetsPair =
      if (MatcherActor.compare(Some(bobAssetId.arr), Some(aliceAssetId.arr)) < 0)
        AssetPair(
          amountAsset = Some(aliceAssetId),
          priceAsset = Some(bobAssetId)
        )
      else
        AssetPair(
          amountAsset = Some(bobAssetId),
          priceAsset = Some(aliceAssetId)
        )

    matcherNode.assertAssetBalance(bobAcc.address, bobNewAsset, bobAssetQuantity)

    "owner moves assets/mir to another account and order become an invalid" ignore {
      // todo: reactivate after balance watcher is reimplemented
      // Could not work sometimes because of NODE-546
      "order with assets" - {
        "moved assets, insufficient assets" in {
          val oldestOrderId = bobPlacesAssetOrder(8000, twoAssetsPair, bobNewAsset)
          val newestOrderId = bobPlacesAssetOrder(1000, twoAssetsPair, bobNewAsset)

          val transferId = bobNode.transfer(bobAcc.address, aliceAcc.address, 3050, exTxFee, Some(bobNewAsset), None, 2).id
          matcherNode.waitForTransaction(transferId)

          withClue(s"The oldest order '$oldestOrderId' was cancelled") {
            matcherNode.waitOrderStatus(bobMirPair, oldestOrderId, "Cancelled")
          }
          withClue(s"The newest order '$newestOrderId' is still active") {
            matcherNode.waitOrderStatus(bobMirPair, newestOrderId, "Accepted")
          }

          // Cleanup
          matcherNode.cancelOrder(bobAcc, twoAssetsPair, newestOrderId)
          matcherNode.waitOrderStatus(twoAssetsPair, newestOrderId, "Cancelled")

          val transferBackId = aliceNode.transfer(aliceAcc.address, bobAcc.address, 3050, exTxFee, Some(bobNewAsset), None, 2).id
          matcherNode.waitForTransaction(transferBackId)
        }

        "leased mir, insufficient fee" in {
          val bobBalance    = matcherNode.accountBalances(bobAcc.address)._1
          val oldestOrderId = bobPlacesAssetOrder(1000, twoAssetsPair, bobNewAsset)
          val newestOrderId = bobPlacesAssetOrder(1000, twoAssetsPair, bobNewAsset)

          // TransactionFee for leasing, matcherFee for one order
          val leaseAmount = bobBalance - exTxFee - matcherFee
          val leaseId     = bobNode.lease(bobAcc.address, aliceAcc.address, leaseAmount, exTxFee, 2).id
          matcherNode.waitForTransaction(leaseId)

          withClue(s"The oldest order '$oldestOrderId' was cancelled") {
            matcherNode.waitOrderStatus(bobMirPair, oldestOrderId, "Cancelled")
          }
          withClue(s"The newest order '$newestOrderId' is still active") {
            matcherNode.waitOrderStatus(bobMirPair, newestOrderId, "Accepted")
          }

          // Cleanup
          matcherNode.cancelOrder(bobAcc, twoAssetsPair, newestOrderId)
          matcherNode.waitOrderStatus(twoAssetsPair, newestOrderId, "Cancelled")

          val cancelLeaseId = bobNode.cancelLease(bobAcc.address, leaseId, exTxFee, 2).id
          matcherNode.waitForTransaction(cancelLeaseId)
        }

        "moved mir, insufficient fee" in {
          val bobBalance    = matcherNode.accountBalances(bobAcc.address)._1
          val oldestOrderId = bobPlacesAssetOrder(1000, twoAssetsPair, bobNewAsset)
          val newestOrderId = bobPlacesAssetOrder(1000, twoAssetsPair, bobNewAsset)

          // TransactionFee for leasing, matcherFee for one order
          val transferAmount = bobBalance - exTxFee - matcherFee
          val transferId     = bobNode.transfer(bobAcc.address, aliceAcc.address, transferAmount, exTxFee, None, None, 2).id
          matcherNode.waitForTransaction(transferId)

          withClue(s"The oldest order '$oldestOrderId' was cancelled") {
            matcherNode.waitOrderStatus(bobMirPair, oldestOrderId, "Cancelled")
          }
          withClue(s"The newest order '$newestOrderId' is still active") {
            matcherNode.waitOrderStatus(bobMirPair, newestOrderId, "Accepted")
          }

          // Cleanup
          matcherNode.cancelOrder(bobAcc, twoAssetsPair, newestOrderId)
          matcherNode.waitOrderStatus(twoAssetsPair, newestOrderId, "Cancelled")

          val transferBackId = aliceNode.transfer(aliceAcc.address, bobAcc.address, transferAmount, exTxFee, None, None, 2).id
          matcherNode.waitForTransaction(transferBackId)
        }
      }

      "order with mir" - {
        "leased mir, insufficient fee for one ExchangeTransaction" in {
          // Amount of mir in order is smaller than fee
          val bobBalance = matcherNode.accountBalances(bobAcc.address)._1

          val oldestOrderId = bobPlacesWaveOrder(bobMirPair, 1, 10.mir * Order.PriceConstant)
          val newestOrderId = bobPlacesWaveOrder(bobMirPair, 1, 10.mir * Order.PriceConstant)

          //      waitForOrderStatus(matcherNode, bobAssetIdRaw, id, "Accepted")
          val leaseAmount = bobBalance - exTxFee - 10.mir - matcherFee
          val leaseId     = bobNode.lease(bobAcc.address, aliceAcc.address, leaseAmount, exTxFee, 2).id
          matcherNode.waitForTransaction(leaseId)

          withClue(s"The newest order '$oldestOrderId' is Cancelled") {
            matcherNode.waitOrderStatus(bobMirPair, oldestOrderId, "Cancelled")
          }
          withClue(s"The newest order '$newestOrderId' is still active") {
            matcherNode.waitOrderStatus(bobMirPair, newestOrderId, "Accepted")
          }

          // Cleanup
          matcherNode.cancelOrder(bobAcc, bobMirPair, newestOrderId)
          matcherNode.waitOrderStatus(twoAssetsPair, newestOrderId, "Cancelled")

          val cancelLeaseId = bobNode.cancelLease(bobNode.address, leaseId, exTxFee, 2).id
          matcherNode.waitForTransaction(cancelLeaseId)
        }

        "leased mir, insufficient mir" in {
          val bobBalance = matcherNode.accountBalances(bobAcc.address)._1
          val price      = 1.mir
          val order2     = bobPlacesWaveOrder(bobMirPair, 1, price * Order.PriceConstant)

          val leaseAmount = bobBalance - exTxFee - price / 2
          val leaseId     = bobNode.lease(bobAcc.address, aliceAcc.address, leaseAmount, exTxFee, 2).id
          matcherNode.waitForTransaction(leaseId)

          withClue(s"The order '$order2' was cancelled") {
            matcherNode.waitOrderStatus(bobMirPair, order2, "Cancelled")
          }

          // Cleanup
          val cancelLeaseId = bobNode.cancelLease(bobAcc.address, leaseId, exTxFee, 2).id
          matcherNode.waitForTransaction(cancelLeaseId)
        }

        "moved mir, insufficient fee" in {
          // Amount of mir in order is smaller than fee
          val bobBalance = matcherNode.accountBalances(bobAcc.address)._1
          val price      = exTxFee / 2
          val order3     = bobPlacesWaveOrder(bobMirPair, 1, price * Order.PriceConstant)

          val transferAmount = bobBalance - exTxFee - price
          val txId           = bobNode.transfer(bobAcc.address, aliceAcc.address, transferAmount, exTxFee, None, None, 2).id
          matcherNode.waitForTransaction(txId)

          withClue(s"The order '$order3' was cancelled") {
            matcherNode.waitOrderStatus(bobMirPair, order3, "Cancelled")
          }

          // Cleanup
          val transferBackId = aliceNode.transfer(aliceAcc.address, bobAcc.address, transferAmount, exTxFee, None, None, 2).id
          matcherNode.waitForTransaction(transferBackId)
        }

      }
    }
  }

  def bobPlacesWaveOrder(assetPair: AssetPair, amount: Long, price: Price): String = {
    val bobOrder = matcherNode.prepareOrder(bobAcc, assetPair, OrderType.BUY, amount, price)
    val order    = matcherNode.placeOrder(bobOrder).message.id
    matcherNode.waitOrderStatus(assetPair, order, "Accepted")
    order
  }

  def bobPlacesAssetOrder(bobCoinAmount: Int, twoAssetsPair: AssetPair, assetId: String): String = {
    val decodedAsset = ByteStr.decodeBase58(assetId).get
    val bobOrder = if (twoAssetsPair.amountAsset.contains(decodedAsset)) {
      matcherNode.prepareOrder(bobAcc, twoAssetsPair, OrderType.SELL, bobCoinAmount, 1 * Order.PriceConstant, orderVersion)
    } else {
      matcherNode.prepareOrder(bobAcc, twoAssetsPair, OrderType.BUY, 1, bobCoinAmount * Order.PriceConstant, orderVersion)
    }
    val order = matcherNode.placeOrder(bobOrder)
    matcherNode.waitOrderStatus(twoAssetsPair, order.message.id, "Accepted")
    order.message.id
  }

}
