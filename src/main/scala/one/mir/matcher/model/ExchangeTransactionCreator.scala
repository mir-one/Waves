package one.mir.matcher.model

import one.mir.account.{Address, PrivateKeyAccount}
import one.mir.features.BlockchainFeatures
import one.mir.features.FeatureProvider.FeatureProviderExt
import one.mir.matcher.MatcherSettings
import one.mir.matcher.model.Events.OrderExecuted
import one.mir.matcher.model.ExchangeTransactionCreator._
import one.mir.state.Blockchain
import one.mir.state.diffs.CommonValidation
import one.mir.transaction.assets.exchange._
import one.mir.transaction.{AssetId, ValidationError}
import one.mir.utils.Time

class ExchangeTransactionCreator(blockchain: Blockchain, matcherPrivateKey: PrivateKeyAccount, settings: MatcherSettings, time: Time) {
  private def calculateMatcherFee(buy: Order, sell: Order, amount: Long): (Long, Long) = {
    def calcFee(o: Order, amount: Long): Long = {
      val p = BigInt(amount) * o.matcherFee / o.amount
      p.toLong
    }

    (calcFee(buy, amount), calcFee(sell, amount))
  }

  def createTransaction(event: OrderExecuted): Either[ValidationError, ExchangeTransaction] = {
    import event.{counter, submitted}
    val price             = counter.price
    val (buy, sell)       = Order.splitByType(submitted.order, counter.order)
    val (buyFee, sellFee) = calculateMatcherFee(buy, sell, event.executedAmount)

    val txFee = getMinFee(blockchain, settings.orderMatchTxFee, matcherPrivateKey, Some(buy.sender), Some(sell.sender), counter.order.assetPair)
    if (blockchain.isFeatureActivated(BlockchainFeatures.SmartAccountTrading, blockchain.height))
      ExchangeTransactionV2.create(matcherPrivateKey, buy, sell, event.executedAmount, price, buyFee, sellFee, txFee, time.getTimestamp())
    else
      for {
        buyV1  <- toV1(buy)
        sellV1 <- toV1(sell)
        tx     <- ExchangeTransactionV1.create(matcherPrivateKey, buyV1, sellV1, event.executedAmount, price, buyFee, sellFee, txFee, time.getTimestamp())
      } yield tx
  }

  private def toV1(order: Order): Either[ValidationError, OrderV1] = order match {
    case x: OrderV1 => Right(x)
    case _          => Left(ValidationError.ActivationError("SmartAccountTrading has not been activated yet"))
  }
}

object ExchangeTransactionCreator {

  /**
    * @note see Verifier.verifyExchange
    */
  def getMinFee(blockchain: Blockchain,
                orderMatchTxFee: Long,
                matcherAddress: Address,
                order1Sender: Option[Address],
                order2Sender: Option[Address],
                assetPair: AssetPair): Long = {
    def assetFee(assetId: AssetId): Long   = if (blockchain.hasAssetScript(assetId)) CommonValidation.ScriptExtraFee else 0L
    def accountFee(address: Address): Long = if (blockchain.hasScript(address)) CommonValidation.ScriptExtraFee else 0L

    orderMatchTxFee +
      accountFee(matcherAddress) +
      assetPair.amountAsset.fold(0L)(assetFee) +
      assetPair.priceAsset.fold(0L)(assetFee)
  }

}
