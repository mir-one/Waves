package one.mir.matcher.api
import one.mir.account.Address
import one.mir.transaction.assets.exchange.AssetPair

case class BatchCancel(address: Address, assetPair: Option[AssetPair], timestamp: Long)
