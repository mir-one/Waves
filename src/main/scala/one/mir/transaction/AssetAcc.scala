package one.mir.transaction

import one.mir.account.Address

case class AssetAcc(account: Address, assetId: Option[AssetId])
