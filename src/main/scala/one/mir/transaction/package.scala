package one.mir

import one.mir.utils.base58Length
import one.mir.block.{Block, MicroBlock}

package object transaction {

  type AssetId = one.mir.state.ByteStr
  val AssetIdLength: Int       = one.mir.crypto.DigestSize
  val AssetIdStringLength: Int = base58Length(AssetIdLength)
  type DiscardedTransactions = Seq[Transaction]
  type DiscardedBlocks       = Seq[Block]
  type DiscardedMicroBlocks  = Seq[MicroBlock]
  type AuthorizedTransaction = Authorized with Transaction
}
