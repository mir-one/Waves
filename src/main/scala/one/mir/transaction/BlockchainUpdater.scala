package one.mir.transaction

import one.mir.state.ByteStr
import monix.reactive.Observable
import one.mir.block.Block.BlockId
import one.mir.block.{Block, MicroBlock}

trait BlockchainUpdater {
  def processBlock(block: Block, verify: Boolean = true): Either[ValidationError, Option[DiscardedTransactions]]

  def processMicroBlock(microBlock: MicroBlock, verify: Boolean = true): Either[ValidationError, Unit]

  def removeAfter(blockId: ByteStr): Either[ValidationError, DiscardedBlocks]

  def lastBlockInfo: Observable[LastBlockInfo]

  def isLastBlockId(id: ByteStr): Boolean

  def shutdown(): Unit
}

case class LastBlockInfo(id: BlockId, height: Int, score: BigInt, ready: Boolean)
