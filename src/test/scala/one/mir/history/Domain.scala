package one.mir.history

import one.mir.state._
import one.mir.account.Address
import one.mir.block.Block
import one.mir.transaction.BlockchainUpdater

case class Domain(blockchainUpdater: BlockchainUpdater with NG) {
  def effBalance(a: Address): Long          = blockchainUpdater.effectiveBalance(a, blockchainUpdater.height, 1000)
  def appendBlock(b: Block)                 = blockchainUpdater.processBlock(b).explicitGet()
  def removeAfter(blockId: ByteStr)         = blockchainUpdater.removeAfter(blockId).explicitGet()
  def lastBlockId                           = blockchainUpdater.lastBlockId.get
  def portfolio(address: Address)           = blockchainUpdater.portfolio(address)
  def addressTransactions(address: Address) = blockchainUpdater.addressTransactions(address, Set.empty, 128, None)
  def carryFee                              = blockchainUpdater.carryFee
}
