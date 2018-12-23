package one.mir.state

import one.mir.block.Block
import one.mir.lagonaki.mocks.TestBlock
import one.mir.crypto._

trait HistoryTest {
  val genesisBlock: Block = TestBlock.withReference(ByteStr(Array.fill(SignatureLength)(0: Byte)))

  def getNextTestBlock(blockchain: Blockchain): Block =
    TestBlock.withReference(blockchain.lastBlock.get.uniqueId)

  def getNextTestBlockWithVotes(blockchain: Blockchain, votes: Set[Short]): Block =
    TestBlock.withReferenceAndFeatures(blockchain.lastBlock.get.uniqueId, votes)
}
