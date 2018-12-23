package one.mir.consensus.nxt

import one.mir.state.ByteStr

case class NxtLikeConsensusBlockData(baseTarget: Long, generationSignature: ByteStr)
