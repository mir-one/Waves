package one.mir

import one.mir.state.Blockchain
import one.mir.transaction.Transaction

package object mining {
  private[mining] def createConstConstraint(maxSize: Long, transactionSize: => Long) = OneDimensionalMiningConstraint(
    maxSize,
    new one.mir.mining.TxEstimators.Fn {
      override def apply(b: Blockchain, t: Transaction) = transactionSize
      override val minEstimate                          = transactionSize
    }
  )
}
