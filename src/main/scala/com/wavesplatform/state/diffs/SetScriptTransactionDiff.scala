package one.mir.state.diffs

import one.mir.features.BlockchainFeatures
import one.mir.features.FeatureProvider._
import one.mir.state.{Blockchain, Diff, LeaseBalance, Portfolio}
import one.mir.transaction.ValidationError
import one.mir.transaction.smart.SetScriptTransaction

import one.mir.transaction.ValidationError
import one.mir.transaction.ValidationError.GenericError
import one.mir.lang.v1.DenyDuplicateVarNames
import one.mir.utils.varNames

import scala.util.Right

object SetScriptTransactionDiff {
  def apply(blockchain: Blockchain, height: Int)(tx: SetScriptTransaction): Either[ValidationError, Diff] = {
    val scriptOpt = tx.script
    for {
      _ <- scriptOpt.fold(Right(()): Either[ValidationError, Unit]) { script =>
        if (blockchain.isFeatureActivated(BlockchainFeatures.SmartAccountTrading, height)) {
          Right(())
        } else {
          val version = script.version
          DenyDuplicateVarNames(version, varNames(version), script.expr).left.map(GenericError.apply)
        }
      }
    } yield {
      Diff(
        height = height,
        tx = tx,
        portfolios = Map(tx.sender.toAddress -> Portfolio(-tx.fee, LeaseBalance.empty, Map.empty)),
        scripts = Map(tx.sender.toAddress    -> scriptOpt)
      )
    }
  }
}
