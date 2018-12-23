package one.mir.state.diffs

import one.mir.features.BlockchainFeatures
import one.mir.state.{Blockchain, Diff, LeaseBalance, Portfolio}
import one.mir.transaction.ValidationError.GenericError
import one.mir.transaction.{CreateAliasTransaction, ValidationError}
import one.mir.features.FeatureProvider._

import scala.util.Right

object CreateAliasTransactionDiff {
  def apply(blockchain: Blockchain, height: Int)(tx: CreateAliasTransaction): Either[ValidationError, Diff] =
    if (blockchain.isFeatureActivated(BlockchainFeatures.DataTransaction, height) && !blockchain.canCreateAlias(tx.alias))
      Left(GenericError("Alias already claimed"))
    else
      Right(
        Diff(height = height,
             tx = tx,
             portfolios = Map(tx.sender.toAddress -> Portfolio(-tx.fee, LeaseBalance.empty, Map.empty)),
             aliases = Map(tx.alias               -> tx.sender.toAddress)))
}
