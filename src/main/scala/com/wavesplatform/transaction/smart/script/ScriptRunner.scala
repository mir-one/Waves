package one.mir.transaction.smart.script

import cats.implicits._
import one.mir.account.AddressScheme
import one.mir.lang.v1.compiler.Terms.EVALUATED
import one.mir.lang.v1.evaluator.EvaluatorV1
import one.mir.lang.{ExecutionError, ExprEvaluator}
import one.mir.state._
import one.mir.transaction.Transaction
import one.mir.transaction.assets.exchange.Order
import one.mir.transaction.smart.BlockchainContext
import monix.eval.Coeval
import shapeless._

object ScriptRunner {

  def apply[A <: EVALUATED](height: Int,
                            in: Transaction :+: Order :+: CNil,
                            blockchain: Blockchain,
                            script: Script,
                            isTokenScript: Boolean): (ExprEvaluator.Log, Either[ExecutionError, A]) = {
    script match {
      case Script.Expr(expr) =>
        val ctx = BlockchainContext.build(
          script.version,
          AddressScheme.current.chainId,
          Coeval.evalOnce(in),
          Coeval.evalOnce(height),
          blockchain,
          isTokenScript
        )
        EvaluatorV1.applywithLogging[A](ctx, expr)

      case _ => (List.empty, "Unsupported script version".asLeft[A])
    }
  }
}
