package one.mir.matcher.smart

import cats.implicits._
import one.mir.account.AddressScheme
import one.mir.lang.ExprEvaluator.Log
import one.mir.lang.v1.compiler.Terms.EVALUATED
import one.mir.lang.v1.evaluator.EvaluatorV1
import one.mir.transaction.assets.exchange.Order
import one.mir.transaction.smart.script.Script
import monix.eval.Coeval

object MatcherScriptRunner {

  def apply[A <: EVALUATED](script: Script, order: Order, isTokenScript: Boolean): (Log, Either[String, A]) = script match {
    case Script.Expr(expr) =>
      val ctx = MatcherContext.build(script.version, AddressScheme.current.chainId, Coeval.evalOnce(order), !isTokenScript)
      EvaluatorV1.applywithLogging[A](ctx, expr)
    case _ => (List.empty, "Unsupported script version".asLeft[A])
  }
}
