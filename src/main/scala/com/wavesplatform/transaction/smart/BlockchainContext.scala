package one.mir.transaction.smart

import cats.kernel.Monoid
import one.mir.lang.{Global, ScriptVersion}
import one.mir.lang.v1.evaluator.ctx.EvaluationContext
import one.mir.lang.v1.evaluator.ctx.impl.waves.WavesContext
import one.mir.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import one.mir.state._
import one.mir.transaction._
import one.mir.transaction.assets.exchange.Order
import monix.eval.Coeval
import shapeless._

object BlockchainContext {

  type In = Transaction :+: Order :+: CNil
  def build(version: ScriptVersion,
            nByte: Byte,
            in: Coeval[In],
            h: Coeval[Int],
            blockchain: Blockchain,
            isTokenContext: Boolean): EvaluationContext = {
    Monoid
      .combineAll(
        Seq(
          PureContext.build(version),
          CryptoContext.build(Global),
          WavesContext.build(version, new WavesEnvironment(nByte, in, h, blockchain), isTokenContext)
        ))
      .evaluationContext
  }
}
