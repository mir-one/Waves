package one.mir.lang

import cats.kernel.Monoid
import one.mir.lang.ScriptVersion.Versions.V1
import one.mir.lang.v1.compiler.CompilerV1
import one.mir.lang.v1.compiler.Terms.EXPR
import one.mir.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import one.mir.lang.v1.evaluator.ctx.impl.mir.MirContext

object JavaAdapter {
  private val version = V1

  lazy val compiler =
    new CompilerV1(
      Monoid.combineAll(
        Seq(
          CryptoContext.compilerContext(one.mir.lang.Global),
          MirContext.build(version, null, false).compilerContext,
          PureContext.build(version).compilerContext
        )))

  def compile(input: String): EXPR = {
    compiler
      .compile(input, List())
      .fold(
        error => throw new IllegalArgumentException(error),
        expr => expr
      )
  }
}
