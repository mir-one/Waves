package one.mir.lang

import one.mir.lang.directives.Directive

trait ExprCompiler extends Versioned {
  def compile(input: String, directives: List[Directive]): Either[String, version.ExprT]
}
