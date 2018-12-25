package one.mir.lang.v1.traits

import one.mir.lang.v1.compiler.Types._

sealed abstract case class DataType(innerType: REAL)
object DataType {
  object Boolean   extends DataType(BOOLEAN)
  object Long      extends DataType(LONG)
  object ByteArray extends DataType(BYTEVECTOR)
  object String    extends DataType(STRING)
}