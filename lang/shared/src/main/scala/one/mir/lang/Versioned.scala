package one.mir.lang

trait Versioned {
  type Ver <: ScriptVersion
  val version: Ver
}
