package one.mir.transaction

trait VersionedTransaction {
  def version: Byte
}
