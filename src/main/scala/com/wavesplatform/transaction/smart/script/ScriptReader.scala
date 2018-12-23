package one.mir.transaction.smart.script

import one.mir.crypto
import one.mir.lang.ScriptVersion
import one.mir.lang.v1.Serde
import one.mir.transaction.ValidationError.ScriptParseError
import one.mir.transaction.smart.script.v1.ScriptV1

object ScriptReader {

  val checksumLength = 4

  def fromBytes(bytes: Array[Byte]): Either[ScriptParseError, Script] = {
    val checkSum         = bytes.takeRight(checksumLength)
    val computedCheckSum = crypto.secureHash(bytes.dropRight(checksumLength)).take(checksumLength)
    val version          = bytes.head
    val scriptBytes      = bytes.drop(1).dropRight(checksumLength)

    for {
      _ <- Either.cond(checkSum.sameElements(computedCheckSum), (), ScriptParseError("Invalid checksum"))
      sv <- ScriptVersion
        .fromInt(version)
        .fold[Either[ScriptParseError, ScriptVersion]](Left(ScriptParseError(s"Invalid version: $version")))(v => Right(v))
      script <- ScriptV1
        .validateBytes(scriptBytes)
        .flatMap { _ =>
          Serde.deserialize(scriptBytes).flatMap(ScriptV1(sv, _, checkSize = false))
        }
        .left
        .map(ScriptParseError)
    } yield script
  }

}
