package one.mir.transaction.assets

import java.nio.charset.StandardCharsets

import cats.implicits._
import com.google.common.primitives.{Bytes, Longs}
import one._
import one.mir.account.PublicKeyAccount
import one.mir.crypto._
import one.mir.serialization.Deser
import one.mir.transaction.smart.script.Script
import one.mir.transaction.validation._
import one.mir.transaction.{AssetId, ProvenTransaction, ValidationError, VersionedTransaction}
import monix.eval.Coeval
import play.api.libs.json.Json

trait IssueTransaction extends ProvenTransaction with VersionedTransaction {
  def name: Array[Byte]
  def description: Array[Byte]
  def quantity: Long
  def decimals: Byte
  def reissuable: Boolean
  def fee: Long
  def script: Option[Script]

  final lazy val assetId                               = id
  override final val assetFee: (Option[AssetId], Long) = (None, fee)

  val issueJson = Coeval.evalOnce(
    jsonBase() ++ Json.obj(
      "version"     -> version,
      "assetId"     -> assetId().base58,
      "name"        -> new String(name, StandardCharsets.UTF_8),
      "quantity"    -> quantity,
      "reissuable"  -> reissuable,
      "decimals"    -> decimals,
      "description" -> new String(description, StandardCharsets.UTF_8),
    ))

  final protected val bytesBase: Coeval[Array[Byte]] = Coeval.evalOnce(
    Bytes.concat(
      sender.publicKey,
      Deser.serializeArray(name),
      Deser.serializeArray(description),
      Longs.toByteArray(quantity),
      Array(decimals),
      Deser.serializeBoolean(reissuable),
      Longs.toByteArray(fee),
      Longs.toByteArray(timestamp)
    ))
}
object IssueTransaction {

  val typeId: Byte = 3

  val MaxDescriptionLength = 1000
  val MaxAssetNameLength   = 16
  val MinAssetNameLength   = 4
  val MaxDecimals          = 8

  def validateIssueParams(name: Array[Byte],
                          description: Array[Byte],
                          quantity: Long,
                          decimals: Byte,
                          reissuable: Boolean,
                          fee: Long,
                          timestamp: Long): Either[ValidationError, Unit] = {
    if (timestamp <= 1547252700000L || (
        (name.length > 1 || fee >= 100000000000000L) && /* 1'000'000 MIR */
        (name.length > 2 || fee >= 10000000000000L) && /* 100'000 MIR */
        (name.length > 3 || fee >= 1000000000000L) && /* 10'000 MIR */
        (name.length > 4 || fee >= 100000000000L) && /* 1000 MIR */
        (name.length > 5 || fee >= 10000000000L) && /* 100 MIR */
        (name.length > 6 || fee >= 1000000000L) && /* 10 MIR */
        (name.length > 7 || fee >= 100000000L) && /* 1 MIR */
        (name.length > 8 || fee >= 10000000L) /* 0.1 MIR */ )) {
    (
      validateAmount(quantity, "assets"),
      validateName(name),
      validateDescription(description),
      validateDecimals(decimals),
      validateFee(fee)
    ).mapN { case _ => () }
      .leftMap(_.head)
      .toEither
    } else {
      (
        validateAmount(0, "assets"),
        validateName(name),
        validateFee(fee)
      ).mapN { case _ => () }
        .leftMap(_.head)
        .toEither
    }
  }

  def validateIssueParams2(name: Array[Byte], description: Array[Byte], quantity: Long, decimals: Byte, reissuable: Boolean, fee: Long, timestamp: Long): Boolean = {
    if (timestamp <= 1547252700000L || (
        (name.length > 1 || fee >= 100000000000000L) && /* 1'000'000 MIR */
        (name.length > 2 || fee >= 10000000000000L) && /* 100'000 MIR */
        (name.length > 3 || fee >= 1000000000000L) && /* 10'000 MIR */
        (name.length > 4 || fee >= 100000000000L) && /* 1000 MIR */
        (name.length > 5 || fee >= 10000000000L) && /* 100 MIR */
        (name.length > 6 || fee >= 1000000000L) && /* 10 MIR */
        (name.length > 7 || fee >= 100000000L) && /* 1 MIR */
        (name.length > 8 || fee >= 10000000L) /* 0.1 MIR */ )) {
      mir.transaction.validation.validateAmount2(quantity, "assets") && mir.transaction.validation.validateName2(name) &&
        mir.transaction.validation.validateFee2(fee) && mir.transaction.validation.validateDescription2(description) &&
        mir.transaction.validation.validateDecimals2(decimals)
    } else {
      mir.transaction.ValidationError.GenericError(s"ERROR ValidateIssueParams")
      false
    }
  }

  def parseBase(bytes: Array[Byte], start: Int) = {
    val sender                        = PublicKeyAccount(bytes.slice(start, start + KeyLength))
    val (assetName, descriptionStart) = Deser.parseArraySize(bytes, start + KeyLength)
    val (description, quantityStart)  = Deser.parseArraySize(bytes, descriptionStart)
    val quantity                      = Longs.fromByteArray(bytes.slice(quantityStart, quantityStart + 8))
    val decimals                      = bytes.slice(quantityStart + 8, quantityStart + 9).head
    val reissuable                    = bytes.slice(quantityStart + 9, quantityStart + 10).head == (1: Byte)
    val fee                           = Longs.fromByteArray(bytes.slice(quantityStart + 10, quantityStart + 18))
    val timestamp                     = Longs.fromByteArray(bytes.slice(quantityStart + 18, quantityStart + 26))
    (sender, assetName, description, quantity, decimals, reissuable, fee, timestamp, quantityStart + 26)
  }
}
