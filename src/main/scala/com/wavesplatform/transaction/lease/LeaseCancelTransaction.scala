package one.mir.transaction.lease

import com.google.common.primitives.{Bytes, Longs}
import one.mir.crypto
import one.mir.state.ByteStr
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import one.mir.account.PublicKeyAccount
import one.mir.transaction.{AssetId, ProvenTransaction, ValidationError, VersionedTransaction}
import one.mir.crypto.KeyLength

trait LeaseCancelTransaction extends ProvenTransaction with VersionedTransaction {
  def chainByte: Option[Byte]
  def leaseId: ByteStr
  def fee: Long
  override val assetFee: (Option[AssetId], Long) = (None, fee)

  override val json: Coeval[JsObject] = Coeval.evalOnce(
    jsonBase() ++ Json.obj(
      "chainId"   -> chainByte,
      "version"   -> version,
      "fee"       -> fee,
      "timestamp" -> timestamp,
      "leaseId"   -> leaseId.base58
    ))
  protected val bytesBase = Coeval.evalOnce(Bytes.concat(sender.publicKey, Longs.toByteArray(fee), Longs.toByteArray(timestamp), leaseId.arr))

}

object LeaseCancelTransaction {

  val typeId: Byte = 9

  def validateLeaseCancelParams(leaseId: ByteStr, fee: Long) =
    if (leaseId.arr.length != crypto.DigestSize) {
      Left(ValidationError.GenericError("Lease transaction id is invalid"))
    } else if (fee <= 0) {
      Left(ValidationError.InsufficientFee())
    } else Right(())

  def parseBase(bytes: Array[Byte], start: Int) = {
    val sender    = PublicKeyAccount(bytes.slice(start, start + KeyLength))
    val fee       = Longs.fromByteArray(bytes.slice(start + KeyLength, start + KeyLength + 8))
    val timestamp = Longs.fromByteArray(bytes.slice(start + KeyLength + 8, start + KeyLength + 16))
    val end       = start + KeyLength + 16 + crypto.DigestSize
    val leaseId   = ByteStr(bytes.slice(start + KeyLength + 16, end))
    (sender, fee, timestamp, leaseId, end)
  }
}
