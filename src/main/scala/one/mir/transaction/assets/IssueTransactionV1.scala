package one.mir.transaction.assets

import com.google.common.primitives.Bytes
import com.google.common.primitives.Longs
import one.mir.account.{PrivateKeyAccount, PublicKeyAccount}
import one.mir.crypto
import one.mir.crypto.SignatureLength
import one.mir.serialization.Deser
import one.mir.state.ByteStr
import one.mir.transaction._
import one.mir.transaction.smart.script.Script
import monix.eval.Coeval
import play.api.libs.json.JsObject

import scala.util.{Failure, Success, Try}

case class IssueTransactionV1 private (sender: PublicKeyAccount,
                                       name: Array[Byte],
                                       description: Array[Byte],
                                       quantity: Long,
                                       decimals: Byte,
                                       reissuable: Boolean,
                                       fee: Long,
                                       timestamp: Long,
                                       signature: ByteStr)
    extends IssueTransaction
    with SignedTransaction
    with FastHashId {
  override val version: Byte                    = 1
  override val script: Option[Script]           = None
  override val builder: IssueTransactionV1.type = IssueTransactionV1
  override val bodyBytes: Coeval[Array[Byte]]   = Coeval.evalOnce(Bytes.concat(Array(builder.typeId), bytesBase()))
  override val bytes: Coeval[Array[Byte]]       = Coeval.evalOnce(Bytes.concat(Array(builder.typeId), signature.arr, bodyBytes()))
  override val json: Coeval[JsObject]           = issueJson
}

object IssueTransactionV1 extends TransactionParserFor[IssueTransactionV1] with TransactionParser.HardcodedVersion1 {

  override val typeId: Byte = IssueTransaction.typeId

  override protected def parseTail(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      val signature = ByteStr(bytes.slice(0, SignatureLength))
      val txId      = bytes(SignatureLength)
      require(txId == typeId, s"Signed tx id is not match")
      val (sender, assetName, description, quantity, decimals, reissuable, fee, timestamp, _) = IssueTransaction.parseBase(bytes, SignatureLength + 1)
      if (IssueTransaction.validateIssueParams2(assetName, description, quantity, decimals, reissuable, fee, timestamp))
        Success(new IssueTransactionV1(sender, assetName, description, quantity, decimals, reissuable, fee, timestamp, signature))
      else
        Failure(new Exception(one.mir.transaction.ValidationError.GenericError(s"ERROR ValidateIssueParams").toString))
    }.flatten

  def create(sender: PublicKeyAccount,
             name: Array[Byte],
             description: Array[Byte],
             quantity: Long,
             decimals: Byte,
             reissuable: Boolean,
             fee: Long,
             timestamp: Long,
             signature: ByteStr): Either[ValidationError, TransactionT] =
    Either.cond(
      IssueTransaction.validateIssueParams2(name, description, quantity, decimals, reissuable, fee, timestamp),
      new IssueTransactionV1(sender, name, description, quantity, decimals, reissuable, fee, timestamp, signature),
      one.mir.transaction.ValidationError.GenericError(s"ERROR ValidateIssueParams")
    )

  def signed(sender: PublicKeyAccount,
             name: Array[Byte],
             description: Array[Byte],
             quantity: Long,
             decimals: Byte,
             reissuable: Boolean,
             fee: Long,
             timestamp: Long,
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] =
  {
    Either.cond(
      IssueTransaction.validateIssueParams2(name, description, quantity, decimals, reissuable, fee, timestamp),
      {
        val bytesBase1: Array[Byte] = Bytes.concat(
          sender.publicKey, Deser.serializeArray(name), Deser.serializeArray(description), Longs.toByteArray(quantity),
          Array(decimals), Deser.serializeBoolean(reissuable), Longs.toByteArray(fee), Longs.toByteArray(timestamp)
        )
        val bodyBytes1: Array[Byte] = Bytes.concat(Array(IssueTransactionV1.typeId), bytesBase1)
        new IssueTransactionV1(sender, name, description, quantity, decimals, reissuable, fee, timestamp, ByteStr(crypto.sign(signer, bodyBytes1)))
      },
      one.mir.transaction.ValidationError.GenericError(s"ERROR ValidateIssueParams")
    )
    }

  def selfSigned(sender: PrivateKeyAccount,
                 name: Array[Byte],
                 description: Array[Byte],
                 quantity: Long,
                 decimals: Byte,
                 reissuable: Boolean,
                 fee: Long,
                 timestamp: Long): Either[ValidationError, TransactionT] =
    signed(sender, name, description, quantity, decimals, reissuable, fee, timestamp, sender)

}
