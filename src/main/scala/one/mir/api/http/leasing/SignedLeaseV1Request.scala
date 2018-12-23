package one.mir.api.http.leasing

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}
import one.mir.account.{AddressOrAlias, PublicKeyAccount}
import one.mir.api.http.BroadcastRequest
import one.mir.transaction.TransactionParsers.SignatureStringLength
import one.mir.transaction.ValidationError
import one.mir.transaction.lease.LeaseTransactionV1

case class SignedLeaseV1Request(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                                senderPublicKey: String,
                                @ApiModelProperty(required = true)
                                amount: Long,
                                @ApiModelProperty(required = true)
                                fee: Long,
                                @ApiModelProperty(value = "Recipient address", required = true)
                                recipient: String,
                                @ApiModelProperty(required = true)
                                timestamp: Long,
                                @ApiModelProperty(required = true)
                                signature: String)
    extends BroadcastRequest {
  def toTx: Either[ValidationError, LeaseTransactionV1] =
    for {
      _sender    <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _signature <- parseBase58(signature, "invalid.signature", SignatureStringLength)
      _recipient <- AddressOrAlias.fromString(recipient)
      _t         <- LeaseTransactionV1.create(_sender, amount, fee, timestamp, _recipient, _signature)
    } yield _t
}

object SignedLeaseV1Request {
  implicit val broadcastLeaseRequestReadsFormat: Format[SignedLeaseV1Request] = Json.format
}
