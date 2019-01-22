package one.mir.api.http.alias

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}
import one.mir.account.{Alias, PublicKeyAccount}
import one.mir.api.http.BroadcastRequest
import one.mir.transaction.TransactionParsers.SignatureStringLength
import one.mir.transaction.{CreateAliasTransactionV1, ValidationError}

case class SignedCreateAliasV1Request(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                                      senderPublicKey: String,
                                      @ApiModelProperty(required = true)
                                      fee: Long,
                                      @ApiModelProperty(value = "Alias", required = true)
                                      alias: String,
                                      @ApiModelProperty(required = true)
                                      timestamp: Long,
                                      @ApiModelProperty(required = true)
                                      signature: String)
    extends BroadcastRequest {
  def toTx: Either[ValidationError, CreateAliasTransactionV1] =
    for {
      _sender    <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _signature <- parseBase58(signature, "invalid.signature", SignatureStringLength)
      _alias     <- Alias.buildWithCurrentChainId(alias)
      _t         <- CreateAliasTransactionV1.create(_sender, _alias, fee, timestamp, _signature)
    } yield _t
}

object SignedCreateAliasV1Request {
  implicit val broadcastAliasV1RequestReadsFormat: Format[SignedCreateAliasV1Request] = Json.format
}
