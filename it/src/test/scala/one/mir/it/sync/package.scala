package one.mir.it

import one.mir.api.http.assets.{SignedIssueV1Request, SignedIssueV2Request}
import one.mir.it.util._
import one.mir.state.DataEntry
import one.mir.transaction.assets.{IssueTransactionV1, IssueTransactionV2}
import one.mir.transaction.smart.script.{Script, ScriptCompiler}
import one.mir.utils.Base58

package object sync {
  val smartFee: Long                   = 0.004.mir
  val minFee: Long                     = 0.001.mir
  val leasingFee: Long                 = 0.002.mir
  val issueFee: Long                   = 1.mir
  val burnFee: Long                    = 1.mir
  val sponsorFee: Long                 = 1.mir
  val setAssetScriptFee: Long          = 1.mir
  val setScriptFee: Long               = 0.01.mir
  val transferAmount: Long             = 10.mir
  val leasingAmount: Long              = transferAmount
  val issueAmount: Long                = transferAmount
  val massTransferFeePerTransfer: Long = 0.0005.mir
  val someAssetAmount: Long            = 9999999999999L
  val matcherFee: Long                 = 0.003.mir
  val orderFee: Long                   = matcherFee
  val smartMatcherFee: Long            = 0.007.mir
  val smartMinFee: Long                = minFee + smartFee

  def calcDataFee(data: List[DataEntry[_]]): Long = {
    val dataSize = data.map(_.toBytes.length).sum + 128
    if (dataSize > 1024) {
      minFee * (dataSize / 1024 + 1)
    } else minFee
  }

  def calcMassTransferFee(numberOfRecipients: Int): Long = {
    minFee + massTransferFeePerTransfer * (numberOfRecipients + 1)
  }

  val supportedVersions: List[Byte] = List(1, 2)

  val script: Script       = ScriptCompiler(s"""true""".stripMargin, isAssetScript = false).explicitGet()._1
  val scriptBase64: String = script.bytes.value.base64

  val errNotAllowedByToken = "Transaction is not allowed by token-script"

  def createSignedIssueRequest(tx: IssueTransactionV1): SignedIssueV1Request = {
    import tx._
    SignedIssueV1Request(
      Base58.encode(tx.sender.publicKey),
      new String(name),
      new String(description),
      quantity,
      decimals,
      reissuable,
      fee,
      timestamp,
      signature.base58
    )
  }

  def createSignedIssueRequest(tx: IssueTransactionV2): SignedIssueV2Request = {
    import tx._
    SignedIssueV2Request(
      2.toByte,
      Base58.encode(tx.sender.publicKey),
      new String(name),
      new String(description),
      quantity,
      decimals,
      reissuable,
      fee,
      timestamp,
      proofs.proofs.map(_.toString),
      tx.script.map(_.bytes().base64)
    )
  }

}
