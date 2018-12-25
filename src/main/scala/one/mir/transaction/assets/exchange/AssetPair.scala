package one.mir.transaction.assets.exchange

import one.mir.state.ByteStr
import one.mir.transaction._
import one.mir.transaction.assets.exchange.Order.assetIdBytes
import one.mir.transaction.assets.exchange.Validation.booleanOperators
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.json.{JsObject, Json}

import scala.annotation.meta.field
import scala.util.{Success, Try}

@ApiModel
case class AssetPair(@(ApiModelProperty @field)(
                       value = "Base58 encoded amount asset id",
                       dataType = "string",
                       example = "MIR"
                     ) amountAsset: Option[AssetId],
                     @(ApiModelProperty @field)(
                       value = "Base58 encoded amount price id",
                       dataType = "string",
                       example = "8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS"
                     ) priceAsset: Option[AssetId]) {
  import AssetPair._

  @ApiModelProperty(hidden = true)
  lazy val priceAssetStr: String = assetIdStr(priceAsset)
  @ApiModelProperty(hidden = true)
  lazy val amountAssetStr: String = assetIdStr(amountAsset)
  override def toString: String   = key
  def key: String                 = amountAssetStr + "-" + priceAssetStr
  def isValid: Validation         = (amountAsset != priceAsset) :| "Invalid AssetPair"
  def bytes: Array[Byte]          = assetIdBytes(amountAsset) ++ assetIdBytes(priceAsset)
  def json: JsObject = Json.obj(
    "amountAsset" -> amountAsset.map(_.base58),
    "priceAsset"  -> priceAsset.map(_.base58)
  )
  def reverse = AssetPair(priceAsset, amountAsset)

  def assets: Set[Option[AssetId]] = Set(amountAsset, priceAsset)
}

object AssetPair {
  val MirName = "MIR"

  def assetIdStr(aid: Option[AssetId]): String = aid.fold(MirName)(_.base58)

  def extractAssetId(a: String): Try[Option[AssetId]] = a match {
    case `MirName` => Success(None)
    case other     => ByteStr.decodeBase58(other).map(Option(_))
  }

  def createAssetPair(amountAsset: String, priceAsset: String): Try[AssetPair] =
    for {
      a1 <- extractAssetId(amountAsset)
      a2 <- extractAssetId(priceAsset)
    } yield AssetPair(a1, a2)
}
