package one.mir.api.http.assets

import akka.http.scaladsl.server.Route
import one.mir.api.http._
import one.mir.http.BroadcastRoute
import one.mir.network._
import one.mir.settings.RestAPISettings
import one.mir.state.diffs.TransactionDiffer.TransactionValidationError
import one.mir.transaction.{Transaction, ValidationError}
import one.mir.utx.UtxPool
import io.netty.channel.group.ChannelGroup

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Left, Right}

case class AssetsBroadcastApiRoute(settings: RestAPISettings, utx: UtxPool, allChannels: ChannelGroup) extends ApiRoute with BroadcastRoute {

  override val route: Route = pathPrefix("assets" / "broadcast") {
    issue ~ reissue ~ transfer ~ burnRoute ~ batchTransfer ~ exchange
  }

  def issue: Route = (path("issue") & post) {
    json[SignedIssueV1Request] { issueReq =>
      doBroadcast(issueReq.toTx)
    }
  }

  def reissue: Route = (path("reissue") & post) {
    json[SignedReissueV1Request] { reissueReq =>
      doBroadcast(reissueReq.toTx)
    }
  }

  def burnRoute: Route = (path("burn") & post) {
    json[SignedBurnV1Request] { burnReq =>
      doBroadcast(burnReq.toTx)
    }
  }

  def batchTransfer: Route = (path("batch-transfer") & post) {
    json[List[SignedTransferRequests]] { reqs =>
      val r = Future
        .traverse(reqs) { req =>
          Future {
            req.eliminate(
              _.toTx,
              _.eliminate(
                _.toTx,
                _ => Left(ValidationError.UnsupportedTransactionType)
              )
            )
          }
        }
        .map { xs: List[Either[ValidationError, Transaction]] =>
          utx.batched { ops =>
            xs.map {
              case Left(e)   => Left(e)
              case Right(tx) => ops.putIfNew(tx).map { case (isNew, _) => (tx, isNew) }
            }
          }
        }
        .map { xs =>
          xs.map {
            case Left(TransactionValidationError(_: ValidationError.AlreadyInTheState, tx)) => Right(tx -> false)
            case Left(e)                                                                    => Left(ApiError.fromValidationError(e))
            case Right(x)                                                                   => Right(x)
          }
        }

      r.foreach { xs =>
        val newTxs = xs.collect { case Right((tx, true)) => tx }
        allChannels.broadcastTx(newTxs)
      }

      r.map { xs =>
        xs.map {
          case Left(e)        => e.json
          case Right((tx, _)) => tx.json()
        }
      }
    }
  }

  def transfer: Route = (path("transfer") & post) {
    json[SignedTransferRequests] { transferReq =>
      doBroadcast(
        transferReq.eliminate(
          _.toTx,
          _.eliminate(
            _.toTx,
            _ => Left(ValidationError.UnsupportedTransactionType)
          )
        )
      )
    }
  }

  def exchange: Route = (path("exchange") & post) {
    json[SignedExchangeRequest] { req =>
      doBroadcast(req.toTx)
    }
  }
}
