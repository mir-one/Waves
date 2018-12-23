package one.mir.api.http.leasing

import akka.http.scaladsl.server.Route
import one.mir.api.http._
import one.mir.http.BroadcastRoute
import one.mir.settings.RestAPISettings
import one.mir.utx.UtxPool
import io.netty.channel.group.ChannelGroup

case class LeaseBroadcastApiRoute(settings: RestAPISettings, utx: UtxPool, allChannels: ChannelGroup) extends ApiRoute with BroadcastRoute {
  override val route = pathPrefix("leasing" / "broadcast") {
    signedLease ~ signedLeaseCancel
  }

  def signedLease: Route = (path("lease") & post) {
    json[SignedLeaseV1Request] { leaseReq =>
      doBroadcast(leaseReq.toTx)
    }
  }

  def signedLeaseCancel: Route = (path("cancel") & post) {
    json[SignedLeaseCancelV1Request] { leaseCancelReq =>
      doBroadcast(leaseCancelReq.toTx)
    }
  }
}
