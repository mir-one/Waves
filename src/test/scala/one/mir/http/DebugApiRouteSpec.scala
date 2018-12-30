package one.mir.http

import one.mir.{NTPTime, TestWallet}
import one.mir.settings.MirSettings
import one.mir.api.http.ApiKeyNotValid

class DebugApiRouteSpec extends RouteSpec("/debug") with RestAPISettingsHelper with TestWallet with NTPTime {
  private val sampleConfig = com.typesafe.config.ConfigFactory.load()
  private val mirSettings  = MirSettings.fromConfig(sampleConfig)
  private val configObject = sampleConfig.root()
  private val route =
    DebugApiRoute(mirSettings, ntpTime, null, null, null, null, null, null, null, null, null, null, null, null, null, configObject).route

  routePath("/configInfo") - {
    "requires api-key header" in {
      Get(routePath("/configInfo?full=true")) ~> route should produce(ApiKeyNotValid)
      Get(routePath("/configInfo?full=false")) ~> route should produce(ApiKeyNotValid)
    }
  }
}
