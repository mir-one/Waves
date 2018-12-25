package one.mir.settings

import one.mir.Version
import one.mir.utils.ScorexLogging

/**
  * System constants here.
  */
object Constants extends ScorexLogging {
  val ApplicationName = "mir"
  val AgentName       = s"Mir v${Version.VersionString}"

  val UnitsInWave = 100000000L
  val TotalMir    = 100000000L
}
