package one.mir.it

import com.typesafe.config.ConfigFactory.{defaultApplication, defaultReference}
import one.mir.account.PublicKeyAccount
import one.mir.block.Block
import one.mir.consensus.PoSSelector
import one.mir.db.openDB
import one.mir.history.StorageFactory
import one.mir.settings._
import one.mir.state.{ByteStr, EitherExt2}
import one.mir.utils.NTP
import net.ceedubs.ficus.Ficus._

object BaseTargetChecker {
  def main(args: Array[String]): Unit = {
    val sharedConfig = Docker.genesisOverride
      .withFallback(Docker.configTemplate)
      .withFallback(defaultApplication())
      .withFallback(defaultReference())
      .resolve()
    val settings     = MirSettings.fromConfig(sharedConfig)
    val genesisBlock = Block.genesis(settings.blockchainSettings.genesisSettings).explicitGet()
    val db           = openDB("/tmp/tmp-db")
    val time         = new NTP("ntp.pool.org")
    val bu           = StorageFactory(settings, db, time)
    val pos          = new PoSSelector(bu, settings.blockchainSettings)
    bu.processBlock(genesisBlock)

    try {
      NodeConfigs.Default.map(_.withFallback(sharedConfig)).collect {
        case cfg if cfg.as[Boolean]("mir.miner.enable") =>
          val account   = PublicKeyAccount(cfg.as[ByteStr]("public-key").arr)
          val address   = account.toAddress
          val balance   = bu.balance(address, None)
          val consensus = genesisBlock.consensusData
          val timeDelay = pos
            .getValidBlockDelay(bu.height, account.publicKey, consensus.baseTarget, balance)
            .explicitGet()

          f"$address: ${timeDelay * 1e-3}%10.3f s"
      }
    } finally time.close()
  }
}
