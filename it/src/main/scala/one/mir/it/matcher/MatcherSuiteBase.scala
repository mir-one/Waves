package one.mir.it.matcher

import com.typesafe.config.Config
import one.mir.it._
import one.mir.it.transactions.NodesFromDocker
import org.scalatest._
import one.mir.it.util._
import scala.concurrent.ExecutionContext

abstract class MatcherSuiteBase
    extends FreeSpec
    with Matchers
    with CancelAfterFailure
    with ReportingTestName
    with NodesFromDocker
    with BeforeAndAfterAll
    with MatcherNode {

  protected implicit val ec: ExecutionContext = ExecutionContext.Implicits.global

  val defaultAssetQuantity = 999999999999L

  val smartFee         = 0.004.mir
  val minFee           = 0.001.mir + smartFee
  val issueFee         = 1.mir + smartFee
  val leasingFee       = 0.002.mir + smartFee
  val tradeFee         = 0.003.mir
  val smartTradeFee    = tradeFee + smartFee
  val twoSmartTradeFee = tradeFee + 2 * smartFee

  protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .withDefault(4)
      .buildNonConflicting()

}
