package one.mir.it

import one.mir.account.PrivateKeyAccount
import one.mir.it.api.SyncHttpApi._
import one.mir.it.util._
import one.mir.transaction.smart.SetScriptTransaction
import one.mir.transaction.smart.script.ScriptCompiler
import one.mir.utils.ScorexLogging
import org.scalatest.{BeforeAndAfterAll, Suite}

trait MatcherNode extends BeforeAndAfterAll with Nodes with ScorexLogging {
  this: Suite =>

  def matcherNode: Node = nodes.head
  def aliceNode: Node   = nodes(1)
  def bobNode: Node     = nodes(2)

  protected lazy val matcherAddress: String = matcherNode.createAddress()
  protected lazy val aliceAddress: String   = aliceNode.createAddress()
  protected lazy val bobAddress: String     = bobNode.createAddress()

  protected lazy val matcherAcc = PrivateKeyAccount.fromSeed(matcherNode.seed(matcherAddress)).right.get
  protected lazy val aliceAcc   = PrivateKeyAccount.fromSeed(aliceNode.seed(aliceAddress)).right.get
  protected lazy val bobAcc     = PrivateKeyAccount.fromSeed(bobNode.seed(bobAddress)).right.get

  private val addresses = Seq(matcherAddress, aliceAddress, bobAddress)

  //really before all tests, because FreeSpec issue with "-" and "in"
  initialBalances()

  def initialBalances(): Unit = {
    List(matcherNode, aliceNode, bobNode).indices
      .map { i =>
        nodes(i).transfer(nodes(i).address, addresses(i), 10000.mir, 0.001.mir).id
      }
      .foreach(nodes.waitForTransaction)
  }

  def initialScripts(): Unit = {
    for (i <- List(matcherNode, aliceNode, bobNode).indices) {
      val script = ScriptCompiler("true", isAssetScript = false).explicitGet()._1
      val pk     = PrivateKeyAccount.fromSeed(nodes(i).seed(addresses(i))).right.get
      val setScriptTransaction = SetScriptTransaction
        .selfSigned(SetScriptTransaction.supportedVersions.head, pk, Some(script), 0.01.mir, System.currentTimeMillis())
        .right
        .get

      matcherNode
        .signedBroadcast(setScriptTransaction.json(), waitForTx = true)
    }
  }

  def setContract(contractText: Option[String], acc: PrivateKeyAccount): String = {
    val script = contractText.map { x =>
      val scriptText = x.stripMargin
      ScriptCompiler(scriptText, isAssetScript = false).explicitGet()._1
    }
    val setScriptTransaction = SetScriptTransaction
      .selfSigned(SetScriptTransaction.supportedVersions.head, acc, script, 0.014.mir, System.currentTimeMillis())
      .right
      .get

    matcherNode
      .signedBroadcast(setScriptTransaction.json(), waitForTx = true)
      .id
  }
}
