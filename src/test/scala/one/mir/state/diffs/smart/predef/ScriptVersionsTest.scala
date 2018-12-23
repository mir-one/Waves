package one.mir.state.diffs.smart.predef

import one.mir.TransactionGen
import one.mir.features.BlockchainFeatures
import one.mir.state.EitherExt2
import one.mir.state.diffs._
import one.mir.lang.{ScriptVersion, Testing}
import one.mir.lang.ScriptVersion.Versions._
import one.mir.lang.v1.compiler.CompilerV1
import one.mir.lang.v1.compiler.Terms.{EVALUATED, TRUE}
import one.mir.lang.v1.parser.Parser
import one.mir.settings.TestFunctionalitySettings
import one.mir.state.Blockchain
import one.mir.transaction.smart.SetScriptTransaction
import one.mir.transaction.{GenesisTransaction, Transaction}
import one.mir.transaction.smart.script.ScriptRunner
import one.mir.transaction.smart.script.v1.ScriptV1
import one.mir.utils.{EmptyBlockchain, compilerContext}
import fastparse.core.Parsed.Success
import org.scalatest.{FreeSpec, Matchers}
import org.scalatest.prop.PropertyChecks
import shapeless.Coproduct

class ScriptVersionsTest extends FreeSpec with PropertyChecks with Matchers with TransactionGen {
  def eval[T <: EVALUATED](script: String,
                           version: ScriptVersion,
                           tx: Transaction = null,
                           blockchain: Blockchain = EmptyBlockchain): Either[String, T] = {
    val Success(expr, _) = Parser(script)
    for {
      compileResult <- CompilerV1(compilerContext(version, isAssetScript = false), expr)
      (typedExpr, _) = compileResult
      s <- ScriptV1(version, typedExpr, checkSize = false)
      r <- ScriptRunner[T](blockchain.height, Coproduct(tx), blockchain, s, isTokenScript = false)._2
    } yield r

  }

  val duplicateNames =
    """
      |match tx {
      |  case tx: TransferTransaction => true
      |  case _ => false
      |}
    """.stripMargin

  val orderTypeBindings = "let t = Buy; t == Buy"

  "ScriptV1" - {
    "forbids duplicate names" in {
      import one.mir.lagonaki.mocks.TestBlock.{create => block}

      val Success(expr, _)      = Parser(duplicateNames)
      val Right((typedExpr, _)) = CompilerV1(compilerContext(V1, isAssetScript = false), expr)
      val settings = TestFunctionalitySettings.Enabled.copy(
        preActivatedFeatures = Map(BlockchainFeatures.SmartAccounts.id -> 0, BlockchainFeatures.SmartAccountTrading.id -> 3))
      val setup = for {
        master <- accountGen
        ts     <- positiveLongGen
        genesis = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
        script  = ScriptV1(V1, typedExpr, checkSize = false).explicitGet()
        tx      = SetScriptTransaction.selfSigned(1, master, Some(script), 100000, ts + 1).explicitGet()
      } yield (genesis, tx)

      forAll(setup) {
        case (genesis, tx) =>
          assertDiffEi(Seq(block(Seq(genesis))), block(Seq(tx)), settings) { blockDiffEi =>
            blockDiffEi should produce("duplicate variable names")
          }

          assertDiffEi(Seq(block(Seq(genesis)), block(Seq())), block(Seq(tx)), settings) { blockDiffEi =>
            blockDiffEi shouldBe 'right
          }
      }
    }

    "does not have bindings defined in V2" in {
      eval[EVALUATED](orderTypeBindings, V1) should produce("definition of 'Buy' is not found")
    }
  }

  "ScriptV2" - {
    "allows duplicate names" in {
      forAll(transferV2Gen) { tx =>
        eval[EVALUATED](duplicateNames, V2, tx) shouldBe Testing.evaluated(true)
      }
    }

    "has bindings defined in V2" in {
      eval[EVALUATED](orderTypeBindings, V2) shouldBe Testing.evaluated(true)
    }

    "only works after SmartAccountTrading feature activation" in {
      import one.mir.lagonaki.mocks.TestBlock.{create => block}

      val settings = TestFunctionalitySettings.Enabled.copy(preActivatedFeatures = Map(BlockchainFeatures.SmartAccountTrading.id -> 3))
      val setup = for {
        master <- accountGen
        ts     <- positiveLongGen
        genesis = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
        script  = ScriptV1(V2, TRUE, checkSize = false).explicitGet()
        tx      = SetScriptTransaction.selfSigned(1, master, Some(script), 100000, ts + 1).explicitGet()
      } yield (genesis, tx)

      forAll(setup) {
        case (genesis, tx) =>
          assertDiffEi(Seq(block(Seq(genesis))), block(Seq(tx)), settings) { blockDiffEi =>
            blockDiffEi should produce("Script version 2 has not been activated yet")
          }

          assertDiffEi(Seq(block(Seq(genesis)), block(Seq())), block(Seq(tx)), settings) { blockDiffEi =>
            blockDiffEi shouldBe 'right
          }
      }
    }
  }
}
