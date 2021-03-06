package one.mir.generator

import java.util.concurrent.atomic.AtomicReference

import cats.Show
import one.mir.generator.DynamicWideTransactionGenerator.Settings
import one.mir.generator.utils.Gen
import one.mir.account.PrivateKeyAccount
import one.mir.transaction.Transaction

class DynamicWideTransactionGenerator(settings: Settings, accounts: Seq[PrivateKeyAccount]) extends TransactionGenerator {
  require(accounts.nonEmpty)

  private val nextTxsNumber = new AtomicReference[Double](settings.start)

  private val limitedRecipientGen = Gen.address(settings.limitDestAccounts)

  override def next(): Iterator[Transaction] = {
    val currTxsNumber = nextTxsNumber.getAndUpdate { x =>
      val newValue = x + settings.growAdder
      settings.maxTxsPerRequest.foldLeft(newValue)(Math.min(_, _))
    }.toInt

    Gen.txs(settings.minFee, settings.maxFee, accounts, limitedRecipientGen).take(currTxsNumber)
  }

}

object DynamicWideTransactionGenerator {

  case class Settings(start: Int, growAdder: Double, maxTxsPerRequest: Option[Int], limitDestAccounts: Option[Int], minFee: Long, maxFee: Long) {
    require(start >= 1)
  }

  object Settings {
    implicit val toPrintable: Show[Settings] = { x =>
      import x._
      s"""txs at start: $start
         |grow adder: $growAdder
         |max txs: $maxTxsPerRequest
         |limit destination accounts: $limitDestAccounts
         |min fee: $minFee
         |max fee: $maxFee""".stripMargin
    }
  }

}
