package one.mir.state.diffs

import one.mir.settings.TestFunctionalitySettings
import one.mir.state.EitherExt2
import one.mir.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import one.mir.lagonaki.mocks.TestBlock
import one.mir.transaction.GenesisTransaction
import one.mir.transaction.lease.LeaseTransaction
import one.mir.transaction.transfer._

class BalanceDiffValidationTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  property("disallows overflow") {
    val preconditionsAndPayment: Gen[(GenesisTransaction, GenesisTransaction, TransferTransactionV1, TransferTransactionV1)] = for {
      master    <- accountGen
      master2   <- accountGen
      recipient <- otherAccountGen(candidate = master)
      ts        <- timestampGen
      gen1: GenesisTransaction = GenesisTransaction.create(master, Long.MaxValue - 1, ts).explicitGet()
      gen2: GenesisTransaction = GenesisTransaction.create(master2, Long.MaxValue - 1, ts).explicitGet()
      fee    <- smallFeeGen
      amount <- Gen.choose(Long.MaxValue / 2, Long.MaxValue - fee - 1)
      transfer1 = createMirTransfer(master, recipient, amount, fee, ts).explicitGet()
      transfer2 = createMirTransfer(master2, recipient, amount, fee, ts).explicitGet()
    } yield (gen1, gen2, transfer1, transfer2)

    forAll(preconditionsAndPayment) {
      case (gen1, gen2, transfer1, transfer2) =>
        assertDiffEi(Seq(TestBlock.create(Seq(gen1, gen2, transfer1))), TestBlock.create(Seq(transfer2))) { blockDiffEi =>
          blockDiffEi should produce("negative mir balance")
        }
    }
  }

  property("disallows lease overflow") {
    val setup = for {
      master1   <- accountGen
      master2   <- accountGen
      recipient <- accountGen
      ts        <- timestampGen
      gen1 = GenesisTransaction.create(master1, Long.MaxValue - 1, ts).explicitGet()
      gen2 = GenesisTransaction.create(master2, Long.MaxValue - 1, ts).explicitGet()
      fee  <- smallFeeGen
      amt1 <- Gen.choose(Long.MaxValue / 2 + 1, Long.MaxValue - 1 - fee)
      amt2 <- Gen.choose(Long.MaxValue / 2 + 1, Long.MaxValue - 1 - fee)
      l1   <- createLease(master1, amt1, fee, ts, recipient)
      l2   <- createLease(master2, amt2, fee, ts, recipient)
    } yield (gen1, gen2, l1, l2)

    forAll(setup) {
      case (gen1, gen2, l1, l2) =>
        assertDiffEi(Seq(TestBlock.create(Seq(gen1, gen2, l1))), TestBlock.create(Seq(l2)))(totalDiffEi =>
          totalDiffEi should produce("negative effective balance"))
    }
  }

  val ownLessThatLeaseOut: Gen[(GenesisTransaction, TransferTransactionV1, LeaseTransaction, LeaseTransaction, TransferTransactionV1)] = for {
    master <- accountGen
    alice  <- accountGen
    bob    <- accountGen
    cooper <- accountGen
    ts     <- positiveIntGen
    amt    <- positiveLongGen
    fee    <- smallFeeGen
    genesis: GenesisTransaction                   = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
    masterTransfersToAlice: TransferTransactionV1 = createMirTransfer(master, alice, amt, fee, ts).explicitGet()
    (aliceLeasesToBob, _)    <- leaseAndCancelGeneratorP(alice, bob, alice) suchThat (_._1.amount < amt)
    (masterLeasesToAlice, _) <- leaseAndCancelGeneratorP(master, alice, master) suchThat (_._1.amount > aliceLeasesToBob.amount)
    transferAmt              <- Gen.choose(amt - fee - aliceLeasesToBob.amount, amt - fee)
    aliceTransfersMoreThanOwnsMinusLeaseOut = createMirTransfer(alice, cooper, transferAmt, fee, ts).explicitGet()

  } yield (genesis, masterTransfersToAlice, aliceLeasesToBob, masterLeasesToAlice, aliceTransfersMoreThanOwnsMinusLeaseOut)

  property("can transfer more than own-leaseOut before allow-leased-balance-transfer-until") {
    val settings = TestFunctionalitySettings.Enabled.copy(blockVersion3AfterHeight = 4)

    forAll(ownLessThatLeaseOut) {
      case (genesis, masterTransfersToAlice, aliceLeasesToBob, masterLeasesToAlice, aliceTransfersMoreThanOwnsMinusLeaseOut) =>
        assertDiffEi(
          Seq(TestBlock.create(Seq(genesis, masterTransfersToAlice, aliceLeasesToBob, masterLeasesToAlice))),
          TestBlock.create(Seq(aliceTransfersMoreThanOwnsMinusLeaseOut)),
          settings
        ) { totalDiffEi =>
          totalDiffEi shouldBe 'right
        }
    }
  }

  property("cannot transfer more than own-leaseOut after allow-leased-balance-transfer-until") {
    val settings = TestFunctionalitySettings.Enabled.copy(blockVersion3AfterHeight = 4)

    forAll(ownLessThatLeaseOut) {
      case (genesis, masterTransfersToAlice, aliceLeasesToBob, masterLeasesToAlice, aliceTransfersMoreThanOwnsMinusLeaseOut) =>
        assertDiffEi(
          Seq(
            TestBlock.create(Seq(genesis)),
            TestBlock.create(Seq()),
            TestBlock.create(Seq()),
            TestBlock.create(Seq()),
            TestBlock.create(Seq(masterTransfersToAlice, aliceLeasesToBob, masterLeasesToAlice))
          ),
          TestBlock.create(Seq(aliceTransfersMoreThanOwnsMinusLeaseOut)),
          settings
        ) { totalDiffEi =>
          totalDiffEi should produce("trying to spend leased money")
        }
    }
  }
}
