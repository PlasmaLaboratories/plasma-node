package xyz.stratalab.transactiongenerator.interpreters

import cats.effect.IO
import cats.effect.std.{Random, SecureRandom}
import cats.implicits._
import munit.CatsEffectSuite
import quivr.models.SmallData
import xyz.stratalab.numerics.implicits._
import xyz.stratalab.sdk.models.box.Value
import xyz.stratalab.sdk.models.transaction.{IoTransaction, Schedule, UnspentTransactionOutput}
import xyz.stratalab.sdk.models.{Datum, Event}
import xyz.stratalab.sdk.validation.{TransactionCostCalculatorInterpreter, TransactionCostConfig}

class Fs2TransactionGeneratorSpec extends CatsEffectSuite {
  type F[A] = IO[A]

  test("Produces a stream of transactions") {
    for {
      seedTransaction <-
        IoTransaction.defaultInstance
          .withOutputs(
            Seq(
              UnspentTransactionOutput(
                address = HeightLockOneSpendingAddress,
                value = Value.defaultInstance.withLvl(Value.LVL(1000000))
              )
            )
          )
          .withDatum(
            Datum.IoTransaction(Event.IoTransaction(Schedule(0, 0, 0), SmallData.defaultInstance))
          )
          .pure[F]
      wallet = applyTransaction(emptyWallet)(seedTransaction)
      implicit0(random: Random[F]) <- SecureRandom.javaSecuritySecureRandom[F]
      costCalculator = TransactionCostCalculatorInterpreter.make[F](TransactionCostConfig())
      underTest <- Fs2TransactionGenerator.make[F](wallet, costCalculator, Fs2TransactionGenerator.randomMetadata[F])
      stream    <- underTest.generateTransactions
      _         <- stream.take(500).compile.count.assertEquals(500L)
    } yield ()
  }
}
