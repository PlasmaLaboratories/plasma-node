package org.plasmalabs.node.cli

import cats.effect.Async
import cats.effect.std.Console
import com.google.protobuf.duration.Duration
import fs2.io.file.Path
import org.plasmalabs.models.protocol.{ConfigConverter, ConfigGenesis}
import org.plasmalabs.node.cli.ProposalCommand.Messages
import org.plasmalabs.sdk.models.box.Value
import org.plasmalabs.sdk.models.transaction.{IoTransaction, Schedule, UnspentTransactionOutput}
import org.plasmalabs.sdk.models.{Datum, Event, LockAddress}
import quivr.models.Ratio

object ProposalCommand {

  def apply[F[_]: Async: Console]: StageResultT[F, Unit] = new ProposalCommandImpl[F].command

  object Messages {

    val intro =
      "This tool will guide you through the process of preparing a Proposal Update. Press <Enter> to skip."

    val lockAddress = "Using a wallet (i.e. Brambl CLI), create or provide a new LockAddress."

    val writeTransaction = "This process will save the Proposal update in the current /tmp path"

    val finalMsg =
      "Your Update proposal has been saved. The Transaction that was saved should be imported into Brambl-CLI for input selection and broadcast."
  }
}

/**
 * Proposal Update command used by node cli app
 */
private class ProposalCommandImpl[F[_]: Async](implicit c: Console[F]) {

  val command: StageResultT[F, Unit] =
    for {
      _ <- writeMessage[F](Messages.intro)

      label                   <- readParameter[F, String]("label <required>", List("Update Slot duration"))
      fEffective              <- readDefaultedOptional[F, Ratio]("f-effective", List("15/100"), "12/100")
      vrfLddCutoff            <- readDefaultedOptional[F, Int]("vrf-ldd-cutoff", List("50"), "15")
      vrfPrecision            <- readDefaultedOptional[F, Int]("vrf-precision", List("40"), "40")
      vrfBaselineDifficulty   <- readDefaultedOptional[F, Ratio]("vrf-baseline-difficulty", List("1/20"), "5/100")
      vrfAmplitude            <- readDefaultedOptional[F, Ratio]("vrf-amplitude", List("1/2"), "50, 100")
      slotGapLeaderElection   <- readDefaultedOptional[F, Long]("slot-gap-leader-election", List("0"), "0")
      chainSelectionKLookback <- readDefaultedOptional[F, Long]("chain-selection-k-lookback", List("50"), "5184")
      slotDuration            <- readDefaultedOptional[F, Duration]("slot-duration", List("1000 milli"), "1000 milli")
      forwardBiasedSlotWindow <- readDefaultedOptional[F, Long]("forward-biased-slot-window", List("50"), "50")
      operationalPeriodsPerEpoch <- readDefaultedOptional[F, Long]("operational-periods-per-epoch", List("2"), "25")
      kesKeyHours                <- readDefaultedOptional[F, Int]("kes-key-hours", List("2"), "9")
      kesKeyMinutes              <- readDefaultedOptional[F, Int]("kes-key-minutes", List("9"), "9")

      proposal = ConfigConverter.pack[ConfigGenesis](
        ConfigGenesis(
          label,
          fEffective,
          vrfLddCutoff,
          vrfPrecision,
          vrfBaselineDifficulty,
          vrfAmplitude,
          chainSelectionKLookback,
          slotDuration,
          forwardBiasedSlotWindow,
          operationalPeriodsPerEpoch,
          kesKeyHours,
          kesKeyMinutes,
          slotGapLeaderElection
        )
      )

      lockAddress <- readParameter[F, LockAddress](
        "Address",
        List("ptetP7jshHVrEKqDRdKAZtuybPZoMWTKKM2ngaJ7L5iZnxP5BprDB3hGJEFr")
      )

      _ <- writeMessage[F](Messages.lockAddress)
      //      _ <- requiresBramblCli // TODO refactor this method and print this message for BramblCLI

      unspentTransactionOutput =
        UnspentTransactionOutput(lockAddress, Value.defaultInstance.withConfigProposal(proposal))

      transaction = IoTransaction(datum =
        Datum.IoTransaction(
          Event.IoTransaction.defaultInstance.withSchedule(
            Schedule(0L, Long.MaxValue, System.currentTimeMillis())
          )
        )
      ).withOutputs(Seq(unspentTransactionOutput))

      // TODO The name should be the id of the proposal Updated, path should be a user input
      _ <- writeFile[F](Path("/tmp"))(transaction.toByteArray)(
        "Proposal Update",
        "proposalUpdate.transaction.pbuf"
      )
      _ <- writeMessage[F](Messages.writeTransaction)
      _ <- writeMessage[F](Messages.finalMsg)
    } yield StageResult.Menu

}
