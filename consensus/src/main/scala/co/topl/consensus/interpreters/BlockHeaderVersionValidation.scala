package co.topl.consensus.interpreters

import co.topl.consensus.algebras._
import co.topl.consensus.models.{BlockHeader, BlockHeaderValidationFailure, BlockId}
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.algebras.ClockAlgebra
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.consensus.models.BlockHeaderValidationFailures._
import co.topl.consensus.interpreters.VotingEventSourceState.VotingData
import co.topl.eventtree.EventSourcedState

object BlockHeaderVersionValidation {

  def make[F[_]: Async](
    clockAlgebra:             ClockAlgebra[F],
    versionsEventSourceState: EventSourcedState[F, VotingData[F], BlockId]
  ): F[BlockHeaderVersionValidationAlgebra[F]] =
    Async[F].delay(new Impl[F](clockAlgebra, versionsEventSourceState))

  private class Impl[F[_]: Async](
    clockAlgebra:             ClockAlgebra[F],
    versionsEventSourceState: EventSourcedState[F, VotingData[F], BlockId]
  ) extends BlockHeaderVersionValidationAlgebra[F] {

    override def validate(header: BlockHeader): F[Either[BlockHeaderValidationFailure, BlockHeader]] =
      versionsEventSourceState.useStateAt(header.parentHeaderId) { versionData =>
        for {
          versionInfo <- versionData.versionAlgebra.pure[F]
          blockEpoch  <- clockAlgebra.epochOf(header.slot)
          expected    <- versionInfo.getVersionForEpoch(blockEpoch)
          actual      <- header.versionId.pure[F]
        } yield Either.cond(actual == expected, header, IncorrectVersionId(expected, actual))
      }
  }
}
