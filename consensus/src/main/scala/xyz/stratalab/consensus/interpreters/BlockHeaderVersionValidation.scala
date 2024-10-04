package xyz.stratalab.consensus.interpreters

import cats.data._
import cats.effect.kernel.Async
import cats.implicits._
import xyz.stratalab.algebras.ClockAlgebra
import xyz.stratalab.algebras.ClockAlgebra.implicits._
import xyz.stratalab.consensus.algebras._
import xyz.stratalab.consensus.interpreters.VotingEventSourceState.VotingData
import xyz.stratalab.consensus.models.BlockHeaderValidationFailures._
import xyz.stratalab.consensus.models.{BlockHeader, BlockHeaderValidationFailure, BlockId}
import xyz.stratalab.eventtree.EventSourcedState
import xyz.stratalab.models.VersionId

object BlockHeaderVersionValidation {

  def make[F[_]: Async](
    clockAlgebra:             ClockAlgebra[F],
    versionsEventSourceState: EventSourcedState[F, VotingData[F], BlockId],
    maxSupportedVersion:      VersionId
  ): F[BlockHeaderVersionValidationAlgebra[F]] =
    Async[F].delay(new Impl[F](clockAlgebra, versionsEventSourceState, maxSupportedVersion))

  private class Impl[F[_]: Async](
    clockAlgebra:             ClockAlgebra[F],
    versionsEventSourceState: EventSourcedState[F, VotingData[F], BlockId],
    maxSupportedVersion:      VersionId
  ) extends BlockHeaderVersionValidationAlgebra[F] {

    override def validate(header: BlockHeader): F[Either[BlockHeaderValidationFailure, BlockHeader]] = {
      for {
        _   <- EitherT(validateSupportedVersion(header))
        res <- EitherT(validateExpectedVersion(header))
      } yield res
    }.value

    private def validateSupportedVersion(header: BlockHeader): F[Either[BlockHeaderValidationFailure, BlockHeader]] =
      Either
        .cond[BlockHeaderValidationFailure, BlockHeader](
          header.versionId <= maxSupportedVersion,
          header,
          UnsupportedVersionId(header.versionId, maxSupportedVersion)
        )
        .pure[F]

    private def validateExpectedVersion(header: BlockHeader): F[Either[BlockHeaderValidationFailure, BlockHeader]] =
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
