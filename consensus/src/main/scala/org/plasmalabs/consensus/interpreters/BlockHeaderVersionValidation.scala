package org.plasmalabs.consensus.interpreters

import cats.data.*
import cats.effect.kernel.Async
import cats.implicits.*
import org.plasmalabs.algebras.ClockAlgebra
import org.plasmalabs.algebras.ClockAlgebra.implicits.*
import org.plasmalabs.consensus.algebras.*
import org.plasmalabs.consensus.interpreters.CrossEpochEventSourceState.VotingData
import org.plasmalabs.consensus.models.BlockHeaderValidationFailures.*
import org.plasmalabs.consensus.models.{BlockHeader, BlockHeaderValidationFailure, BlockId}
import org.plasmalabs.eventtree.EventSourcedState
import org.plasmalabs.models.VersionId

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
