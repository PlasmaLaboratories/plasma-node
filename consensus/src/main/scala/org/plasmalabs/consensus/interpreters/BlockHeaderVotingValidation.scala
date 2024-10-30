package org.plasmalabs.consensus.interpreters

import cats.data._
import cats.effect.kernel.Async
import cats.implicits._
import org.plasmalabs.algebras.ClockAlgebra.implicits._
import org.plasmalabs.algebras._
import org.plasmalabs.consensus.algebras._
import org.plasmalabs.consensus.interpreters.CrossEpochEventSourceState.VotingData
import org.plasmalabs.consensus.models.BlockHeaderValidationFailures.{IncorrectVotedProposalId, IncorrectVotedVersionId}
import org.plasmalabs.consensus.models.{BlockHeader, BlockHeaderValidationFailure, BlockId}
import org.plasmalabs.eventtree.EventSourcedState
import org.typelevel.log4cats.Logger

object BlockHeaderVotingValidation {

  def make[F[_]: Async: Logger](
    clockAlgebra:             ClockAlgebra[F],
    versionsEventSourceState: EventSourcedState[F, VotingData[F], BlockId]
  ): F[BlockHeaderVotingValidationAlgebra[F]] =
    Async[F].delay(new Impl[F](clockAlgebra, versionsEventSourceState))

  private class Impl[F[_]: Async: Logger](
    clockAlgebra:             ClockAlgebra[F],
    versionsEventSourceState: EventSourcedState[F, VotingData[F], BlockId]
  ) extends BlockHeaderVotingValidationAlgebra[F] {

    override def validate(header: BlockHeader): F[Either[BlockHeaderValidationFailure, BlockHeader]] =
      versionsEventSourceState.useStateAt(header.parentHeaderId) { versionData =>
        {
          for {
            _   <- EitherT(checkVersionVoting(header, versionData))
            res <- EitherT(checkProposalVoting(header, versionData))
          } yield res
        }.value
      }

    private def checkVersionVoting(
      header:      BlockHeader,
      versionData: VotingData[F]
    ): F[Either[BlockHeaderValidationFailure, BlockHeader]] =
      header.getVersionVote match {
        case Some(votedVersion) =>
          for {
            versionCounter       <- versionData.versionCounter.pure[F]
            nextAvailableVersion <- versionCounter.getOrRaise(())
          } yield Either.cond(votedVersion < nextAvailableVersion, header, IncorrectVotedVersionId(votedVersion))
        case None => Either.right[BlockHeaderValidationFailure, BlockHeader](header).pure[F]
      }

    private def checkProposalVoting(
      header:      BlockHeader,
      versionData: VotingData[F]
    ): F[Either[BlockHeaderValidationFailure, BlockHeader]] =
      header.getProposalVote match {
        case Some(votedProposal) =>
          for {
            proposalVoting <- versionData.proposalVoting.pure[F]
            epoch          <- clockAlgebra.epochOf(header.slot)
            votesOpt       <- proposalVoting.get((epoch, votedProposal))
            // we allow blocks which votes for valid proposal in previous epoch but not for current
            prevEpochVotesOpt <- proposalVoting.get((epoch - 1, votedProposal))
            validProposal = votesOpt.isDefined || prevEpochVotesOpt.isDefined
          } yield Either.cond(validProposal, header, IncorrectVotedProposalId(votedProposal))
        case None => Either.right[BlockHeaderValidationFailure, BlockHeader](header).pure[F]
      }
  }
}
