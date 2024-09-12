package co.topl.consensus.interpreters

import co.topl.consensus.algebras._
import co.topl.consensus.models.{BlockHeader, BlockHeaderValidationFailure, BlockId}
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.algebras._
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.consensus.interpreters.VotingEventSourceState.VotingData
import co.topl.consensus.models.BlockHeaderValidationFailures._
import co.topl.eventtree.EventSourcedState
import org.typelevel.log4cats.Logger
import cats.data._

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
          } yield Either.cond(votesOpt.isDefined, header, IncorrectVotedProposalId(votedProposal))
        case None => Either.right[BlockHeaderValidationFailure, BlockHeader](header).pure[F]
      }
  }
}
