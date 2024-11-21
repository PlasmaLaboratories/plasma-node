package org.plasmalabs.ledger.interpreters

import cats.data.{NonEmptyChain, Validated, ValidatedNec}
import cats.effect.Sync
import cats.implicits.*
import org.plasmalabs.algebras.ClockAlgebra
import org.plasmalabs.algebras.ClockAlgebra.implicits.*
import org.plasmalabs.consensus.models.BlockId
import org.plasmalabs.ledger.algebras.BodyProposalValidationAlgebra
import org.plasmalabs.ledger.interpreters.ProposalEventSourceState.*
import org.plasmalabs.ledger.models.*
import org.plasmalabs.ledger.models.BodySemanticErrors.*
import org.plasmalabs.models.{Epoch, *}
import org.plasmalabs.node.models.BlockBody
import org.plasmalabs.sdk.models.TransactionId
import org.plasmalabs.sdk.models.box.Value.ConfigProposal
import org.plasmalabs.sdk.models.transaction.IoTransaction
import org.plasmalabs.typeclasses.implicits.*
import org.typelevel.log4cats.Logger

object BodyProposalValidation {

  // scalastyle:off method.length
  def make[F[_]: Sync: Logger](
    clock:              ClockAlgebra[F],
    fetchTransaction:   TransactionId => F[IoTransaction],
    proposalEventState: ProposalEventSourceStateType[F],
    config:             ProposalConfig
  ): F[BodyProposalValidationAlgebra[F]] =
    Sync[F].delay {
      new BodyProposalValidationAlgebra[F] {

        override def validate(
          context: BodyProposalValidationContext
        )(body: BlockBody): F[ValidatedNec[BodyValidationError, BlockBody]] =
          for {
            _            <- Logger[F].info(show"BodyProposalValidation for block ${context.id}")
            transactions <- body.transactionIds.traverse(fetchTransaction(_))
            proposals    <- transactions.proposals.pure[F]
            currentEpoch <- clock.epochOf(context.slot)
            res <-
              if (proposals.isEmpty) {
                Logger[F].info(show"No proposals in block ${context.id}, skip check") >>
                Validated.valid[NonEmptyChain[BodyValidationError], BlockBody](body).pure[F]
              } else {
                checkBodyWithProposals(context.id, body, proposals, currentEpoch)
              }
          } yield res

        def checkBodyWithProposals(
          blockId:      BlockId,
          body:         BlockBody,
          proposals:    List[ConfigProposal],
          currentEpoch: Epoch
        ): F[ValidatedNec[BodyValidationError, BlockBody]] =
          for {
            proposalsIds           <- proposals.map(getProposalId).pure[F]
            doubleProposal         <- checkProposalsWithTheSameIdsInBlock(body, proposalsIds)
            alreadyActiveProposals <- checkProposalsUseInactiveIdsOnly(blockId, body, proposalsIds, currentEpoch)
          } yield doubleProposal.combine(alreadyActiveProposals).map(_.head)

        def checkProposalsWithTheSameIdsInBlock(
          body:         BlockBody,
          proposalsIds: List[ProposalId]
        ): F[ValidatedNec[BodyValidationError, Seq[BlockBody]]] = {
          val res = if (proposalsIds.sizeIs == proposalsIds.toSet.size) {
            Validated.Valid(Seq(body))
          } else {
            Validated.Invalid(NonEmptyChain(DoubleProposalIdTransaction: BodyValidationError))
          }
          res.pure[F]
        }

        def checkProposalsUseInactiveIdsOnly(
          blockId:      BlockId,
          body:         BlockBody,
          proposalIds:  List[ProposalId],
          currentEpoch: Epoch
        ): F[ValidatedNec[BodyValidationError, Seq[BlockBody]]] =
          proposalEventState.useStateAt(blockId) { proposalData =>
            for {
              activeProposalEpoch <- (currentEpoch + proposalDelta).pure[F]
              previousEpochs      <- getPreviousEpochs(proposalData, activeProposalEpoch)
            } yield proposalIds.flatMap(id => checkProposal(id, previousEpochs)) match {
              case Nil          => Validated.Valid(Seq(body))
              case head :: tail => Validated.Invalid(NonEmptyChain(head, tail*))
            }
          }

        private def getPreviousEpochs(
          state:               ProposalData[F],
          activeProposalEpoch: Epoch
        ): F[Map[Epoch, Set[ProposalId]]] =
          Range.Long
            .inclusive(
              Math.max(0, activeProposalEpoch - config.proposalInactiveVotingWindow - config.proposalVotingMaxWindow),
              activeProposalEpoch,
              1
            )
            .toList
            .traverse(epoch =>
              state.epochToCreatedProposalIds
                .get(epoch)
                .map(ids => epoch -> ids.getOrElse(Set.empty[ProposalId]))
            )
            .map(_.toMap)

        private def checkProposal(
          id:             ProposalId,
          previousEpochs: Map[Epoch, Set[ProposalId]]
        ): Option[BodyValidationError] =
          Option.when(previousEpochs.exists(_._2.contains(id)))(ProposalTransactionAlreadyUsedId(id))
      }
    }
  // scalastyle:on method.length
}
