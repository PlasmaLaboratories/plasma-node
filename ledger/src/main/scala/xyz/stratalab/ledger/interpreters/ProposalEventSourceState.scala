package org.plasmalabs.ledger.interpreters

import cats._
import cats.effect.Async
import cats.implicits._
import org.typelevel.log4cats.Logger
import org.plasmalabs.algebras.ClockAlgebra.implicits._
import org.plasmalabs.algebras.StoreOps._
import org.plasmalabs.algebras._
import org.plasmalabs.consensus.models.{BlockHeader, BlockId}
import org.plasmalabs.crypto.hash.Blake2b256
import org.plasmalabs.eventtree.{EventSourcedState, ParentChildTree}
import org.plasmalabs.models._
import org.plasmalabs.node.models._
import org.plasmalabs.sdk.models.TransactionId
import org.plasmalabs.sdk.models.box.Value.ConfigProposal
import org.plasmalabs.sdk.models.transaction.IoTransaction
import org.plasmalabs.typeclasses.implicits._

import java.nio.ByteBuffer

object ProposalEventSourceState {
  type ProposalEventSourceStateType[F[_]] = EventSourcedState[F, ProposalData[F], BlockId]

  case class ProposalData[F[_]](
    idToProposal:              Store[F, ProposalId, ConfigProposal],
    epochToCreatedProposalIds: Store[F, Epoch, Set[ProposalId]]
  )

  def getProposalId(proposal: ConfigProposal): ProposalId =
    Math.abs(ByteBuffer.wrap(new Blake2b256().hash(proposal.toByteArray)).getInt)

  def make[F[_]: Async: Logger](
    currentBlockId:      F[BlockId],
    parentChildTree:     ParentChildTree[F, BlockId],
    currentEventChanged: BlockId => F[Unit],
    initialState:        F[ProposalData[F]],
    clock:               ClockAlgebra[F],
    fetchHeader:         BlockId => F[BlockHeader],
    fetchBlockBody:      BlockId => F[BlockBody],
    fetchTransaction:    TransactionId => F[IoTransaction],
    config:              ProposalConfig
  ): F[ProposalEventSourceStateType[F]] =
    EventSourcedState.OfTree.make(
      initialState = initialState,
      initialEventId = currentBlockId,
      applyEvent = new ApplyBlock(clock, fetchHeader, fetchBlockBody, fetchTransaction, config),
      unapplyEvent = new UnapplyBlock(clock, fetchHeader, fetchBlockBody, fetchTransaction),
      parentChildTree = parentChildTree,
      currentEventChanged
    )

  private class ApplyBlock[F[_]: MonadThrow: Logger](
    clock:            ClockAlgebra[F],
    fetchBlockHeader: BlockId => F[BlockHeader],
    fetchBlockBody:   BlockId => F[BlockBody],
    fetchTransaction: TransactionId => F[IoTransaction],
    config:           ProposalConfig
  ) extends ((ProposalData[F], BlockId) => F[ProposalData[F]]) {

    def apply(state: ProposalData[F], blockId: BlockId): F[ProposalData[F]] =
      for {
        header       <- fetchBlockHeader(blockId)
        currentEpoch <- clock.epochOf(header.slot)
        _            <- Logger[F].debug(show"Apply block with proposals $blockId of epoch $currentEpoch")
        _            <- applyNewProposals(state, blockId, currentEpoch)
      } yield state

    private def applyNewProposals(state: ProposalData[F], blockId: BlockId, currentEpoch: Epoch): F[Unit] =
      for {
        blockTxs            <- fetchBlockBody(blockId)
        transactions        <- blockTxs.transactionIds.traverse(fetchTransaction(_))
        proposals           <- transactions.proposals.pure[F]
        activeProposalEpoch <- (currentEpoch + proposalDelta).pure[F]
        previousEpochs      <- getPreviousEpochs(state, activeProposalEpoch)
        _ <- proposals.traverse(proposal => applyNewProposal(state, activeProposalEpoch, proposal, previousEpochs))
      } yield ()

    private def getPreviousEpochs(state: ProposalData[F], activeProposalEpoch: Epoch): F[Map[Epoch, Set[ProposalId]]] =
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

    private def applyNewProposal(
      state:               ProposalData[F],
      activeProposalEpoch: Epoch,
      configProposal:      ConfigProposal,
      previousEpochs:      Map[Epoch, Set[ProposalId]]
    ): F[Unit] = {
      val id = getProposalId(configProposal)

      // Check that there is no active other proposal with the same id within proposalInactiveVotingWindow
      val sameProposalIdError =
        new IllegalStateException(show"Received proposal with the same id $id within ${previousEpochs.keySet}")
      MonadThrow[F].ensure(().pure[F])(sameProposalIdError)(_ => !previousEpochs.exists(_._2.contains(id))) >>
      Logger[F].info(show"Received new proposal with id $id active starting from $activeProposalEpoch epoch") >>
      Logger[F].info(show"Proposal id $id with data $configProposal") >>
      state.idToProposal.put(id, configProposal) >>
      state.epochToCreatedProposalIds.addIdToEpoch(activeProposalEpoch, id)
    }
  }

  private class UnapplyBlock[F[_]: MonadThrow: Logger](
    clock:            ClockAlgebra[F],
    fetchBlockHeader: BlockId => F[BlockHeader],
    fetchBlockBody:   BlockId => F[BlockBody],
    fetchTransaction: TransactionId => F[IoTransaction]
  ) extends ((ProposalData[F], BlockId) => F[ProposalData[F]]) {

    def apply(state: ProposalData[F], blockId: BlockId): F[ProposalData[F]] =
      for {
        header              <- fetchBlockHeader(blockId)
        currentEpoch        <- clock.epochOf(header.slot)
        _                   <- Logger[F].debug(show"Unapply block with proposals $blockId of epoch $currentEpoch")
        activeProposalEpoch <- (currentEpoch + proposalDelta).pure[F]
        _                   <- unapplyNewProposals(state, blockId, activeProposalEpoch)
        _                   <- state.epochToCreatedProposalIds.remove(activeProposalEpoch)
      } yield state

    private def unapplyNewProposals(state: ProposalData[F], blockId: BlockId, activeProposalEpoch: Epoch): F[Unit] =
      for {
        blockTxs           <- fetchBlockBody(blockId)
        transactions       <- blockTxs.transactionIds.traverse(fetchTransaction(_))
        proposals          <- transactions.proposals.pure[F]
        idToProposalUpdate <- proposals.map(p => getProposalId(p) -> p).pure[F]
        _ <- idToProposalUpdate.traverse { case (id, _) =>
          Logger[F].debug(show"Remove proposal with id $id") >>
          state.idToProposal.remove(id) >>
          state.epochToCreatedProposalIds.removeIdFromEpoch(activeProposalEpoch, id)
        }
      } yield ()
  }

  implicit val configProposalShow: Show[ConfigProposal] = proposal => proposal.toString
}
