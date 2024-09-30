package xyz.stratalab.ledger.interpreters

import cats.data.ValidatedNec
import cats.effect.IO
import cats.implicits._
import co.topl.brambl.generators.ModelGenerators._
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.box.Value
import co.topl.brambl.models.box.Value.UpdateProposal
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.syntax._
import co.topl.consensus.models.BlockId
import co.topl.node.models.BlockBody
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import xyz.stratalab.algebras.ClockAlgebra
import xyz.stratalab.algebras.testInterpreters.TestStore
import xyz.stratalab.codecs.bytes.tetra.ModelGenerators.arbitraryTxsAndBlock
import xyz.stratalab.eventtree.EventSourcedState
import xyz.stratalab.ledger.interpreters.ProposalEventSourceState.ProposalData
import xyz.stratalab.ledger.models._
import xyz.stratalab.models.ModelGenerators._
import xyz.stratalab.models.generators.consensus.ModelGenerators._
import xyz.stratalab.models.{emptyVersion, proposalDelta, Epoch, ProposalConfig, ProposalId, Slot, Timestamp}

import scala.collection.immutable.NumericRange
import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration

class BodyProposalValidationTest extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {
  implicit val logger: Logger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)

  type F[A] = IO[A]
  type PseudoId = Int

  private val defaultClocks = new ClockAlgebra[F] {
    override def slotLength: F[FiniteDuration] = ???
    override def slotsPerEpoch: F[Long] = 1L.pure[F]
    override def slotsPerOperationalPeriod: F[Long] = ???
    override def currentEpoch: F[Epoch] = ???
    override def globalSlot: F[Slot] = ???
    override def currentTimestamp: F[Timestamp] = ???
    override def forwardBiasedSlotWindow: F[Slot] = ???
    override def timestampToSlot(timestamp:       Timestamp): F[Slot] = ???
    override def slotToTimestamps(slot:           Slot): F[NumericRange.Inclusive[Timestamp]] = ???
    override def delayedUntilSlot(slot:           Slot): F[Unit] = ???
    override def delayedUntilTimestamp(timestamp: Timestamp): F[Unit] = ???
  }

  private val defaultConfig = ProposalConfig(
    proposalVotingMaxWindow = 5,
    proposalVotingWindow = 2,
    proposalInactiveVotingWindow = 1,
    updateProposalPercentage = 0.2,
    versionVotingWindow = 2,
    versionSwitchWindow = 2,
    updateVersionPercentage = 0.9
  )

  private def buildProposalData(
    epochToCreatedProposalIdsData: Seq[(Epoch, Set[PseudoId])] = Seq.empty
  ): F[ProposalData[F]] =
    for {
      pseudoIds    <- epochToCreatedProposalIdsData.flatMap { case (_, ids) => ids }.pure[F]
      idToProposal <- TestStore.make[F, ProposalId, UpdateProposal]
      _ <- pseudoIds.traverse { id =>
        idToProposal.put(getProposalIdByPseudoId(id), getProposalByPseudoId(id))
      }

      epochToCreatedProposalIds <- TestStore.make[F, Epoch, Set[ProposalId]]
      _ <- epochToCreatedProposalIdsData.traverse { case (epoch, pseudoIds) =>
        val realIds = pseudoIds.map(getProposalIdByPseudoId)
        epochToCreatedProposalIds.put(epoch, realIds)
      }
    } yield ProposalData(idToProposal, epochToCreatedProposalIds)

  private def buildData(pseudoProposalIds: Seq[PseudoId], blockEpoch: Epoch) = {
    val (txs, body) = arbitraryTxsAndBlock.arbitrary.first
    val proposalsTxs = pseudoProposalIds.map(txWithProposal)
    val transactionsStore = mutable.Map.empty[TransactionId, IoTransaction]

    val transactionsWithProposal = txs ++ proposalsTxs
    val bodyWithProposal = body.copy(transactionIds = transactionsWithProposal.map(_.id))
    transactionsWithProposal.foreach(tx => transactionsStore.put(tx.id, tx))

    val blockId = arbitraryBlockId.arbitrary.first
    val bodyProposalValidationContext = BodyProposalValidationContext(blockId, blockEpoch + 1)
    (transactionsStore, bodyWithProposal, bodyProposalValidationContext)
  }

  def txWithProposal(pseudoProposalId: Int): IoTransaction = {
    val valueValueProposal: co.topl.brambl.models.box.Value.Value =
      Value.Value.UpdateProposal(getProposalByPseudoId(pseudoProposalId))
    val value: co.topl.brambl.models.box.Value = new co.topl.brambl.models.box.Value(value = valueValueProposal)
    val unspentOutputWithProposal = arbitraryUnspentTransactionOutput.arbitrary.first.copy(value = value)
    val transaction = arbitraryIoTransaction.arbitrary.first
    transaction.copy(outputs = transaction.outputs :+ unspentOutputWithProposal)
  }

  val pseudoIdToProposal: mutable.Map[Int, UpdateProposal] = mutable.Map.empty

  def getProposalByPseudoId(pseudoId: Int): UpdateProposal =
    pseudoIdToProposal.getOrElseUpdate(pseudoId, UpdateProposal(label = pseudoId.toString))

  val pseudoIdToProposalId: mutable.Map[Int, Int] = mutable.Map.empty

  def getProposalIdByPseudoId(pseudoId: Int): Int =
    if (pseudoId == 0) {
      emptyVersion
    } else {
      pseudoIdToProposalId.getOrElseUpdate(
        pseudoId,
        ProposalEventSourceState.getProposalId(getProposalByPseudoId(pseudoId))
      )
    }

  private def getValidator(proposalData: ProposalData[F], txs: mutable.Map[TransactionId, IoTransaction]) = {
    val eventSource = mock[EventSourcedState[F, ProposalData[F], BlockId]]
    (eventSource
      .useStateAt(_: BlockId)(_: ProposalData[F] => F[ValidatedNec[BodyValidationError, BlockBody]]))
      .expects(*, *)
      .once()
      .onCall { case (_: BlockId, fun: (ProposalData[F] => F[ValidatedNec[BodyValidationError, BlockBody]])) =>
        fun(proposalData)
      }

    for {
      validation <- BodyProposalValidation.make(
        defaultClocks,
        id => txs(id).pure[F],
        eventSource,
        defaultConfig
      )
    } yield validation
  }

  test("Block is valid if no proposals exist") {
    withMock {
      for {
        proposalData     <- buildProposalData()
        (txs, body, ctx) <- buildData(Seq(1), 5).pure[F]
        validation       <- getValidator(proposalData, txs)
        res1             <- validation.validate(ctx)(body)
        _                <- assert(res1.isValid).pure[F]
      } yield ()
    }
  }

  test("Block is not valid if previous proposal inactive window is not expired") {
    withMock {
      for {
        proposalId <- 1.pure[F]
        epoch      <- 20L.pure[F]
        proposalInEpoch =
          epoch + proposalDelta - defaultConfig.proposalVotingMaxWindow - defaultConfig.proposalInactiveVotingWindow
        proposalData     <- buildProposalData(Seq(proposalInEpoch -> Set(proposalId)))
        (txs, body, ctx) <- buildData(Seq(proposalId), epoch).pure[F]
        validation       <- getValidator(proposalData, txs)
        res1             <- validation.validate(ctx)(body)
        _                <- assert(res1.isInvalid).pure[F]
      } yield ()
    }
  }

  test("Block is not valid if previous proposal created in the same epoch") {
    withMock {
      for {
        proposalId <- 1.pure[F]
        epoch      <- 20L.pure[F]
        proposalInEpoch = epoch
        proposalData     <- buildProposalData(Seq(proposalInEpoch -> Set(proposalId)))
        (txs, body, ctx) <- buildData(Seq(proposalId), epoch).pure[F]
        validation       <- getValidator(proposalData, txs)
        res1             <- validation.validate(ctx)(body)
        _                <- assert(res1.isInvalid).pure[F]
      } yield ()
    }
  }

  test("Block is not valid if two proposals with the same id in the same block") {
    withMock {
      for {
        proposalId       <- 1.pure[F]
        epoch            <- 20L.pure[F]
        proposalData     <- buildProposalData()
        (txs, body, ctx) <- buildData(Seq(proposalId, proposalId), epoch).pure[F]
        validation       <- getValidator(proposalData, txs)
        res1             <- validation.validate(ctx)(body)
        _                <- assert(res1.isInvalid).pure[F]
      } yield ()
    }
  }
}
