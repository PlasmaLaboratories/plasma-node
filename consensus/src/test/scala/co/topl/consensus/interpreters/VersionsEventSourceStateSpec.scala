package co.topl.consensus.interpreters

import cats.effect.Async
import cats.implicits._
import cats.effect.IO
import co.topl.consensus.models._
import co.topl.eventtree.EventSourcedState
import co.topl.models.generators.consensus.ModelGenerators.arbitraryHeader
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory
import co.topl.algebras.{ClockAlgebra, Store}
import co.topl.models.{ProposalId, Slot, Timestamp, VersionId}
import co.topl.algebras.testInterpreters.TestStore
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.box.Value.UpdateProposal
import co.topl.brambl.models.transaction._
import co.topl.proto.node.EpochData
import co.topl.models.{Epoch, _}

import scala.collection.mutable
import scala.collection.immutable.NumericRange
import scala.concurrent.duration.FiniteDuration
import co.topl.eventtree.ParentChildTree
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.tetra.ModelGenerators._
import co.topl.node.models._
import co.topl.brambl.generators.TransactionGenerator
import co.topl.brambl.models.box.Value
import co.topl.models.ModelGenerators._
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.consensus.algebras.VersionInfoAlgebra
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import co.topl.numerics.implicits._
import co.topl.typeclasses.implicits._

class VersionsEventSourceStateSpec
    extends CatsEffectSuite
    with ScalaCheckEffectSuite
    with AsyncMockFactory
    with TransactionGenerator {
  type F[A] = IO[A]
  implicit val logger: Logger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)

  import VersionsEventSourceStateSpec._

  private val epochLen = 10L
  private val initialFreeVersion = 1

  private val defaultConfig = ProposalConfig(
    proposalVotingMaxWindow = 5,
    proposalVotingWindow = 2,
    proposalInactiveVotingWindow = 1,
    updateProposalPercentage = 0.2,
    versionVotingWindow = 2,
    versionSwitchWindow = 2,
    updateVersionPercentage = 0.9
  )

  private val defaultClocks = new ClockAlgebra[F] {
    override def slotLength: F[FiniteDuration] = ???
    override def slotsPerEpoch: F[Long] = epochLen.pure[F]
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

  private def makeInitialVersionsData[F[_]: Async](): F[VersionsEventSourceState.VersionsData[F]] =
    for {
      idToProposal             <- TestStore.make[F, ProposalId, UpdateProposal]
      epochToProposalIds       <- TestStore.make[F, Epoch, Set[ProposalId]]
      proposalVoting           <- TestStore.make[F, (Epoch, ProposalId), Long]
      epochToCreatedVersionIds <- TestStore.make[F, Epoch, Set[VersionId]]
      epochToVersionIds        <- TestStore.make[F, Epoch, Set[VersionId]]
      versionIdToProposal      <- TestStore.make[F, VersionId, UpdateProposal]
      versionCounter           <- TestStore.make[F, Unit, VersionId]
      versionVoting            <- TestStore.make[F, (Epoch, VersionId), Long]
      _                        <- versionCounter.put((), initialFreeVersion)
    } yield VersionsEventSourceState.VersionsData(
      idToProposal,
      epochToProposalIds,
      proposalVoting,
      epochToCreatedVersionIds,
      epochToVersionIds,
      versionIdToProposal,
      versionCounter,
      versionVoting,
      makeEpochDataStore[F]
    )

  private def makeEpochDataStore[F[_]: Async]: Store[F, Epoch, EpochData] = new Store[F, Epoch, EpochData] {

    override def get(id: Epoch): F[Option[EpochData]] =
      EpochData(
        startHeight = id * 10,
        endHeight = 9 + id * 10,
        totalTransactionReward = 1,
        activeStake = 0,
        inactiveStake = 0
      ).some.pure[F]

    override def contains(id: Epoch): F[Boolean] = ???

    override def put(id: Epoch, t: EpochData): F[Unit] = ???

    override def remove(id: Epoch): F[Unit] = ???

    override def getAll(): F[Seq[(Epoch, EpochData)]] = ???
  }

  private def makeParentTree[F[_]: Async](chain: Seq[BlockHeader]): F[ParentChildTree[F, BlockId]] =
    for {
      tree <- ParentChildTree.FromRef.make[F, BlockId]
      _    <- chain.traverse(sd => tree.associate(sd.id, sd.parentHeaderId))
    } yield tree

  private val eventChangedStub: BlockId => F[Unit] = _ => ().pure[F]

  case class BlocksStorages(
    headers: mutable.Map[BlockId, BlockHeader],
    body:    mutable.Map[BlockId, BlockBody],
    tx:      mutable.Map[TransactionId, IoTransaction]
  )

  private def makeBlocksStorages(
    chain:        Seq[BlockHeader],
    proposalsMap: Map[BlockId, Seq[UpdateProposal]]
  ): BlocksStorages = {
    val blocksStorages = BlocksStorages(mutable.Map.empty, mutable.Map.empty, mutable.Map.empty)

    val idToHeader = chain.map(h => h.id -> h)
    idToHeader.foreach { case (id, header) =>
      blocksStorages.headers.put(id, header)
      val (txs, body) = arbitraryTxsAndBlock.arbitrary.first
      blocksStorages.body.put(id, body)
      txs.foreach(tx => blocksStorages.tx.put(tx.id, tx))
    }

    idToHeader.foreach { case (id, _) =>
      proposalsMap
        .get(id)
        .foreach(proposals => proposals.foreach(proposal => addProposalToBlock(id, proposal, blocksStorages)))
    }

    blocksStorages
  }

  private def addProposalToBlock(blockId: BlockId, proposal: UpdateProposal, storages: BlocksStorages): Unit = {
    val valueValueProposal: co.topl.brambl.models.box.Value.Value = Value.Value.UpdateProposal(proposal)
    val value: co.topl.brambl.models.box.Value = new co.topl.brambl.models.box.Value(value = valueValueProposal)
    val unspentOutputWithProposal = arbitraryUnspentTransactionOutput.arbitrary.first.copy(value = value)
    val transaction = arbitraryIoTransaction.arbitrary.first
    val transactionWithProposal = transaction.copy(outputs = transaction.outputs :+ unspentOutputWithProposal)

    val (transactions, body) =
      storages.body
        .get(blockId)
        .fold(arbitraryTxsAndBlock.arbitrary.first)(body => body.transactionIds.flatMap(storages.tx.get) -> body)

    val transactionsWithProposal = transactions.appended(transactionWithProposal)
    val bodyWithProposal = body.copy(transactionIds = transactionsWithProposal.map(_.id))

    transactionsWithProposal.foreach(tx => storages.tx.put(tx.id, tx))
    storages.body.put(blockId, bodyWithProposal)
  }

  val noPlan: (Seq[Int], Int, Int) = (Seq.empty, 0, 0)
  val noPlanForEpoch: Seq[(Seq[Int], Int, Int)] = Range(0, epochLen.toInt).map(_ => noPlan)

  // For list:
  // position in list -- position in chain
  // first argument -- proposal pseudo id in particular block where
  //   real id = VersionsEventSourceState.getProposalId(UpdateProposal(label = pseudoId.toString))
  // second argument -- voted version in block
  // third argument -- voted proposal id in block
  def makeData(plan: List[(Seq[Int], Int, Int)]): (Seq[BlockHeader], BlocksStorages) = {
    val rootBlock: BlockHeader = blockWithVersions(arbitraryHeader.arbitrary.first, -1, 0, 0, 0)
    val initialAcc = Seq.empty[(BlockHeader, Seq[UpdateProposal])]
    val blockChainWithProposals = plan.zipWithIndex.foldLeft(initialAcc) {
      case (acc, ((proposals, versionVote, proposalVote), index)) =>
        val parent = acc.lastOption.fold(rootBlock)(_._1)
        val header = blockWithVersions(parent, index, 0, versionVote, getProposalIdByPseudoId(proposalVote))
        val updateProposals = proposals.map(getProposalByPseudoId)
        acc :+ (header, updateProposals)
    }

    val chain = blockChainWithProposals.map(_._1)
    val proposalsMap = blockChainWithProposals.map { case (header, proposals) => header.id -> proposals }.toMap
    val storages = makeBlocksStorages(chain, proposalsMap)
    (chain, storages)
  }

  def blockWithVersions(
    parentBlockHeader: BlockHeader,
    index:             Int,
    version:           Int,
    versionVote:       Int,
    ProposalVote:      Int
  ): BlockHeader =
    arbitraryHeader.arbitrary
      .map { h =>
        h.copy(
          parentHeaderId = parentBlockHeader.id,
          parentSlot = parentBlockHeader.slot,
          version = ProtocolVersion(version, versionVote, ProposalVote),
          height = index,
          slot = 1 + index
        )
      }
      .first
      .embedId

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
        VersionsEventSourceState.getProposalId(getProposalByPseudoId(pseudoId))
      )
    }

  def createDefaultVersionEventsAndChainFromPlan(plan: List[(Seq[Int], Int, Int)]): F[
    (
      EventSourcedState[F, VersionsEventSourceState.VersionsData[F], BlockId],
      VersionsEventSourceState.VersionsData[F],
      Seq[BlockHeader],
      TestStore[F, Epoch, VersionId]
    )
  ] = {
    val (headers, storages) = makeData(plan)

    for {
      parentTree     <- makeParentTree[F](headers)
      versionsData   <- makeInitialVersionsData[F]()
      versionStorage <- TestStore.make[F, Epoch, VersionId]
      versionAlgebra: VersionInfoAlgebra[F] = new VersionInfoAlgebra[F] {

        override def addVersionStartEpoch(epoch: Epoch, version: VersionId): F[Unit] =
          versionStorage.put(epoch, version)

        override def removeVersionStartEpoch(epoch: Epoch): F[Unit] = versionStorage.remove(epoch)

        override def getVersionForEpoch(epoch: Epoch): F[VersionId] = ???
      }
      initialState <- VersionsEventSourceState.make[F](
        headers.head.parentHeaderId.pure[F],
        parentTree,
        eventChangedStub,
        versionsData.pure[F],
        defaultClocks,
        id => storages.headers(id).pure[F],
        id => storages.body(id).pure[F],
        id => storages.tx(id).pure[F],
        versionAlgebra,
        defaultConfig
      )
    } yield (initialState, versionsData, headers, versionStorage)
  }

  test("Add proposal and proposal vote in the same block") {
    withMock {
      val proposalPseudoId1 = 1
      val plan: List[(Seq[Int], Int, Int)] = List((Seq(proposalPseudoId1), 0, proposalPseudoId1))

      for {
        (initialState, versionsData, headers, _) <- createDefaultVersionEventsAndChainFromPlan(plan)
        initialData                              <- versionsData.makeCopy

        _             <- initialState.stateAt(headers.head.id)
        versionOfHead <- versionsData.versionCounter.getOrRaise(())
        _             <- assert(versionOfHead == initialFreeVersion).pure[F]
        epochOfHead   <- defaultClocks.epochOf(headers.head.slot)
        epochToIds    <- versionsData.epochToProposalIds.getOrRaise(epochOfHead)
        _             <- assert(epochToIds.size == 1).pure[F]
        proposalRealId = getProposalIdByPseudoId(proposalPseudoId1)
        _            <- assert(epochToIds.head == proposalRealId).pure[F]
        dataAtHeader <- versionsData.makeCopy
        _            <- assert(dataAtHeader.proposalVotes((epochOfHead, proposalRealId)) == 1).pure[F]
        _            <- assert(initialData != dataAtHeader).pure[F]

        _               <- initialState.stateAt(headers.head.parentHeaderId)
        epochToIds2     <- versionsData.epochToProposalIds.get(epochOfHead)
        _               <- assert(epochToIds2.isEmpty).pure[F]
        dataAtPreHeader <- versionsData.makeCopy
        _               <- assert(initialData == dataAtPreHeader).pure[F]
      } yield ()
    }
  }

  test("Two proposals in the same block") {
    withMock {
      val p1 = 1
      val p2 = 2
      val plan: List[(Seq[Int], Int, Int)] = List((Seq(p1, p2), 0, 0))

      for {
        (initialState, versionsData, headers, _) <- createDefaultVersionEventsAndChainFromPlan(plan)
        initialData                              <- versionsData.makeCopy

        _             <- initialState.stateAt(headers.head.id)
        versionOfHead <- versionsData.versionCounter.getOrRaise(())
        _             <- assert(versionOfHead == initialFreeVersion).pure[F]
        epochOfHead   <- defaultClocks.epochOf(headers.head.slot)
        epochToIds    <- versionsData.epochToProposalIds.getOrRaise(epochOfHead)
        _             <- assert(epochToIds.size == 2).pure[F]
        proposalRealId1 = getProposalIdByPseudoId(p1)
        proposalRealId2 = getProposalIdByPseudoId(p2)
        expectedIds = Seq(proposalRealId1, proposalRealId2)
        _            <- assert(expectedIds.forall(epochToIds.contains)).pure[F]
        dataAtHeader <- versionsData.makeCopy
        _            <- assert(initialData != dataAtHeader).pure[F]

        _               <- initialState.stateAt(headers.head.parentHeaderId)
        epochToIds2     <- versionsData.epochToProposalIds.get(epochOfHead)
        _               <- assert(epochToIds2.isEmpty).pure[F]
        dataAtPreHeader <- versionsData.makeCopy
        _               <- assert(initialData == dataAtPreHeader).pure[F]
      } yield ()
    }
  }

  test("Proposal exist during proposalVotingMaxWindow epochs, vote exist only in voted epoch") {
    withMock {
      val proposalPseudoId1 = 1
      val plan: List[(Seq[Int], Int, Int)] = List(
        (Seq(proposalPseudoId1), 0, proposalPseudoId1),
        noPlan,
        noPlan,
        noPlan,
        noPlan,
        noPlan,
        noPlan,
        noPlan,
        noPlan,
        noPlan
      ) ++ noPlanForEpoch ++ noPlanForEpoch ++ noPlanForEpoch ++ noPlanForEpoch :+ noPlan

      for {
        (initialState, versionsData, headers, _) <- createDefaultVersionEventsAndChainFromPlan(plan)
        initialData                              <- versionsData.makeCopy

        atHeader1 = headers.init.last
        _             <- initialState.stateAt(atHeader1.id)
        versionOfHead <- versionsData.versionCounter.getOrRaise(())
        _             <- assert(versionOfHead == initialFreeVersion).pure[F]
        header1Epoch  <- defaultClocks.epochOf(atHeader1.slot)
        epochToIds    <- versionsData.epochToProposalIds.getOrRaise(header1Epoch)
        _             <- assert(epochToIds.size == 1).pure[F]
        proposalRealId = getProposalIdByPseudoId(proposalPseudoId1)
        _             <- assert(epochToIds.head == proposalRealId).pure[F]
        dataAtHeader1 <- versionsData.makeCopy
        _             <- assert(dataAtHeader1.proposalVotes((header1Epoch, proposalRealId)) == 0).pure[F]
        _             <- assert(initialData != dataAtHeader1).pure[F]

        _ <- initialState.stateAt(headers(7).id)

        atHeader2 = headers.last
        _             <- initialState.stateAt(atHeader2.id)
        versionOfHead <- versionsData.versionCounter.getOrRaise(())
        _             <- assert(versionOfHead == initialFreeVersion).pure[F]
        header2Epoch  <- defaultClocks.epochOf(atHeader2.slot)
        epochToIds    <- versionsData.epochToProposalIds.get(header2Epoch)
        _             <- assert(epochToIds.isEmpty).pure[F]
        dataAtHeader2 <- versionsData.makeCopy
        _             <- assert(!dataAtHeader2.proposalVotes.contains((header2Epoch, proposalRealId))).pure[F]
        _             <- assert(initialData != dataAtHeader2).pure[F]

        _               <- initialState.stateAt(headers.head.parentHeaderId)
        dataAtPreHeader <- versionsData.makeCopy
        _               <- assert(initialData == dataAtPreHeader).pure[F]
      } yield ()
    }
  }

  test("Make version from voted proposal") {
    withMock {
      val proposalPseudoId1 = 1
      val plan: List[(Seq[Int], Int, Int)] = List(
        (Seq(proposalPseudoId1), 0, proposalPseudoId1),
        noPlan,
        noPlan,
        noPlan,
        noPlan,
        noPlan,
        noPlan,
        noPlan,
        noPlan,
        noPlan
      ) ++
        List(
          (Seq(), 0, proposalPseudoId1),
          noPlan,
          noPlan,
          noPlan,
          noPlan,
          noPlan,
          noPlan,
          noPlan,
          noPlan,
          noPlan
        ) ++
        noPlanForEpoch ++
        List(
          (Seq(), 0, proposalPseudoId1),
          noPlan,
          noPlan,
          noPlan,
          noPlan,
          noPlan,
          noPlan,
          noPlan,
          noPlan,
          (Seq(), 0, proposalPseudoId1)
        ) ++
        List(
          (Seq(), 0, proposalPseudoId1),
          noPlan,
          noPlan,
          noPlan,
          noPlan,
          noPlan,
          noPlan,
          noPlan,
          noPlan,
          (Seq(), 0, proposalPseudoId1)
        ) :+ noPlan

      for {
        (initialState, versionsData, headers, _) <- createDefaultVersionEventsAndChainFromPlan(plan)
        initialData                              <- versionsData.makeCopy

        atHeader1 = headers.init.last
        _             <- initialState.stateAt(atHeader1.id)
        versionOfHead <- versionsData.versionCounter.getOrRaise(())
        _             <- assert(versionOfHead == initialFreeVersion).pure[F]
        header1Epoch  <- defaultClocks.epochOf(atHeader1.slot)
        epochToIds    <- versionsData.epochToProposalIds.getOrRaise(header1Epoch)
        _             <- assert(epochToIds.size == 1).pure[F]
        proposalRealId = getProposalIdByPseudoId(proposalPseudoId1)
        _             <- assert(epochToIds.head == proposalRealId).pure[F]
        dataAtHeader1 <- versionsData.makeCopy
        _             <- assert(dataAtHeader1.proposalVotes((header1Epoch, proposalRealId)) == 2).pure[F]
        _             <- assert(initialData != dataAtHeader1).pure[F]

        _ <- initialState.stateAt(headers(3).id)

        atHeader2 = headers.last
        _             <- initialState.stateAt(atHeader2.id)
        versionOfHead <- versionsData.versionCounter.getOrRaise(())
        _             <- assert(versionOfHead == initialFreeVersion + 1).pure[F]
        header2Epoch  <- defaultClocks.epochOf(atHeader2.slot)
        epochToIds    <- versionsData.epochToProposalIds.get(header2Epoch)
        _             <- assert(epochToIds.isEmpty).pure[F]
        versions      <- versionsData.epochToCreatedVersionIds.getOrRaise(header2Epoch)
        _             <- assert(versions.head == initialFreeVersion).pure[F]

        dataAtHeader2 <- versionsData.makeCopy
        _             <- assert(!dataAtHeader2.proposalVotes.contains((header2Epoch, proposalRealId))).pure[F]
        _             <- assert(initialData != dataAtHeader2).pure[F]

        _               <- initialState.stateAt(headers(7).id)
        _               <- initialState.stateAt(headers.head.parentHeaderId)
        dataAtPreHeader <- versionsData.makeCopy
        _               <- assert(initialData == dataAtPreHeader).pure[F]
      } yield ()
    }
  }

  test("Make version from voted proposal, vote version to add version starts information") {
    withMock {
      val proposalPseudoId1 = 1
      val plan: List[(Seq[Int], Int, Int)] = List(
        (Seq(proposalPseudoId1), 0, proposalPseudoId1),
        noPlan,
        noPlan,
        noPlan,
        noPlan,
        (Seq(), 0, proposalPseudoId1),
        noPlan,
        noPlan,
        noPlan,
        (Seq(), 0, proposalPseudoId1)
      ) ++
        List(
          (Seq(), 0, proposalPseudoId1),
          noPlan,
          noPlan,
          (Seq(), 0, proposalPseudoId1),
          noPlan,
          noPlan,
          noPlan,
          noPlan,
          noPlan,
          noPlan
        ) ++
        List(
          (Seq(), initialFreeVersion, 0),
          (Seq(), initialFreeVersion, 0),
          (Seq(), initialFreeVersion, 0),
          (Seq(), initialFreeVersion, 0),
          (Seq(), initialFreeVersion, 0),
          (Seq(), initialFreeVersion, 0),
          (Seq(), initialFreeVersion, 0),
          noPlan,
          noPlan,
          noPlan
        ) ++
        List(
          (Seq(), initialFreeVersion, 0),
          (Seq(), initialFreeVersion, 0),
          (Seq(), initialFreeVersion, 0),
          (Seq(), initialFreeVersion, 0),
          (Seq(), initialFreeVersion, 0),
          (Seq(), initialFreeVersion, 0),
          (Seq(), initialFreeVersion, 0),
          (Seq(), initialFreeVersion, 0),
          (Seq(), initialFreeVersion, 0),
          noPlan
        ) ++
        List(
          (Seq(), initialFreeVersion, 0),
          (Seq(), initialFreeVersion, 0),
          (Seq(), initialFreeVersion, 0),
          (Seq(), initialFreeVersion, 0),
          (Seq(), initialFreeVersion, 0),
          (Seq(), initialFreeVersion, 0),
          (Seq(), initialFreeVersion, 0),
          (Seq(), initialFreeVersion, 0),
          (Seq(), initialFreeVersion, 0),
          (Seq(), initialFreeVersion, 0)
        ) :+ noPlan

      for {
        (initialState, versionsData, headers, epochToVersionStore) <- createDefaultVersionEventsAndChainFromPlan(plan)
        initialData                                                <- versionsData.makeCopy
        initialEpochVersion                                        <- epochToVersionStore.copyData

        atHeader1 = headers(epochLen.toInt * 1)
        _             <- initialState.stateAt(atHeader1.id)
        versionOfHead <- versionsData.versionCounter.getOrRaise(())
        _             <- assert(versionOfHead == initialFreeVersion).pure[F]
        header1Epoch  <- defaultClocks.epochOf(atHeader1.slot)
        epochToIds    <- versionsData.epochToProposalIds.getOrRaise(header1Epoch)
        _             <- assert(epochToIds.head == getProposalIdByPseudoId(proposalPseudoId1)).pure[F]
        dataAtHeader1 <- versionsData.makeCopy
        epochToVer1   <- epochToVersionStore.copyData
        _             <- assert(initialData != dataAtHeader1).pure[F]

        newVersion = initialFreeVersion
        atHeader2 = headers(epochLen.toInt * 2)
        _              <- initialState.stateAt(atHeader2.id)
        versionOfHead2 <- versionsData.versionCounter.getOrRaise(())
        _              <- assert(versionOfHead2 == initialFreeVersion + 1).pure[F]
        header2Epoch   <- defaultClocks.epochOf(atHeader2.slot)
        epochToIds2    <- versionsData.epochToProposalIds.get(header2Epoch)
        _              <- assert(epochToIds2.isEmpty).pure[F]
        dataAtHeader2  <- versionsData.makeCopy
        _ <- assert(dataAtHeader2.versionIdToProposal(newVersion) == getProposalByPseudoId(proposalPseudoId1))
          .pure[F]
        _           <- assert(dataAtHeader2.epochToCreatedVersionIds(header2Epoch).head == newVersion).pure[F]
        epochToVer2 <- epochToVersionStore.copyData
        _           <- assert(initialData != dataAtHeader2).pure[F]
        _           <- assert(epochToVer1 == epochToVer2).pure[F]

        atHeader3 = headers(epochLen.toInt * 4)
        _              <- initialState.stateAt(atHeader3.id)
        versionOfHead3 <- versionsData.versionCounter.getOrRaise(())
        _              <- assert(versionOfHead3 == initialFreeVersion + 1).pure[F]
        header3Epoch   <- defaultClocks.epochOf(atHeader3.slot)
        epochToIds3    <- versionsData.epochToProposalIds.get(header3Epoch)
        _              <- assert(epochToIds3.isEmpty).pure[F]
        dataAtHeader3  <- versionsData.makeCopy
        epochToIds3    <- versionsData.epochToProposalIds.get(header3Epoch)
        _              <- assert(epochToIds3.isEmpty).pure[F]
        epochToVer3    <- epochToVersionStore.copyData
        _              <- assert(dataAtHeader3.epochToCreatedVersionIds(header2Epoch).head == newVersion).pure[F]
        _              <- assert(dataAtHeader3.epochToCreatedVersionIds.get(header3Epoch).isEmpty).pure[F]
        _              <- assert(epochToVer1 == epochToVer3).pure[F]
        _              <- assert(initialData != dataAtHeader3).pure[F]

        _            <- initialState.stateAt(headers.head.parentHeaderId)
        initialData2 <- versionsData.makeCopy
        _            <- assert(initialData == initialData2).pure[F]

        atHeader4 = headers.last
        _              <- initialState.stateAt(atHeader4.id)
        versionOfHead4 <- versionsData.versionCounter.getOrRaise(())
        _              <- assert(versionOfHead4 == initialFreeVersion + 1).pure[F]
        header4Epoch   <- defaultClocks.epochOf(atHeader4.slot)
        epochToIds4    <- versionsData.epochToProposalIds.get(header2Epoch)
        _              <- assert(epochToIds4.isEmpty).pure[F]
        versions4      <- versionsData.epochToCreatedVersionIds.getOrRaise(header2Epoch)
        _              <- assert(versions4.head == newVersion).pure[F]
        epochToVer4    <- epochToVersionStore.copyData
        _              <- assert(epochToVer4(7) == newVersion).pure[F]

        _                    <- initialState.stateAt(headers.head.parentHeaderId)
        initialData3         <- versionsData.makeCopy
        _                    <- assert(initialData == initialData3).pure[F]
        initialEpochVersion3 <- epochToVersionStore.copyData
        _                    <- assert(initialEpochVersion == initialEpochVersion3).pure[F]
      } yield ()
    }
  }

}

object VersionsEventSourceStateSpec {

  case class StoragesData(
    proposalIdToProposal:     Map[ProposalId, UpdateProposal],
    epochToProposalIds:       Map[Epoch, Set[ProposalId]],
    proposalVotes:            Map[(Epoch, ProposalId), Epoch],
    epochToCreatedVersionIds: Map[Epoch, Set[VersionId]],
    versionIdToProposal:      Map[VersionId, UpdateProposal],
    versionCounter:           Map[Unit, VersionId],
    epochToVersionIds:        Map[Epoch, Set[VersionId]]
  )

  implicit class VersionsDataOps[F[_]: Async](data: VersionsEventSourceState.VersionsData[F]) {

    private def getDataFromStorage[Key, T](storage: Store[F, Key, T]): F[Map[Key, T]] =
      storage.getAll().map(_.toMap)

    def makeCopy: F[StoragesData] =
      for {
        idToProposal             <- getDataFromStorage[ProposalId, UpdateProposal](data.idToProposal)
        epochToProposalIds       <- getDataFromStorage[Epoch, Set[ProposalId]](data.epochToProposalIds)
        proposalVoting           <- getDataFromStorage[(Epoch, ProposalId), Long](data.proposalVoting)
        epochToCreatedVersionIds <- getDataFromStorage[Epoch, Set[VersionId]](data.epochToCreatedVersionIds)
        versionIdToProposal      <- getDataFromStorage[VersionId, UpdateProposal](data.versionIdToProposal)
        versionCounter           <- getDataFromStorage[Unit, VersionId](data.versionCounter)
        epochToVersionsIds       <- getDataFromStorage[Epoch, Set[VersionId]](data.epochToVersionIds)
      } yield StoragesData(
        idToProposal,
        epochToProposalIds,
        proposalVoting,
        epochToCreatedVersionIds,
        versionIdToProposal,
        versionCounter,
        epochToVersionsIds
      )
  }

}
