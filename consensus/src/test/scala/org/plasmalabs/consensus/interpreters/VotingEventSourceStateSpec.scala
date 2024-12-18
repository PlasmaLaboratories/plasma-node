package org.plasmalabs.consensus.interpreters

import cats.effect.{Async, IO}
import cats.implicits.*
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.plasmalabs.algebras.ClockAlgebra.implicits.*
import org.plasmalabs.algebras.testInterpreters.TestStore
import org.plasmalabs.algebras.{ClockAlgebra, Store}
import org.plasmalabs.codecs.bytes.tetra.ModelGenerators.*
import org.plasmalabs.codecs.bytes.tetra.instances.*
import org.plasmalabs.consensus.*
import org.plasmalabs.consensus.algebras.VersionInfoAlgebra
import org.plasmalabs.consensus.interpreters.CrossEpochEventSourceState.VotingData
import org.plasmalabs.consensus.models.{BlockHeader, BlockId, *}
import org.plasmalabs.crypto.signing.Ed25519VRF
import org.plasmalabs.eventtree.{EventSourcedState, ParentChildTree}
import org.plasmalabs.ledger.interpreters.ProposalEventSourceState
import org.plasmalabs.ledger.interpreters.ProposalEventSourceState.*
import org.plasmalabs.models.*
import org.plasmalabs.models.ModelGenerators.*
import org.plasmalabs.models.generators.consensus.ModelGenerators.arbitraryHeader
import org.plasmalabs.models.protocol.{ConfigConverter, ConfigGenesis}
import org.plasmalabs.models.utility.Ratio
import org.plasmalabs.node.models.BlockBody
import org.plasmalabs.numerics.implicits.*
import org.plasmalabs.proto.node.EpochData
import org.plasmalabs.sdk.generators.TransactionGenerator
import org.plasmalabs.sdk.models.TransactionId
import org.plasmalabs.sdk.models.box.Value
import org.plasmalabs.sdk.models.box.Value.ConfigProposal
import org.plasmalabs.sdk.models.transaction.*
import org.plasmalabs.typeclasses.implicits.*
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.collection.immutable.NumericRange
import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration
import scala.util.Random

class VotingEventSourceStateSpec
    extends CatsEffectSuite
    with ScalaCheckEffectSuite
    with AsyncMockFactory
    with TransactionGenerator {
  type F[A] = IO[A]
  implicit val logger: Logger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)
  implicit val ed255Vrf: Ed25519VRF = Ed25519VRF.precomputed()

  import VotingEventSourceStateSpec._

  private val epochLen = 10L
  private val initialFreeVersion = 2

  private val defaultConfig = ProposalConfig(
    proposalVotingMaxWindow = 5,
    proposalVotingWindow = 2,
    proposalInactiveVotingWindow = 1,
    configProposalPercentage = 0.2,
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

  private def makeInitialVersionsData[F[_]: Async: Logger](
    versionStorage: TestStore[F, Epoch, VersionId]
  ): F[VotingData[F]] =
    for {
      epochToProposalIds       <- TestStore.make[F, Epoch, Set[ProposalId]]
      proposalVoting           <- TestStore.make[F, (Epoch, ProposalId), Long]
      epochToCreatedVersionIds <- TestStore.make[F, Epoch, Set[VersionId]]
      epochToVersionIds        <- TestStore.make[F, Epoch, Set[VersionId]]
      versionIdToProposal      <- TestStore.make[F, VersionId, ConfigProposal]
      versionCounter           <- TestStore.make[F, Unit, VersionId]
      versionVoting            <- TestStore.make[F, (Epoch, VersionId), Long]
      _                        <- versionCounter.put((), initialFreeVersion)

      versionAlgebra: VersionInfoAlgebra[F] <- VersionInfo.make(versionStorage)

    } yield VotingData(
      epochToProposalIds,
      proposalVoting,
      epochToCreatedVersionIds,
      epochToVersionIds,
      versionIdToProposal,
      versionCounter,
      versionVoting,
      versionAlgebra
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
    proposalsMap: Map[BlockId, Seq[ConfigProposal]]
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

  private def addProposalToBlock(blockId: BlockId, proposal: ConfigProposal, storages: BlocksStorages): Unit = {
    val valueValueProposal: org.plasmalabs.sdk.models.box.Value.Value = Value.Value.ConfigProposal(proposal)
    val value: org.plasmalabs.sdk.models.box.Value = new org.plasmalabs.sdk.models.box.Value(value = valueValueProposal)
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
  val noPlanForEpoch: List[(Seq[Int], Int, Int)] = Range(0, epochLen.toInt).map(_ => noPlan).toList

  // For list:
  // position in list -- position in chain
  // first argument -- proposal pseudo id in particular block where
  //   real id = VersionsEventSourceState.getProposalId(ConfigProposal(label = pseudoId.toString))
  // second argument -- voted version in block
  // third argument -- voted proposal id in block
  def makeData(plan: List[(Seq[Int], Int, Int)]): (Seq[BlockHeader], BlocksStorages) = {
    val rootBlock: BlockHeader = blockWithVersions(arbitraryHeader.arbitrary.first, -1, 0, 0, 0)
    val initialAcc = Seq.empty[(BlockHeader, Seq[ConfigProposal])]
    val blockChainWithProposals = plan.zipWithIndex.foldLeft(initialAcc) {
      case (acc, ((proposals, versionVote, proposalVote), index)) =>
        val parent = acc.lastOption.fold(rootBlock)(_._1)
        val header = blockWithVersions(parent, index, 0, versionVote, getProposalIdByPseudoId(proposalVote))
        val configProposals = proposals.map(getProposalByPseudoId)
        acc :+ (header, configProposals)
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

  val pseudoIdToProposal: mutable.Map[Int, ConfigProposal] = mutable.Map.empty

  private val defaultGenesisConfig = ConfigGenesis(
    label = "",
    Ratio(56, 89),
    45,
    99,
    Ratio(99, 56),
    Ratio(66, 7),
    100,
    com.google.protobuf.duration.Duration(56, 9),
    55,
    9,
    13,
    4,
    1000
  )

  def getProposalByPseudoId(pseudoId: Int): ConfigProposal =
    pseudoIdToProposal.getOrElseUpdate(
      pseudoId,
      ConfigConverter.pack[ConfigGenesis](defaultGenesisConfig.copy(label = pseudoId.toString))
    )

  val pseudoIdToProposalId: mutable.Map[Int, Int] = mutable.Map.empty

  def getProposalIdByPseudoId(pseudoId: Int): Int =
    if (pseudoId == 0) {
      emptyVersionId
    } else {
      pseudoIdToProposalId.getOrElseUpdate(
        pseudoId,
        ProposalEventSourceState.getProposalId(getProposalByPseudoId(pseudoId))
      )
    }

  private val initialVersionMap = Map(Long.MinValue -> 1)

  def createDefaultVersionEventsAndChainFromPlan(
    plan:     List[(Seq[Int], Int, Int)],
    versions: Map[Epoch, VersionId] = Map.empty
  ): F[
    (
      EventSourcedState[F, VotingEventSourceState.State[F], BlockId],
      VotingData[F],
      ProposalData[F],
      Seq[BlockHeader],
      TestStore[F, Epoch, VersionId]
    )
  ] = {
    val (headers, storages) = makeData(plan)

    for {
      parentTree     <- makeParentTree[F](headers)
      versionStorage <- TestStore.make[F, Epoch, VersionId]
      versionMap = initialVersionMap ++ versions
      _            <- versionMap.toList.traverse { case (k, v) => versionStorage.put(k, v) }
      versionsData <- makeInitialVersionsData[F](versionStorage)

      initialEpochData <- TestStore.make[F, Epoch, BlockId]
      epochBoundaryEventSourcedState <-
        EpochBoundariesEventSourcedState.make[F](
          defaultClocks,
          headers.head.parentHeaderId.pure[F],
          parentTree,
          eventChangedStub,
          (initialEpochData: Store[F, Epoch, BlockId]).pure[F],
          id => storages.headers(id).slotData.pure[F]
        )

      idToProposal              <- TestStore.make[F, ProposalId, ConfigProposal]
      epochToCreatedProposalIds <- TestStore.make[F, Epoch, Set[ProposalId]]
      proposalData = ProposalData(idToProposal, epochToCreatedProposalIds)
      proposalState <- ProposalEventSourceState.make[F](
        headers.head.parentHeaderId.pure[F],
        parentTree,
        eventChangedStub,
        proposalData.pure[F],
        defaultClocks,
        id => storages.headers(id).pure[F],
        id => storages.body(id).pure[F],
        id => storages.tx(id).pure[F],
        defaultConfig
      )

      epochDataStore <- makeEpochDataStore[F].pure[F]
      epochDataSource = new EventSourcedState[F, Store[F, Epoch, EpochData], BlockId] {
        override def stateAt(eventId: BlockId): F[Store[F, Epoch, EpochData]] = ???

        override def useStateAt[U](eventId: BlockId)(f: Store[F, Epoch, EpochData] => F[U]): F[U] =
          f(epochDataStore)
      }

      crossEpochInitialState <- CrossEpochEventSourceState.make[F](
        headers.head.parentHeaderId.pure[F],
        parentTree,
        eventChangedStub,
        versionsData.pure[F],
        defaultClocks,
        id => storages.headers(id).pure[F],
        epochBoundaryEventSourcedState,
        epochDataSource,
        proposalState,
        headers.head.id,
        defaultConfig
      )

      initialState <- VotingEventSourceState.make[F](
        headers.head.parentHeaderId.pure[F],
        parentTree,
        eventChangedStub,
        VotingEventSourceState.State[F]().pure[F],
        defaultClocks,
        id => storages.headers(id).pure[F],
        crossEpochInitialState
      )
    } yield (initialState, versionsData, proposalData, headers, versionStorage)
  }

  test("Add proposal and proposal vote in first available epoch") {
    withMock {
      val proposalPseudoId1 = 1
      val proposalPseudoId2 = 2
      val proposalPseudoId3 = 3
      val plan: List[(Seq[Int], Int, Int)] = noPlanForEpoch ++
        List(
          (Seq(proposalPseudoId1), 0, 0),
          (Seq(proposalPseudoId2, proposalPseudoId3), 0, 0),
          noPlan,
          noPlan,
          noPlan,
          noPlan,
          noPlan,
          noPlan,
          noPlan,
          noPlan
        ) ++ noPlanForEpoch ++ noPlanForEpoch :+ (Seq.empty[Int], 0, proposalPseudoId1)

      for {
        (voteState, votingData, proposalData, headers, _) <- createDefaultVersionEventsAndChainFromPlan(plan)
        initialVersionData                                <- votingData.makeCopy
        initialProposalData                               <- proposalData.makeCopy

        proposalIndex = epochLen.toInt
        _             <- voteState.stateAt(headers(proposalIndex).id)
        versionOfHead <- votingData.versionCounter.getOrRaise(())
        _             <- assert(versionOfHead == initialFreeVersion).pure[F]
        epochOfHead   <- defaultClocks.epochOf(headers(proposalIndex).slot)
        epochToIds    <- votingData.epochToProposalIds.get(epochOfHead)
        _             <- assert(epochToIds.isEmpty).pure[F]

        _                      <- voteState.stateAt(headers.last.id)
        epochToCreatedProposal <- proposalData.epochToCreatedProposalIds.getOrRaise(epochOfHead + proposalDelta)
        _                      <- assert(epochToCreatedProposal.size == 3).pure[F]
        proposalRealId = getProposalIdByPseudoId(proposalPseudoId1)
        _                 <- proposalData.idToProposal.getOrRaise(proposalRealId)
        epochOfLast       <- defaultClocks.epochOf(headers.last.slot)
        proposalVotesLast <- votingData.proposalVoting.getOrRaise((epochOfLast, proposalRealId))
        _                 <- assert(proposalVotesLast == 1).pure[F] // voted for that proposal at last block
        activeProposals   <- votingData.epochToProposalIds.getOrRaise(epochOfLast)
        _                 <- assert(activeProposals.size == 3).pure[F]
        _                 <- assert(activeProposals.contains(proposalRealId)).pure[F]

        _ <- voteState.stateAt(headers(5).id)
        _ <- voteState.stateAt(headers(15).id)

        _               <- voteState.stateAt(headers.head.parentHeaderId)
        endVersionData  <- votingData.makeCopy
        endProposalData <- proposalData.makeCopy

        _ <- assert(endVersionData == initialVersionData).pure[F]
        _ <- assert(endProposalData == initialProposalData).pure[F]
      } yield ()
    }
  }

  test("Proposal exist during proposalVotingMaxWindow epochs, vote exist only in voted epoch") {
    withMock {
      val proposalPseudoId1 = 1
      val proposalRealId1 = getProposalIdByPseudoId(proposalPseudoId1)
      val plan: List[(Seq[Int], Int, Int)] =
        noPlanForEpoch ++
        List(
          (Seq(proposalPseudoId1), 0, 0),
          noPlan,
          noPlan,
          noPlan,
          noPlan,
          noPlan,
          noPlan,
          noPlan,
          noPlan,
          noPlan
        ) ++ noPlanForEpoch ++
        noPlanForEpoch ++ // proposalIndex1
        List(
          (Seq(), 0, 1),
          noPlan,
          noPlan,
          noPlan,
          noPlan,
          (Seq(), 0, 1),
          noPlan,
          (Seq(), 0, 1),
          noPlan,
          noPlan // proposalIndex2
        ) ++
        noPlanForEpoch ++
        noPlanForEpoch ++
        noPlanForEpoch ++
        noPlanForEpoch :+ // proposalIndex3
        noPlan // proposalIndex4

      for {
        (voteState, votingData, proposalData, headers, versions) <- createDefaultVersionEventsAndChainFromPlan(plan)
        initialVersionData                                       <- votingData.makeCopy
        initialProposalData                                      <- proposalData.makeCopy

        availableProposalEpoch = 3
        proposalIndex1 = epochLen.toInt * availableProposalEpoch
        _                     <- voteState.stateAt(headers(proposalIndex1).id)
        proposalEpoch1        <- defaultClocks.epochOf(headers(proposalIndex1).slot)
        proposalVersionData1  <- votingData.makeCopy
        proposalProposalData1 <- proposalData.makeCopy
        _ <- assert(proposalVersionData1.epochToProposalIds == Map(proposalEpoch1 -> Set(proposalRealId1))).pure[F]
        _ <- assert(proposalVersionData1.proposalVoting == Map((proposalEpoch1, proposalRealId1) -> 0)).pure[F]
        _ <- assert(proposalVersionData1.epochToCreatedVersionIds.isEmpty).pure[F]
        _ <- assert(proposalVersionData1.epochToVersionIds.isEmpty).pure[F]
        _ <- assert(proposalVersionData1.versionIdToProposal.isEmpty).pure[F]
        _ <- assert(proposalVersionData1.versionCounter == Map(() -> initialFreeVersion)).pure[F]
        _ <- assert(proposalVersionData1.versionVoting.isEmpty).pure[F]
        _ <- assertIOBoolean(versions.getAll().map(_.toMap == initialVersionMap))
        _ <- assert(
          proposalProposalData1.idToProposal ==
            Map(proposalRealId1 -> getProposalByPseudoId(proposalPseudoId1))
        ).pure[F]
        _ <- assert(
          proposalProposalData1.epochToCreatedProposalIds ==
            Map(proposalEpoch1 -> Set(proposalRealId1))
        ).pure[F]

        proposalIndex2 = epochLen.toInt * 4 + 9
        _                     <- voteState.stateAt(headers(proposalIndex2).id)
        proposalEpoch2        <- defaultClocks.epochOf(headers(proposalIndex2).slot)
        proposalVersionData2  <- votingData.makeCopy
        proposalProposalData2 <- proposalData.makeCopy
        _ <- assert(
          proposalVersionData2.epochToProposalIds ==
            Map(proposalEpoch1 -> Set(proposalRealId1), proposalEpoch2 -> Set(proposalRealId1))
        ).pure[F]
        _ <- assert(
          proposalVersionData2.proposalVoting ==
            Map((proposalEpoch1, proposalRealId1) -> 0, (proposalEpoch2, proposalRealId1) -> 3)
        ).pure[F]
        _ <- assert(proposalVersionData2.epochToCreatedVersionIds.isEmpty).pure[F]
        _ <- assert(proposalVersionData2.epochToVersionIds.isEmpty).pure[F]
        _ <- assert(proposalVersionData2.versionIdToProposal.isEmpty).pure[F]
        _ <- assert(proposalVersionData2.versionCounter == Map(() -> initialFreeVersion)).pure[F]
        _ <- assert(proposalVersionData2.versionVoting.isEmpty).pure[F]
        _ <- assertIOBoolean(versions.getAll().map(_.toMap == initialVersionMap))
        _ <- assert(
          proposalProposalData2.idToProposal ==
            Map(proposalRealId1 -> getProposalByPseudoId(proposalPseudoId1))
        ).pure[F]
        _ <- assert(
          proposalProposalData2.epochToCreatedProposalIds ==
            Map(proposalEpoch1 -> Set(proposalRealId1))
        ).pure[F]

        lastAvailableProposalEpoch = availableProposalEpoch + defaultConfig.proposalVotingMaxWindow
        proposalIndex3 = epochLen.toInt * lastAvailableProposalEpoch
        _                     <- voteState.stateAt(headers(proposalIndex3).id)
        proposalVersionData3  <- votingData.makeCopy
        proposalProposalData3 <- proposalData.makeCopy
        epochsRange3 = (availableProposalEpoch.toLong to lastAvailableProposalEpoch)
        _ <- assert(
          proposalVersionData3.epochToProposalIds ==
            epochsRange3.map(e => e -> Set(proposalRealId1)).toMap
        ).pure[F]
        _ <- assert(
          proposalVersionData3.proposalVoting ==
            epochsRange3.map(e => (e, proposalRealId1) -> 0).toMap ++ Map((proposalEpoch2, proposalRealId1) -> 3)
        ).pure[F]
        _ <- assert(proposalVersionData3.epochToCreatedVersionIds.isEmpty).pure[F]
        _ <- assert(proposalVersionData3.epochToVersionIds.isEmpty).pure[F]
        _ <- assert(proposalVersionData3.versionIdToProposal.isEmpty).pure[F]
        _ <- assert(proposalVersionData3.versionCounter == Map(() -> initialFreeVersion)).pure[F]
        _ <- assert(proposalVersionData3.versionVoting.isEmpty).pure[F]
        _ <- assertIOBoolean(versions.getAll().map(_.toMap == initialVersionMap))
        _ <- assert(
          proposalProposalData3.idToProposal ==
            Map(proposalRealId1 -> getProposalByPseudoId(proposalPseudoId1))
        ).pure[F]
        _ <- assert(
          proposalProposalData3.epochToCreatedProposalIds ==
            Map(proposalEpoch1 -> Set(proposalRealId1))
        ).pure[F]

        expiredProposalEpoch = availableProposalEpoch + defaultConfig.proposalVotingMaxWindow + 1
        proposalIndex4 = epochLen.toInt * expiredProposalEpoch
        _                     <- voteState.stateAt(headers(proposalIndex4).id)
        proposalVersionData4  <- votingData.makeCopy
        proposalProposalData4 <- proposalData.makeCopy
        // proposal is expired so do not include last epoch
        epochsRange4 = (availableProposalEpoch.toLong to (expiredProposalEpoch - 1))
        _ <- assert(
          proposalVersionData4.epochToProposalIds ==
            epochsRange4.map(e => e -> Set(proposalRealId1)).toMap
        ).pure[F]
        _ <- assert(
          proposalVersionData4.proposalVoting ==
            epochsRange4.map(e => (e, proposalRealId1) -> 0).toMap ++ Map((proposalEpoch2, proposalRealId1) -> 3)
        ).pure[F]
        _ <- assert(proposalVersionData4.epochToCreatedVersionIds.isEmpty).pure[F]
        _ <- assert(proposalVersionData4.epochToVersionIds.isEmpty).pure[F]
        _ <- assert(proposalVersionData4.versionIdToProposal.isEmpty).pure[F]
        _ <- assert(proposalVersionData4.versionCounter == Map(() -> initialFreeVersion)).pure[F]
        _ <- assert(proposalVersionData4.versionVoting.isEmpty).pure[F]
        _ <- assertIOBoolean(versions.getAll().map(_.toMap == initialVersionMap))
        _ <- assert(
          proposalProposalData4.idToProposal ==
            Map(proposalRealId1 -> getProposalByPseudoId(proposalPseudoId1))
        ).pure[F]
        _ <- assert(
          proposalProposalData4.epochToCreatedProposalIds ==
            Map(proposalEpoch1 -> Set(proposalRealId1))
        ).pure[F]

        randomPoint1 = Random.nextInt(plan.size - 1)
        _                        <- Logger[F].info(show"Go to $randomPoint1 element")
        _                        <- voteState.stateAt(headers(randomPoint1).id)
        randomPoint1VersionData  <- votingData.makeCopy
        randomPoint1ProposalData <- proposalData.makeCopy
        randomPoint2 = Random.nextInt(plan.size - 1)
        _                          <- Logger[F].info(show"Go to $randomPoint2 element")
        _                          <- voteState.stateAt(headers(randomPoint2).id)
        _                          <- voteState.stateAt(headers(randomPoint1).id)
        randomPoint1_1VersionData  <- votingData.makeCopy
        randomPoint1_1ProposalData <- proposalData.makeCopy
        _                          <- assert(randomPoint1VersionData == randomPoint1_1VersionData).pure[F]
        _                          <- assert(randomPoint1ProposalData == randomPoint1_1ProposalData).pure[F]

        _               <- voteState.stateAt(headers.head.parentHeaderId)
        endVotingData   <- votingData.makeCopy
        endProposalData <- proposalData.makeCopy

        _ <- assert(endVotingData == initialVersionData).pure[F]
        _ <- assert(endProposalData == initialProposalData).pure[F]
      } yield ()
    }
  }

  test("Make version from voted proposal") {
    withMock {
      val proposalPseudoId1 = 1
      val plan: List[(Seq[Int], Int, Int)] =
        noPlanForEpoch ++
        List(
          noPlan,
          (Seq(proposalPseudoId1), 0, 0),
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
        (voteState, votingData, proposalData, headers, versions) <- createDefaultVersionEventsAndChainFromPlan(plan)
        initialVersionData                                       <- votingData.makeCopy
        initialProposalData                                      <- proposalData.makeCopy

        proposalRealId = getProposalIdByPseudoId(proposalPseudoId1)

        atHeader2 = headers.last
        _                        <- voteState.stateAt(atHeader2.id)
        versionOfHead            <- votingData.versionCounter.getOrRaise(())
        _                        <- assert(versionOfHead == initialFreeVersion + 1).pure[F]
        header2Epoch             <- defaultClocks.epochOf(atHeader2.slot)
        epochToIds               <- votingData.epochToProposalIds.get(header2Epoch)
        _                        <- assert(epochToIds.isEmpty).pure[F]
        epochToCreatedVersionIds <- votingData.epochToCreatedVersionIds.getOrRaise(header2Epoch)
        _                        <- assert(epochToCreatedVersionIds.head == initialFreeVersion).pure[F]

        dataAtHeader2   <- votingData.makeCopy
        _               <- assert(!dataAtHeader2.proposalVoting.contains((header2Epoch, proposalRealId))).pure[F]
        _               <- assert(initialVersionData != dataAtHeader2).pure[F]
        _               <- voteState.stateAt(headers(7).id)
        _               <- voteState.stateAt(headers.head.parentHeaderId)
        dataAtPreHeader <- votingData.makeCopy
        _               <- assert(initialVersionData == dataAtPreHeader).pure[F]

        _               <- voteState.stateAt(headers.head.parentHeaderId)
        endVotingData   <- votingData.makeCopy
        endProposalData <- proposalData.makeCopy

        _ <- assert(endVotingData == initialVersionData).pure[F]
        _ <- assert(endProposalData == initialProposalData).pure[F]
      } yield ()
    }
  }

  test("Make version from voted proposal, vote version to add version starts information") {
    withMock {
      val proposalPseudoId1 = 1
      val currentVersion = initialFreeVersion - 1
      val plan: List[(Seq[Int], Int, Int)] =
        noPlanForEpoch ++
        List(
          (Seq(proposalPseudoId1), currentVersion, 0),
          noPlan,
          noPlan,
          noPlan,
          noPlan,
          (Seq(), currentVersion, -9999), // we shall ignore non-exist proposal voting
          noPlan,
          noPlan,
          noPlan,
          (Seq(), currentVersion, 0)
        ) ++
        noPlanForEpoch ++
        noPlanForEpoch ++
        List(
          (Seq(), 0, proposalPseudoId1),
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
          (Seq(), initialFreeVersion, 0), // index1
          (Seq(), initialFreeVersion, -9999), // we shall ignore non-exist proposal voting
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
          (Seq(), initialFreeVersion, 0) // index2
        ) :+ noPlan // index3

      for {
        (voteState, votingData, proposalData, headers, epochToVersionStore) <-
          createDefaultVersionEventsAndChainFromPlan(plan)
        initialVotingData    <- votingData.makeCopy
        initialProposingData <- proposalData.makeCopy
        initialEpochVersion  <- epochToVersionStore.copyData

        index1 = epochLen.toInt * 6
        atHeader1 = headers(index1)
        _               <- voteState.stateAt(atHeader1.id)
        votingDataCopy1 <- votingData.makeCopy
        epochToVersion  <- epochToVersionStore.getAll().map(_.toMap)
        index1Epoch     <- defaultClocks.epochOf(atHeader1.slot)
        epochToIds1 = votingDataCopy1.epochToProposalIds.get(index1Epoch)
        _ <- assert(epochToIds1.isEmpty).pure[F]
        proposalForVersion1 = votingDataCopy1.versionIdToProposal(initialFreeVersion)
        _ <- assert(proposalForVersion1 == getProposalByPseudoId(proposalPseudoId1)).pure[F]
        versionCounter1 = votingDataCopy1.versionCounter(())
        _ <- assert(versionCounter1 == initialFreeVersion + 1).pure[F]
        // also check that voting for current version has no effect at all
        _ <- assert(votingDataCopy1.versionVoting == Map((index1Epoch, initialFreeVersion) -> 1)).pure[F]
        _ = assert(epochToVersion == initialVersionMap)

        index2 = epochLen.toInt * 8 + epochLen.toInt - 1
        atHeader2 = headers(index2)
        _               <- voteState.stateAt(atHeader2.id)
        votingDataCopy2 <- votingData.makeCopy
        epochToVersion2 <- epochToVersionStore.getAll().map(_.toMap)
        index2Epoch     <- defaultClocks.epochOf(atHeader2.slot)
        epochToIds2 = votingDataCopy2.epochToProposalIds.get(index2Epoch)
        _ <- assert(epochToIds2.isEmpty).pure[F]
        proposalForVersion2 = votingDataCopy2.versionIdToProposal(initialFreeVersion)
        _ <- assert(proposalForVersion2 == getProposalByPseudoId(proposalPseudoId1)).pure[F]
        versionCounter2 = votingDataCopy2.versionCounter(())
        _ <- assert(versionCounter2 == initialFreeVersion + 1).pure[F]
        _ <- assert(
          votingDataCopy2.versionVoting ==
            Map(
              (index1Epoch, initialFreeVersion)     -> 7,
              (index1Epoch + 1, initialFreeVersion) -> 9,
              (index2Epoch, initialFreeVersion)     -> 10
            )
        ).pure[F]
        _ = assert(epochToVersion2 == initialVersionMap)

        index3 = headers.size - 1
        atHeader3 = headers(index3)
        _               <- voteState.stateAt(atHeader3.id)
        votingDataCopy3 <- votingData.makeCopy
        epochToVersion3 <- epochToVersionStore.getAll().map(_.toMap)
        index3Epoch     <- defaultClocks.epochOf(atHeader3.slot)
        epochToIds3 = votingDataCopy3.epochToProposalIds.get(index3Epoch)
        _ <- assert(epochToIds3.isEmpty).pure[F]
        proposalForVersion3 = votingDataCopy3.versionIdToProposal(initialFreeVersion)
        _ <- assert(proposalForVersion3 == getProposalByPseudoId(proposalPseudoId1)).pure[F]
        versionCounter3 = votingDataCopy2.versionCounter(())
        _ <- assert(versionCounter3 == initialFreeVersion + 1).pure[F]
        _ <- assert(
          votingDataCopy2.versionVoting ==
            Map(
              (index1Epoch, initialFreeVersion)     -> 7,
              (index1Epoch + 1, initialFreeVersion) -> 9,
              (index2Epoch, initialFreeVersion)     -> 10
            )
        ).pure[F]
        expectedMap3 = initialVersionMap ++ Map((index3Epoch + defaultConfig.versionSwitchWindow) -> initialFreeVersion)
        _ <- assert(epochToVersion3 == expectedMap3)
          .pure[F]

        randomPoint1 = Random.nextInt(plan.size - 1)
        _                          <- Logger[F].info(show"Go to $randomPoint1 element")
        _                          <- voteState.stateAt(headers(randomPoint1).id)
        randomPoint1VersionData    <- votingData.makeCopy
        randomPoint1ProposalData   <- proposalData.makeCopy
        randomPoint1EpochToVersion <- epochToVersionStore.getAll().map(_.toMap)
        randomPoint2 = Random.nextInt(plan.size - 1)
        _                            <- Logger[F].info(show"Go to $randomPoint2 element")
        _                            <- voteState.stateAt(headers(randomPoint2).id)
        _                            <- voteState.stateAt(headers(randomPoint1).id)
        randomPoint1_1VersionData    <- votingData.makeCopy
        randomPoint1_1ProposalData   <- proposalData.makeCopy
        randomPoint1_1EpochToVersion <- epochToVersionStore.getAll().map(_.toMap)
        _                            <- assert(randomPoint1VersionData == randomPoint1_1VersionData).pure[F]
        _                            <- assert(randomPoint1ProposalData == randomPoint1_1ProposalData).pure[F]
        _                            <- assert(randomPoint1EpochToVersion == randomPoint1_1EpochToVersion).pure[F]

        _               <- voteState.stateAt(headers.head.parentHeaderId)
        endVotingData   <- votingData.makeCopy
        endProposalData <- proposalData.makeCopy
        endVersion      <- epochToVersionStore.getAll().map(_.toMap)

        _ <- assert(endVotingData == initialVotingData).pure[F]
        _ <- assert(endProposalData == initialProposingData).pure[F]
        _ <- assert(endVersion == initialEpochVersion).pure[F]
      } yield ()
    }
  }

}

object VotingEventSourceStateSpec {

  case class StoragesData(
    epochToProposalIds:       Map[Epoch, Set[ProposalId]],
    proposalVoting:           Map[(Epoch, ProposalId), Long],
    epochToCreatedVersionIds: Map[Epoch, Set[VersionId]],
    epochToVersionIds:        Map[Epoch, Set[VersionId]],
    versionIdToProposal:      Map[VersionId, ConfigProposal],
    versionCounter:           Map[Unit, VersionId],
    versionVoting:            Map[(Epoch, VersionId), Long]
  )

  private def getDataFromStorage[F[_]: Async, Key, T](storage: Store[F, Key, T]): F[Map[Key, T]] =
    storage.getAll().map(_.toMap)

  implicit class VersionsDataOps[F[_]: Async](data: VotingData[F]) {

    def makeCopy: F[StoragesData] =
      for {
        epochToProposalIds       <- getDataFromStorage[F, Epoch, Set[ProposalId]](data.epochToProposalIds)
        proposalVoting           <- getDataFromStorage[F, (Epoch, ProposalId), Long](data.proposalVoting)
        epochToCreatedVersionIds <- getDataFromStorage[F, Epoch, Set[VersionId]](data.epochToCreatedVersionIds)
        epochToVersionIds        <- getDataFromStorage[F, Epoch, Set[VersionId]](data.epochToVersionIds)
        versionIdToProposal      <- getDataFromStorage[F, VersionId, ConfigProposal](data.versionIdToProposal)
        versionCounter           <- getDataFromStorage[F, Unit, VersionId](data.versionCounter)
        versionVoting            <- getDataFromStorage[F, (Epoch, VersionId), Long](data.versionVoting)
      } yield StoragesData(
        epochToProposalIds,
        proposalVoting,
        epochToCreatedVersionIds,
        epochToVersionIds,
        versionIdToProposal,
        versionCounter,
        versionVoting
      )
  }

  case class ProposalsDataStorages(
    idToProposal:              Map[ProposalId, ConfigProposal],
    epochToCreatedProposalIds: Map[Epoch, Set[ProposalId]]
  )

  implicit class ProposalDataOps[F[_]: Async](data: ProposalData[F]) {

    def makeCopy: F[ProposalsDataStorages] =
      for {
        idToProposal              <- getDataFromStorage[F, ProposalId, ConfigProposal](data.idToProposal)
        epochToCreatedProposalIds <- getDataFromStorage[F, Epoch, Set[ProposalId]](data.epochToCreatedProposalIds)
      } yield ProposalsDataStorages(
        idToProposal,
        epochToCreatedProposalIds
      )
  }

}
