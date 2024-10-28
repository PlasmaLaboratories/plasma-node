package org.plasmalabs.config

import org.plasmalabs.consensus.models.{BlockId, StakingAddress}
import org.plasmalabs.models.Slot
import org.plasmalabs.models.utility.Ratio
import org.plasmalabs.numerics.implicits._
import org.plasmalabs.proto.node.NodeConfig
import org.plasmalabs.sdk.models.LockAddress

import scala.concurrent.duration._

// $COVERAGE-OFF$
case class ApplicationConfig(
  node:    ApplicationConfig.Node,
  indexer: ApplicationConfig.Indexer,
  kamon:   ApplicationConfig.Kamon
)

object ApplicationConfig {

  case class Node(
    data:                Node.Data,
    staking:             Node.Staking,
    p2p:                 Node.P2P,
    rpc:                 Node.RPC,
    ethereumJsonRpc:     Node.EthereumJsonRpc,
    mempool:             Node.Mempool,
    bigBang:             Node.BigBang,
    maxSupportedVersion: Int = 1,
    protocols:           Map[Slot, Node.Protocol],
    cache:               Node.Cache,
    ntp:                 Node.Ntp,
    versionInfo:         Node.VersionInfo,
    votedVersion:        Int = 0,
    votedProposal:       Int = 0
  )

  object Node {

    case class Data(directory: String, databaseType: String)

    case class Staking(directory: String, rewardAddress: LockAddress, stakingAddress: Option[StakingAddress])

    case class P2P(
      bindHost:          String,
      bindPort:          Int,
      publicHost:        Option[String],
      publicPort:        Option[Int],
      knownPeers:        List[KnownPeer],
      networkProperties: NetworkProperties
    )

    case class NetworkProperties(
      useHostNames:                         Boolean = false,
      pingPongInterval:                     FiniteDuration = 90.seconds,
      expectedSlotsPerBlock:                Double = 15.0, // TODO shall be calculated?
      maxPerformanceDelayInSlots:           Double = 2.0,
      remotePeerNoveltyInExpectedBlocks:    Double = 2.0,
      minimumBlockProvidingReputationPeers: Int = 2,
      minimumPerformanceReputationPeers:    Int = 2,
      // every slot we update txMempoolReputation as
      // newReputation = (currentReputation * (txImpactValue - 1) + txMempoolReputationForLastSlot) / txImpactValue
      txImpactRatio:                   Int = 1000,
      minimumTxMempoolReputationPeers: Int = 2,
      minimumRequiredReputation:       Double = 0.66,
      // any non-new peer require that reputation to be hot
      minimumBlockProvidingReputation: Double = 0.15,
      minimumEligibleColdConnections:  Int = 50,
      maximumEligibleColdConnections:  Int = 100,
      clearColdIfNotActiveForInMs:     Long = 7 * 24 * 60 * 60 * 1000, // 7 days
      minimumHotConnections:           Int = 7,
      maximumWarmConnections:          Int = 12,
      warmHostsUpdateEveryNBlock:      Double = 4.0,
      p2pTrackInterval:                FiniteDuration = 10.seconds,
      // we could try to connect to remote peer again after
      // closeTimeoutFirstDelayInMs * {number of closed connections in last closeTimeoutWindowInMs} ^ 2
      closeTimeoutFirstDelayInMs: Long = 1000,
      closeTimeoutWindowInMs:     Long = 1000 * 60 * 60 * 24, // 1 day
      aggressiveP2P:              Boolean = true, // always try to found new good remote peers
      aggressiveP2PCount:         Int = 1, // how many new connection will be opened
      // do not try to open aggressively connection to remote peer if we have closed N connection(s) to them recently
      aggressiveP2PMaxCloseEvent: Int = 3,
      defaultTimeout:             FiniteDuration = 3.seconds,
      chunkSize:                  Int = 1,
      // If remote peer have ip address in that list then that peer will not be exposed to other peers
      // Examples of supported ip addresses description:
      // 10.*.65-67.0/24 (first octet is "10", second octet is any, third octet in range 65-67, subnet mask is 24)
      // If subnet mask is used then first address in subnet shall be set, otherwise subnet mask will not be applied
      // In that case next addresses will be filtered: 10.45.67.0, 10.0.65.80, 10.255.66.200.
      // Next address is not filtered: 10.45.64.255
      // Could be used if current node serves as proxy, and we don't want to expose any node behind proxy
      doNotExposeIps: List[String] = List.empty,
      // If remote peer have id in that list then that peer will not be exposed to other peers
      // Could be used if current node serves as proxy, and we don't want to expose any node behind proxy
      doNotExposeIds: List[String] = List.empty,

      // How many parents shall be returned for getRemoteSlotDataWithParents request
      // Increasing number lead to fast sync from scratch from that peer but increased network usage
      slotDataParentDepth: Int = 25
    )

    case class KnownPeer(host: String, port: Int)

    case class RPC(bindHost: String, bindPort: Int, networkControl: Boolean = false)

    case class EthereumJsonRpc(bindHost: String, bindPort: Int)

    case class Mempool(defaultExpirationSlots: Long, protection: MempoolProtection = MempoolProtection())

    case class MempoolProtection(
      enabled: Boolean = false,
      // Use size in some abstract units which are used in org.plasmalabs.sdk.validation.algebras.TransactionCostCalculator
      maxMempoolSize: Long = 1024 * 1024 * 20,

      // do not perform mempool checks
      // if (protectionEnabledThresholdPercent / 100 * maxMempoolSize) is less than curren mempool size
      protectionEnabledThresholdPercent: Double = 10,

      // during semantic check we will include all transactions from memory pool in context
      // if (useMempoolForSemanticThresholdPercent / 100 * maxMempoolSize) is less than curren mempool size
      useMempoolForSemanticThresholdPercent: Double = 40,

      // Memory pool will accept transactions with fee starting from that threshold value,
      // required fee will increased if free memory pool size is decreased,
      // if free memory pool size is 0 then transaction with infinite fee will be required
      feeFilterThresholdPercent: Double = 50,

      // old box is required to be used as inputs starting from that threshold
      ageFilterThresholdPercent: Double = 75,

      // box with age (in height) maxOldBoxAge will always considered as old
      maxOldBoxAge: Long = 100000
    ) {
      val protectionEnabledThreshold = toMultiplier(protectionEnabledThresholdPercent) * maxMempoolSize
      val useMempoolForSemanticThreshold = toMultiplier(useMempoolForSemanticThresholdPercent) * maxMempoolSize
      val feeFilterThreshold = toMultiplier(feeFilterThresholdPercent) * maxMempoolSize
      val ageFilterThreshold = toMultiplier(ageFilterThresholdPercent) * maxMempoolSize
    }

    sealed abstract class BigBang

    object BigBangs {

      case class Private(
        timestamp:        Long = System.currentTimeMillis() + 5_000L,
        stakerCount:      Int,
        stakes:           Option[List[BigInt]],
        localStakerIndex: Option[Int],
        regtestEnabled:   Boolean = false
      ) extends BigBang

      case class Public(
        genesisId:  BlockId,
        sourcePath: String
      ) extends BigBang
    }

    case class Protocol(
      minAppVersion:              String,
      fEffective:                 Ratio,
      vrfLddCutoff:               Int,
      vrfPrecision:               Int,
      vrfBaselineDifficulty:      Ratio,
      vrfAmplitude:               Ratio,
      slotGapLeaderElection:      Long,
      chainSelectionKLookback:    Long,
      slotDuration:               FiniteDuration,
      forwardBiasedSlotWindow:    Slot,
      operationalPeriodsPerEpoch: Long,
      kesKeyHours:                Int,
      kesKeyMinutes:              Int,
      epochLengthOverride:        Option[Long]
    ) {

      val chainSelectionSWindow: Long =
        (Ratio(chainSelectionKLookback, 4L) * fEffective.inverse).round.toLong

      val epochLength: Long =
        epochLengthOverride.getOrElse(((Ratio(chainSelectionKLookback) * fEffective.inverse) * 3).round.toLong)

      val operationalPeriodLength: Long =
        epochLength / operationalPeriodsPerEpoch

      val vrfCacheSize: Long =
        operationalPeriodLength * 4

      def nodeConfig(slot: Slot): NodeConfig = NodeConfig(
        slot = slot,
        slotDurationMillis = slotDuration.toMillis,
        epochLength = epochLength
      )

      def validation: Either[String, Unit] =
        for {
          _ <- Either.cond(epochLength % 3L == 0, (), s"Epoch length=$epochLength must be divisible by 3")
          _ <- Either.cond(
            epochLength % operationalPeriodsPerEpoch == 0,
            (),
            s"Epoch length=$epochLength must be divisible by $operationalPeriodsPerEpoch"
          )
        } yield ()
    }

    case class Cache(
      parentChildTree:         Cache.CacheConfig,
      slotData:                Cache.CacheConfig,
      headers:                 Cache.CacheConfig,
      bodies:                  Cache.CacheConfig,
      transactions:            Cache.CacheConfig,
      spendableBoxIds:         Cache.CacheConfig,
      epochBoundaries:         Cache.CacheConfig,
      operatorStakes:          Cache.CacheConfig,
      registrations:           Cache.CacheConfig,
      blockHeightTree:         Cache.CacheConfig,
      eligibilities:           Cache.CacheConfig,
      epochData:               Cache.CacheConfig,
      registrationAccumulator: Cache.CacheConfig,
      txIdToBlockId:           Cache.CacheConfig,
      idToProposal:            Cache.CacheConfig,
      epochToCreatedVersion:   Cache.CacheConfig,
      versionVoting:           Cache.CacheConfig,
      epochToProposalIds:      Cache.CacheConfig,
      proposalVoting:          Cache.CacheConfig,
      epochToVersionIds:       Cache.CacheConfig,
      versionIdToProposal:     Cache.CacheConfig,
      versionCounter:          Cache.CacheConfig,
      containsCacheSize:       Long = 16384
    )

    object Cache {
      case class CacheConfig(maximumEntries: Long, ttl: Option[FiniteDuration])
    }

    case class Ntp(server: String, refreshInterval: FiniteDuration, timeout: FiniteDuration)

    case class VersionInfo(enable: Boolean, uri: String, period: FiniteDuration)

  }

  case class Indexer(
    enable:            Boolean,
    orientDbDirectory: String,
    orientDbPassword:  String,
    ttlCacheCheck:     Duration
  )

  case class Kamon(enable: Boolean)
}
// $COVERAGE-ON$
