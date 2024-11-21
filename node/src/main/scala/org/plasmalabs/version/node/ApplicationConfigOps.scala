package org.plasmalabs.node

import cats.Show
import cats.implicits.*
import com.google.protobuf.ByteString
import com.typesafe.config.Config
import monocle.*
import monocle.macros.*
import org.plasmalabs.config.ApplicationConfig
import org.plasmalabs.config.ApplicationConfig.Node
import org.plasmalabs.config.ApplicationConfig.Node.BigBangs.RegtestConfig
import org.plasmalabs.config.ApplicationConfig.Node.KnownPeer
import org.plasmalabs.consensus.models.{BlockId, StakingAddress}
import org.plasmalabs.models.*
import org.plasmalabs.models.utility.*
import org.plasmalabs.sdk.codecs.AddressCodecs.decodeAddress
import org.plasmalabs.sdk.models.LockAddress
import org.plasmalabs.sdk.utils.Encoding
import pureconfig.*
import pureconfig.configurable.*
import pureconfig.generic.ProductHint
import pureconfig.generic.semiauto.*
import scodec.bits.ByteVector

import scala.concurrent.duration.{FiniteDuration, *}
import scala.jdk.CollectionConverters.*
import scala.util.Try

// $COVERAGE-OFF$

object ApplicationConfigOps {

  /**
   * Construct an ApplicationConfig based on the given command-line arguments and a merged HOCON config.
   *
   * May throw exceptions.
   */
  def unsafe(cmdArgs: Args, config: Config): ApplicationConfig = {
    val base = ConfigSource.fromConfig(config).loadOrThrow[ApplicationConfig]

    def createF[B](lens: Lens[ApplicationConfig, B])(value: B): ApplicationConfig => ApplicationConfig =
      (appConf: ApplicationConfig) => lens.replace(value)(appConf)

    val simpleArgApplications =
      List[Option[ApplicationConfig => ApplicationConfig]](
        cmdArgs.runtime.dataDir.map(createF(GenLens[ApplicationConfig](_.node.data.directory))),
        cmdArgs.runtime.databaseType.map(createF(GenLens[ApplicationConfig](_.node.data.databaseType))),
        cmdArgs.runtime.stakingArgs.stakingDir.map(createF(GenLens[ApplicationConfig](_.node.staking.directory))),
        cmdArgs.runtime.stakingArgs.rewardAddress.map(
          createF(GenLens[ApplicationConfig](_.node.staking.rewardAddress))
        ),
        cmdArgs.runtime.stakingArgs.stakingAddress.map(v =>
          createF(GenLens[ApplicationConfig](_.node.staking.stakingAddress))(v.some)
        ),
        cmdArgs.runtime.rpcBindHost.map(createF(GenLens[ApplicationConfig](_.node.rpc.bindHost))),
        cmdArgs.runtime.rpcBindPort.map(createF(GenLens[ApplicationConfig](_.node.rpc.bindPort))),
        cmdArgs.runtime.ethereumJsonRpcBindHost.map(
          createF(GenLens[ApplicationConfig](_.node.ethereumJsonRpc.bindHost))
        ),
        cmdArgs.runtime.ethereumJsonRpcBindPort.map(
          createF(GenLens[ApplicationConfig](_.node.ethereumJsonRpc.bindPort))
        ),
        cmdArgs.runtime.p2pBindHost.map(createF(GenLens[ApplicationConfig](_.node.p2p.bindHost))),
        cmdArgs.runtime.p2pBindPort.map(createF(GenLens[ApplicationConfig](_.node.p2p.bindPort))),
        cmdArgs.runtime.p2pPublicHost.map(v => createF(GenLens[ApplicationConfig](_.node.p2p.publicHost))(v.some)),
        cmdArgs.runtime.p2pPublicPort.map(v => createF(GenLens[ApplicationConfig](_.node.p2p.publicPort))(v.some)),
        cmdArgs.runtime.knownPeers
          .map(parseKnownPeers)
          .map(createF(GenLens[ApplicationConfig](_.node.p2p.knownPeers))),
        cmdArgs.runtime.indexerArgs.orientDbDir.map(createF(GenLens[ApplicationConfig](_.indexer.orientDbDirectory))),
        cmdArgs.runtime.indexerArgs.orientDbPassword.map(
          createF(GenLens[ApplicationConfig](_.indexer.orientDbPassword))
        )
      ).flatten
        .foldLeft(
          if (cmdArgs.runtime.indexerArgs.disableIndexer.value)
            createF(GenLens[ApplicationConfig](_.indexer.enable))(false)(base)
          else
            base
        ) { case (appConf, f) => f(appConf) }
    if (
      cmdArgs.runtime.testnetArgs.testnetTimestamp.nonEmpty ||
      cmdArgs.runtime.testnetArgs.testnetStakerCount.nonEmpty ||
      cmdArgs.runtime.testnetArgs.testnetStakerIndex.nonEmpty ||
      cmdArgs.runtime.testnetArgs.blockRegtestPermission.isDefined
    ) {
      val regTestConfig =
        cmdArgs.runtime.testnetArgs.blockRegtestPermission.map(rc => RegtestConfig(rc))
      val bigBangConfig =
        simpleArgApplications.node.bigBang match {
          case p: Node.BigBangs.Private =>
            p.copy(
              timestamp = cmdArgs.runtime.testnetArgs.testnetTimestamp.getOrElse(p.timestamp),
              stakerCount = cmdArgs.runtime.testnetArgs.testnetStakerCount.getOrElse(p.stakerCount),
              localStakerIndex = cmdArgs.runtime.testnetArgs.testnetStakerIndex.orElse(p.localStakerIndex),
              regtestConfig = regTestConfig
            )
          case p => p
        }
      GenLens[ApplicationConfig](_.node.bigBang).replace(bigBangConfig)(simpleArgApplications)
    } else {
      simpleArgApplications
    }
  }

  implicit val bigIntConfigReader: ConfigReader[BigInt] =
    ConfigReader.fromNonEmptyStringTry(str => Try(BigInt(str)))

  implicit val ratioConfigReader: ConfigReader[Ratio] =
    ConfigReader.fromNonEmptyStringTry { str =>
      Try {
        val Array(numeratorStr, denominatorStr) = str.split('/')
        Ratio(BigInt(numeratorStr), BigInt(denominatorStr))
      }
    }

  private val defaultConfigFieldMapping = ConfigFieldMapping(CamelCase, KebabCase)

  /**
   * Parses the given comma-delimited string of host:port combinations
   * i.e. "1.2.3.4:9095,5.6.7.8:9095"
   */
  private def parseKnownPeers(str: String): List[KnownPeer] =
    str.split(',').toList.filterNot(_.isEmpty).map { addr =>
      val Array(host, portStr) = addr.split(':')
      KnownPeer(host, portStr.toInt)
    }

  implicit val knownPeersReader: ConfigReader[List[KnownPeer]] =
    ConfigReader[String].emap(str =>
      Try(
        parseKnownPeers(str)
      ).toEither.leftMap(e => error.CannotConvert(str, "InetAddressList", e.getMessage))
    )

  implicit val lockAddressReader: ConfigReader[LockAddress] =
    ConfigReader[String].emap(str =>
      decodeAddress(str).leftMap(e => error.CannotConvert(str, "LockAddress", e.toString))
    )

  implicit val stakingAddressReader: ConfigReader[StakingAddress] =
    ConfigReader[String]
      .emap(str =>
        Encoding.decodeFromBase58(str).leftMap(_ => error.CannotConvert(str, "StakingAddress", "Not base58"))
      )
      .map(ByteString.copyFrom)
      .map(StakingAddress(_))

  implicit val blockIdReader: ConfigReader[BlockId] =
    ConfigReader[String]
      .map(str => if (str.startsWith("b_")) str.substring(2) else str)
      .emap(str =>
        ByteVector.fromBase58Descriptive(str).leftMap(_ => error.CannotConvert(str, "BlockId", "Not base58"))
      )
      .ensure(_.length == 32, _ => "Not length 32")
      .map(v => BlockId(v))

  implicit def slotMapReader[T: ConfigReader]: ConfigReader[Map[Slot, T]] =
    genericMapReader[Slot, T](v => v.toLongOption.toRight(error.CannotConvert(v, "Slot", "Not a long")))

  implicit val nodeProductHint: ProductHint[Node] =
    ProductHint[Node](ConfigFieldMapping {
      case "p2p" => "p2p"
      case v     => defaultConfigFieldMapping(v)
    })

  implicit val applicationConfigReader: ConfigReader[ApplicationConfig] = deriveReader[ApplicationConfig]

  implicit val nodeConfigReader: ConfigReader[ApplicationConfig.Node] = deriveReader[ApplicationConfig.Node]

  implicit val nodeDataConfigReader: ConfigReader[ApplicationConfig.Node.Data] =
    deriveReader[ApplicationConfig.Node.Data]

  implicit val nodeStakingConfigReader: ConfigReader[ApplicationConfig.Node.Staking] =
    deriveReader[ApplicationConfig.Node.Staking]

  implicit val nodeP2PConfigReader: ConfigReader[ApplicationConfig.Node.P2P] =
    deriveReader[ApplicationConfig.Node.P2P]

  //  max items 22 allowed, current is 28
  // Note, be careful with kebac case notations, where p2p is involved, example. aggressive-p2-p
  // format: off
  implicit val nodeNetworkPropertiesConfigReader: ConfigReader[ApplicationConfig.Node.NetworkProperties] = {
    val defaultNetworkProperties = ApplicationConfig.Node.NetworkProperties()
    ConfigReader.fromCursor{ cur =>
      for {
        objCur <- cur.asObjectCursor
        useHostNames <- objCur.atKey("use-host-names").flatMap(BasicReaders.booleanConfigReader.from).orElse(Right(defaultNetworkProperties.useHostNames))
        pingPongInterval <- objCur.atKey("ping-pong-interval").flatMap(BasicReaders.finiteDurationConfigReader.from).orElse(Right(defaultNetworkProperties.pingPongInterval))
        expectedSlotsPerBlock <- objCur.atKey("expected-slots-per-block").flatMap(BasicReaders.doubleConfigReader.from).orElse(Right(defaultNetworkProperties.expectedSlotsPerBlock))
        maxPerformanceDelayInSlots <- objCur.atKey("max-performance-delay-in-slots").flatMap(BasicReaders.doubleConfigReader.from).orElse(Right(defaultNetworkProperties.maxPerformanceDelayInSlots))
        remotePeerNoveltyInExpectedBlocks <- objCur.atKey("remote-peer-novelty-in-expected-blocks").flatMap(BasicReaders.doubleConfigReader.from).orElse(Right(defaultNetworkProperties.remotePeerNoveltyInExpectedBlocks))
        minimumBlockProvidingReputationPeers <- objCur.atKey("minimum-block-providing-reputation-peers").flatMap(BasicReaders.intConfigReader.from).orElse(Right(defaultNetworkProperties.minimumBlockProvidingReputationPeers))
        minimumPerformanceReputationPeers <- objCur.atKey("minimum-performance-reputation-peers").flatMap(BasicReaders.intConfigReader.from).orElse(Right(defaultNetworkProperties.minimumPerformanceReputationPeers))
        txImpactRatio <- objCur.atKey("tx-impact-ratio").flatMap(BasicReaders.intConfigReader.from).orElse(Right(defaultNetworkProperties.txImpactRatio))
        minimumTxMempoolReputationPeers <- objCur.atKey("minimum-tx-mempool-reputation-peers").flatMap(BasicReaders.intConfigReader.from).orElse(Right(defaultNetworkProperties.minimumTxMempoolReputationPeers))
        minimumRequiredReputation <- objCur.atKey("minimum-required-reputation").flatMap(BasicReaders.doubleConfigReader.from).orElse(Right(defaultNetworkProperties.minimumRequiredReputation))
        minimumBlockProvidingReputation <- objCur.atKey("minimum-block-providing-reputation").flatMap(BasicReaders.intConfigReader.from).orElse(Right(defaultNetworkProperties.minimumBlockProvidingReputation))
        minimumEligibleColdConnections <- objCur.atKey("minimum-eligible-cold-connections").flatMap(BasicReaders.intConfigReader.from).orElse(Right(defaultNetworkProperties.minimumEligibleColdConnections ))
        maximumEligibleColdConnections <- objCur.atKey("maximum-eligible-cold-connections").flatMap(BasicReaders.intConfigReader.from).orElse(Right(defaultNetworkProperties.maximumEligibleColdConnections ))
        clearColdIfNotActiveForInMs <- objCur.atKey("clear-cold-if-not-active-for-in-ms").flatMap(BasicReaders.longConfigReader.from).orElse(Right(defaultNetworkProperties.clearColdIfNotActiveForInMs))
        minimumHotConnections  <- objCur.atKey("minimum-hot-connections").flatMap(BasicReaders.intConfigReader.from).orElse(Right(defaultNetworkProperties.minimumHotConnections))
        maximumWarmConnections  <- objCur.atKey("maximum-warm-connections").flatMap(BasicReaders.intConfigReader.from).orElse(Right(defaultNetworkProperties.maximumWarmConnections))
        warmHostsUpdateEveryNBlock <- objCur.atKey("warm-hosts-update-every-n-block").flatMap(BasicReaders.doubleConfigReader.from).orElse(Right(defaultNetworkProperties.warmHostsUpdateEveryNBlock))
        p2pTrackInterval <- objCur.atKey("p2p-track-interval").flatMap(BasicReaders.finiteDurationConfigReader.from).orElse(Right(defaultNetworkProperties.p2pTrackInterval))
        closeTimeoutFirstDelayInMs <- objCur.atKey("close-timeout-first-delay-in-ms").flatMap(BasicReaders.longConfigReader.from).orElse(Right(defaultNetworkProperties.closeTimeoutFirstDelayInMs))
        closeTimeoutWindowInMs <- objCur.atKey("close-timeout-window-in-ms").flatMap(BasicReaders.longConfigReader.from).orElse(Right(defaultNetworkProperties.closeTimeoutWindowInMs))
        aggressiveP2P <- objCur.atKey("aggressive-p2-p").flatMap(BasicReaders.booleanConfigReader.from).orElse(Right(defaultNetworkProperties.aggressiveP2P))
        aggressiveP2PCount <- objCur.atKey("aggressive-p2-p-count").flatMap(BasicReaders.intConfigReader.from).orElse(Right(defaultNetworkProperties.aggressiveP2PCount))
        aggressiveP2PMaxCloseEvent <- objCur.atKey("aggressive-p2-p-max-close-event").flatMap(BasicReaders.intConfigReader.from).orElse(Right(defaultNetworkProperties.aggressiveP2PMaxCloseEvent))
        defaultTimeout <- objCur.atKey("default-timeout").flatMap(BasicReaders.finiteDurationConfigReader.from).orElse(Right(defaultNetworkProperties.defaultTimeout))
        chunkSize<- objCur.atKey("chunk-size").flatMap(BasicReaders.intConfigReader.from).orElse(Right(defaultNetworkProperties.chunkSize))
        doNotExposeIps <- objCur.atKey("do-not-expose-ips").flatMap(BasicReaders.configListConfigReader.from).map(_.unwrapped.asInstanceOf[java.util.List[String]]).map(_.asScala.toList).orElse(Right(defaultNetworkProperties.doNotExposeIps))
        doNotExposeIds <- objCur.atKey("do-not-expose-ids").flatMap(BasicReaders.configListConfigReader.from).map(_.unwrapped.asInstanceOf[java.util.List[String]]).map(_.asScala.toList).orElse(Right(defaultNetworkProperties.doNotExposeIds))
        slotDataParentDepth <- objCur.atKey("slot-data-parent-depth").flatMap(BasicReaders.intConfigReader.from).orElse(Right(defaultNetworkProperties.slotDataParentDepth))

      } yield
        ApplicationConfig.Node.NetworkProperties(
          useHostNames = useHostNames,
          pingPongInterval = pingPongInterval,
          expectedSlotsPerBlock = expectedSlotsPerBlock,
          maxPerformanceDelayInSlots = maxPerformanceDelayInSlots,
          remotePeerNoveltyInExpectedBlocks = remotePeerNoveltyInExpectedBlocks,
          minimumBlockProvidingReputationPeers = minimumBlockProvidingReputationPeers,
          minimumPerformanceReputationPeers = minimumPerformanceReputationPeers,
          txImpactRatio= txImpactRatio,
          minimumTxMempoolReputationPeers= minimumTxMempoolReputationPeers,
          minimumRequiredReputation= minimumRequiredReputation,
          minimumEligibleColdConnections= minimumEligibleColdConnections,
          maximumEligibleColdConnections= maximumEligibleColdConnections,
          clearColdIfNotActiveForInMs=clearColdIfNotActiveForInMs ,
          minimumHotConnections=minimumHotConnections ,
          maximumWarmConnections= maximumWarmConnections,
          warmHostsUpdateEveryNBlock=warmHostsUpdateEveryNBlock ,
          p2pTrackInterval= p2pTrackInterval,
          closeTimeoutFirstDelayInMs=closeTimeoutFirstDelayInMs ,
          closeTimeoutWindowInMs= closeTimeoutWindowInMs,
          aggressiveP2P= aggressiveP2P,
          aggressiveP2PCount= aggressiveP2PCount,
          aggressiveP2PMaxCloseEvent= aggressiveP2PMaxCloseEvent,
          defaultTimeout= defaultTimeout,
          chunkSize= chunkSize,
          doNotExposeIps= doNotExposeIps,
          doNotExposeIds= doNotExposeIds,
          slotDataParentDepth= slotDataParentDepth
        )
    }
  }
  // format: on

  implicit val nodeRPCConfigReader: ConfigReader[ApplicationConfig.Node.RPC] =
    deriveReader[ApplicationConfig.Node.RPC]

  implicit val nodeMempoolConfigReader: ConfigReader[ApplicationConfig.Node.Mempool] =
    deriveReader[ApplicationConfig.Node.Mempool]

  implicit val nodeMempoolProtectionConfigReader: ConfigReader[ApplicationConfig.Node.MempoolProtection] =
    deriveReader[ApplicationConfig.Node.MempoolProtection]

  implicit val bigBangPrivateReader: ConfigReader[ApplicationConfig.Node.BigBangs.Private] =
    deriveReader[ApplicationConfig.Node.BigBangs.Private]

  implicit val bigBangRegtestConfigReader: ConfigReader[ApplicationConfig.Node.BigBangs.RegtestConfig] =
    deriveReader[ApplicationConfig.Node.BigBangs.RegtestConfig]

  implicit val bigBangPublicReader: ConfigReader[ApplicationConfig.Node.BigBangs.Public] =
    deriveReader[ApplicationConfig.Node.BigBangs.Public]

  implicit val nodeBigBangConfigReader: ConfigReader[ApplicationConfig.Node.BigBang] =
    deriveReader[ApplicationConfig.Node.BigBang]

  implicit val nodeProtocolConfigReader: ConfigReader[ApplicationConfig.Node.Protocol] =
    deriveReader[ApplicationConfig.Node.Protocol]

  implicit val nodeCacheCacheConfigConfigReader: ConfigReader[ApplicationConfig.Node.Cache.CacheConfig] =
    deriveReader[ApplicationConfig.Node.Cache.CacheConfig]

  // max items 22, current is 23, a potential issue could be that containsCacheSize undefined here, could assign a default that defined on case class
  // format: off
  implicit val nodeCacheConfigReader: ConfigReader[ApplicationConfig.Node.Cache] =
    ConfigReader.fromCursor{ cur =>
      for {
        objCur <- cur.asObjectCursor
        parentChildTree <- objCur.atKey("parent-child-tree").flatMap(nodeCacheCacheConfigConfigReader.from)
        slotData <- objCur.atKey("slot-data").flatMap(nodeCacheCacheConfigConfigReader.from)
        headers <- objCur.atKey("headers").flatMap(nodeCacheCacheConfigConfigReader.from)
        bodies <- objCur.atKey("bodies").flatMap(nodeCacheCacheConfigConfigReader.from)
        transactions <- objCur.atKey("transactions").flatMap(nodeCacheCacheConfigConfigReader.from)
        spendableBoxIds <- objCur.atKey("spendable-box-ids").flatMap(nodeCacheCacheConfigConfigReader.from)
        epochBoundaries <- objCur.atKey("epoch-boundaries").flatMap(nodeCacheCacheConfigConfigReader.from)
        operatorStakes <- objCur.atKey("operator-stakes").flatMap(nodeCacheCacheConfigConfigReader.from)
        registrations <- objCur.atKey("registrations").flatMap(nodeCacheCacheConfigConfigReader.from)
        blockHeightTree <- objCur.atKey("block-height-tree").flatMap(nodeCacheCacheConfigConfigReader.from)
        eligibilities <- objCur.atKey("eligibilities").flatMap(nodeCacheCacheConfigConfigReader.from)
        epochData <- objCur.atKey("epoch-data").flatMap(nodeCacheCacheConfigConfigReader.from)
        registrationAccumulator <- objCur.atKey("registration-accumulator").flatMap(nodeCacheCacheConfigConfigReader.from)
        txIdToBlockId <- objCur.atKey("tx-id-to-block-id").flatMap(nodeCacheCacheConfigConfigReader.from)
        idToProposal <- objCur.atKey("id-to-proposal").flatMap(nodeCacheCacheConfigConfigReader.from)
        epochToCreatedVersion <- objCur.atKey("epoch-to-created-version").flatMap(nodeCacheCacheConfigConfigReader.from)
        versionVoting <- objCur.atKey("version-voting").flatMap(nodeCacheCacheConfigConfigReader.from)
        epochToProposalIds <- objCur.atKey("epoch-to-proposal-ids").flatMap(nodeCacheCacheConfigConfigReader.from)
        proposalVoting <- objCur.atKey("proposal-voting").flatMap(nodeCacheCacheConfigConfigReader.from)
        epochToVersionIds <- objCur.atKey("epoch-to-version-ids").flatMap(nodeCacheCacheConfigConfigReader.from)
        versionIdToProposal <- objCur.atKey("version-id-to-proposal").flatMap(nodeCacheCacheConfigConfigReader.from)
        versionCounter <- objCur.atKey("version-counter").flatMap(nodeCacheCacheConfigConfigReader.from)
        containsCacheSize <- objCur.atKey("contains-cache-size").flatMap(BasicReaders.longConfigReader.from).orElse(Right(16384L)) // this value should be the same than defined on case class
      } yield
        ApplicationConfig.Node.Cache(
          parentChildTree,
          slotData,
          headers,
          bodies,
          transactions,
          spendableBoxIds,
          epochBoundaries,
          operatorStakes,
          registrations,
          blockHeightTree,
          eligibilities,
          epochData,
          registrationAccumulator,
          txIdToBlockId,
          idToProposal,
          epochToCreatedVersion,
          versionVoting,
          epochToProposalIds,
          proposalVoting,
          epochToVersionIds,
          versionIdToProposal,
          versionCounter,
          containsCacheSize
      )
    }
  // format: on

  implicit val nodeNtpConfigReader: ConfigReader[ApplicationConfig.Node.Ntp] =
    deriveReader[ApplicationConfig.Node.Ntp]

  implicit val nodeVersionInfoConfigReader: ConfigReader[ApplicationConfig.Node.VersionInfo] =
    deriveReader[ApplicationConfig.Node.VersionInfo]

  implicit val genusConfigReader: ConfigReader[ApplicationConfig.Indexer] = deriveReader[ApplicationConfig.Indexer]

  implicit val kamonConfigReader: ConfigReader[ApplicationConfig.Kamon] = deriveReader[ApplicationConfig.Kamon]

  implicit val showApplicationConfig: Show[ApplicationConfig] = {
    val base = Show.fromToString[ApplicationConfig]
    val sanitizer = GenLens[ApplicationConfig](_.indexer.orientDbPassword).replace("SANITIZED")
    conf => base.show(sanitizer(conf))
  }
}
// $COVERAGE-ON$
