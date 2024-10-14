package xyz.stratalab.node

import cats.Show
import cats.implicits._
import co.topl.brambl.models.LockAddress
import co.topl.consensus.models.{BlockId, StakingAddress}
import com.google.protobuf.ByteString
import com.typesafe.config.Config
import monocle._
import monocle.macros._
import pureconfig._
import pureconfig.configurable._
import pureconfig.error.{CannotConvert, KeyNotFound}
import pureconfig.generic.ProductHint
import pureconfig.generic.semiauto._
import scodec.bits.ByteVector
import xyz.stratalab.config.ApplicationConfig
import xyz.stratalab.config.ApplicationConfig.Bifrost
import xyz.stratalab.config.ApplicationConfig.Bifrost.{BigBangs, KnownPeer}
import xyz.stratalab.models._
import xyz.stratalab.models.utility._
import xyz.stratalab.sdk.codecs.AddressCodecs.decodeAddress
import xyz.stratalab.sdk.utils.Encoding

import scala.concurrent.duration.{FiniteDuration, _}
import scala.jdk.CollectionConverters._
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
        cmdArgs.runtime.dataDir.map(createF(GenLens[ApplicationConfig](_.bifrost.data.directory))),
        cmdArgs.runtime.databaseType.map(createF(GenLens[ApplicationConfig](_.bifrost.data.databaseType))),
        cmdArgs.runtime.stakingArgs.stakingDir.map(createF(GenLens[ApplicationConfig](_.bifrost.staking.directory))),
        cmdArgs.runtime.stakingArgs.rewardAddress.map(
          createF(GenLens[ApplicationConfig](_.bifrost.staking.rewardAddress))
        ),
        cmdArgs.runtime.stakingArgs.stakingAddress.map(v =>
          createF(GenLens[ApplicationConfig](_.bifrost.staking.stakingAddress))(v.some)
        ),
        cmdArgs.runtime.rpcBindHost.map(createF(GenLens[ApplicationConfig](_.bifrost.rpc.bindHost))),
        cmdArgs.runtime.rpcBindPort.map(createF(GenLens[ApplicationConfig](_.bifrost.rpc.bindPort))),
        cmdArgs.runtime.p2pBindHost.map(createF(GenLens[ApplicationConfig](_.bifrost.p2p.bindHost))),
        cmdArgs.runtime.p2pBindPort.map(createF(GenLens[ApplicationConfig](_.bifrost.p2p.bindPort))),
        cmdArgs.runtime.p2pPublicHost.map(v => createF(GenLens[ApplicationConfig](_.bifrost.p2p.publicHost))(v.some)),
        cmdArgs.runtime.p2pPublicPort.map(v => createF(GenLens[ApplicationConfig](_.bifrost.p2p.publicPort))(v.some)),
        cmdArgs.runtime.knownPeers
          .map(parseKnownPeers)
          .map(createF(GenLens[ApplicationConfig](_.bifrost.p2p.knownPeers))),
        cmdArgs.runtime.genusArgs.orientDbDir.map(createF(GenLens[ApplicationConfig](_.genus.orientDbDirectory))),
        cmdArgs.runtime.genusArgs.orientDbPassword.map(createF(GenLens[ApplicationConfig](_.genus.orientDbPassword)))
      ).flatten
        .foldLeft(
          if (cmdArgs.runtime.genusArgs.disableGenus.value)
            createF(GenLens[ApplicationConfig](_.genus.enable))(false)(base)
          else
            base
        ) { case (appConf, f) => f(appConf) }
    if (
      cmdArgs.runtime.testnetArgs.testnetTimestamp.nonEmpty ||
      cmdArgs.runtime.testnetArgs.testnetStakerCount.nonEmpty ||
      cmdArgs.runtime.testnetArgs.testnetStakerIndex.nonEmpty ||
      cmdArgs.runtime.testnetArgs.regtest.value
    ) {
      val bigBangConfig =
        simpleArgApplications.bifrost.bigBang match {
          case p: Bifrost.BigBangs.Private =>
            p.copy(
              timestamp = cmdArgs.runtime.testnetArgs.testnetTimestamp.getOrElse(p.timestamp),
              stakerCount = cmdArgs.runtime.testnetArgs.testnetStakerCount.getOrElse(p.stakerCount),
              localStakerIndex = cmdArgs.runtime.testnetArgs.testnetStakerIndex.orElse(p.localStakerIndex),
              regtestEnabled = cmdArgs.runtime.testnetArgs.regtest.value
            )
          case p => p
        }
      GenLens[ApplicationConfig](_.bifrost.bigBang).replace(bigBangConfig)(simpleArgApplications)
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

  implicit val bifrostProductHint: ProductHint[Bifrost] =
    ProductHint[Bifrost](ConfigFieldMapping {
      case "p2p" => "p2p"
      case v     => defaultConfigFieldMapping(v)
    })

  implicit val applicationConfigReader: ConfigReader[ApplicationConfig] = deriveReader[ApplicationConfig]

  implicit val bifrostConfigReader: ConfigReader[ApplicationConfig.Bifrost] = deriveReader[ApplicationConfig.Bifrost]

  implicit val bifrostDataConfigReader: ConfigReader[ApplicationConfig.Bifrost.Data] =
    deriveReader[ApplicationConfig.Bifrost.Data]

  implicit val bifrostStakingConfigReader: ConfigReader[ApplicationConfig.Bifrost.Staking] =
    deriveReader[ApplicationConfig.Bifrost.Staking]

  implicit val bifrostP2PConfigReader: ConfigReader[ApplicationConfig.Bifrost.P2P] =
    deriveReader[ApplicationConfig.Bifrost.P2P]

  //  max items 22 allowed, current is 28
  // Note, be careful with kebac case notations, where p2p is involved, example. aggressive-p2-p
  // format: off
  implicit val bifrostNetworkPropertiesConfigReader: ConfigReader[ApplicationConfig.Bifrost.NetworkProperties] = {
    val defaultNetworkProperties = ApplicationConfig.Bifrost.NetworkProperties()
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
        ApplicationConfig.Bifrost.NetworkProperties(
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

  implicit val bifrostRPCConfigReader: ConfigReader[ApplicationConfig.Bifrost.RPC] =
    deriveReader[ApplicationConfig.Bifrost.RPC]

  implicit val bifrostMempoolConfigReader: ConfigReader[ApplicationConfig.Bifrost.Mempool] =
    deriveReader[ApplicationConfig.Bifrost.Mempool]

  implicit val bifrostMempoolProtectionConfigReader: ConfigReader[ApplicationConfig.Bifrost.MempoolProtection] =
    deriveReader[ApplicationConfig.Bifrost.MempoolProtection]

  implicit val bigBangPrivateReader: ConfigReader[ApplicationConfig.Bifrost.BigBangs.Private] =
    deriveReader[ApplicationConfig.Bifrost.BigBangs.Private]

  implicit val bigBangPublicReader: ConfigReader[ApplicationConfig.Bifrost.BigBangs.Public] =
    deriveReader[ApplicationConfig.Bifrost.BigBangs.Public]

  implicit val bifrostBigBangConfigReader: ConfigReader[ApplicationConfig.Bifrost.BigBang] =
    deriveReader[ApplicationConfig.Bifrost.BigBang]

  implicit val bifrostProtocolConfigReader: ConfigReader[ApplicationConfig.Bifrost.Protocol] =
    deriveReader[ApplicationConfig.Bifrost.Protocol]

  implicit val bifrostCacheCacheConfigConfigReader: ConfigReader[ApplicationConfig.Bifrost.Cache.CacheConfig] =
    deriveReader[ApplicationConfig.Bifrost.Cache.CacheConfig]

  // max items 22, current is 23, a potential issue could be that containsCacheSize undefined here, could assign a default that defined on case class
  // format: off
  implicit val bifrostCacheConfigReader: ConfigReader[ApplicationConfig.Bifrost.Cache] =
    ConfigReader.fromCursor{ cur =>
      for {
        objCur <- cur.asObjectCursor
        parentChildTree <- objCur.atKey("parent-child-tree").flatMap(bifrostCacheCacheConfigConfigReader.from)
        slotData <- objCur.atKey("slot-data").flatMap(bifrostCacheCacheConfigConfigReader.from)
        headers <- objCur.atKey("headers").flatMap(bifrostCacheCacheConfigConfigReader.from)
        bodies <- objCur.atKey("bodies").flatMap(bifrostCacheCacheConfigConfigReader.from)
        transactions <- objCur.atKey("transactions").flatMap(bifrostCacheCacheConfigConfigReader.from)
        spendableBoxIds <- objCur.atKey("spendable-box-ids").flatMap(bifrostCacheCacheConfigConfigReader.from)
        epochBoundaries <- objCur.atKey("epoch-boundaries").flatMap(bifrostCacheCacheConfigConfigReader.from)
        operatorStakes <- objCur.atKey("operator-stakes").flatMap(bifrostCacheCacheConfigConfigReader.from)
        registrations <- objCur.atKey("registrations").flatMap(bifrostCacheCacheConfigConfigReader.from)
        blockHeightTree <- objCur.atKey("block-height-tree").flatMap(bifrostCacheCacheConfigConfigReader.from)
        eligibilities <- objCur.atKey("eligibilities").flatMap(bifrostCacheCacheConfigConfigReader.from)
        epochData <- objCur.atKey("epoch-data").flatMap(bifrostCacheCacheConfigConfigReader.from)
        registrationAccumulator <- objCur.atKey("registration-accumulator").flatMap(bifrostCacheCacheConfigConfigReader.from)
        txIdToBlockId <- objCur.atKey("tx-id-to-block-id").flatMap(bifrostCacheCacheConfigConfigReader.from)
        idToProposal <- objCur.atKey("id-to-proposal").flatMap(bifrostCacheCacheConfigConfigReader.from)
        epochToCreatedVersion <- objCur.atKey("epoch-to-created-version").flatMap(bifrostCacheCacheConfigConfigReader.from)
        versionVoting <- objCur.atKey("version-voting").flatMap(bifrostCacheCacheConfigConfigReader.from)
        epochToProposalIds <- objCur.atKey("epoch-to-proposal-ids").flatMap(bifrostCacheCacheConfigConfigReader.from)
        proposalVoting <- objCur.atKey("proposal-voting").flatMap(bifrostCacheCacheConfigConfigReader.from)
        epochToVersionIds <- objCur.atKey("epoch-to-version-ids").flatMap(bifrostCacheCacheConfigConfigReader.from)
        versionIdToProposal <- objCur.atKey("version-id-to-proposal").flatMap(bifrostCacheCacheConfigConfigReader.from)
        versionCounter <- objCur.atKey("version-counter").flatMap(bifrostCacheCacheConfigConfigReader.from)
        containsCacheSize <- objCur.atKey("contains-cache-size").flatMap(BasicReaders.longConfigReader.from).orElse(Right(16384L)) // this value should be the same than defined on case class
      } yield
        ApplicationConfig.Bifrost.Cache(
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

  implicit val bifrostNtpConfigReader: ConfigReader[ApplicationConfig.Bifrost.Ntp] =
    deriveReader[ApplicationConfig.Bifrost.Ntp]

  implicit val bifrostVersionInfoConfigReader: ConfigReader[ApplicationConfig.Bifrost.VersionInfo] =
    deriveReader[ApplicationConfig.Bifrost.VersionInfo]

  implicit val genusConfigReader: ConfigReader[ApplicationConfig.Genus] = deriveReader[ApplicationConfig.Genus]

  implicit val kamonConfigReader: ConfigReader[ApplicationConfig.Kamon] = deriveReader[ApplicationConfig.Kamon]

  implicit val showApplicationConfig: Show[ApplicationConfig] = {
    val base = Show.fromToString[ApplicationConfig]
    val sanitizer = GenLens[ApplicationConfig](_.genus.orientDbPassword).replace("SANITIZED")
    conf => base.show(sanitizer(conf))
  }
}
// $COVERAGE-ON$
