package org.plasmalabs.node

import cats.Show
import cats.implicits._
import com.google.protobuf.ByteString
import com.typesafe.config.Config
import monocle._
import monocle.macros._
import pureconfig._
import pureconfig.configurable._
import pureconfig.generic.ProductHint
import pureconfig.generic.auto._
import scodec.bits.ByteVector
import org.plasmalabs.config.ApplicationConfig
import org.plasmalabs.config.ApplicationConfig.Node
import org.plasmalabs.config.ApplicationConfig.Node.KnownPeer
import org.plasmalabs.consensus.models.{BlockId, StakingAddress}
import org.plasmalabs.models._
import org.plasmalabs.models.utility._
import org.plasmalabs.sdk.codecs.AddressCodecs.decodeAddress
import org.plasmalabs.sdk.models.LockAddress
import org.plasmalabs.sdk.utils.Encoding

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
      cmdArgs.runtime.testnetArgs.regtest.value
    ) {
      val bigBangConfig =
        simpleArgApplications.node.bigBang match {
          case p: Node.BigBangs.Private =>
            p.copy(
              timestamp = cmdArgs.runtime.testnetArgs.testnetTimestamp.getOrElse(p.timestamp),
              stakerCount = cmdArgs.runtime.testnetArgs.testnetStakerCount.getOrElse(p.stakerCount),
              localStakerIndex = cmdArgs.runtime.testnetArgs.testnetStakerIndex.orElse(p.localStakerIndex),
              regtestEnabled = cmdArgs.runtime.testnetArgs.regtest.value
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

  implicit val showApplicationConfig: Show[ApplicationConfig] = {
    val base = Show.fromToString[ApplicationConfig]
    val sanitizer = GenLens[ApplicationConfig](_.indexer.orientDbPassword).replace("SANITIZED")
    conf => base.show(sanitizer(conf))
  }
}
// $COVERAGE-ON$
