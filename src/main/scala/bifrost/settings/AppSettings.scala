package bifrost.settings

import java.io.File
import java.net.InetSocketAddress

import bifrost.utils.{Logging, NetworkTimeProviderSettings}
import com.typesafe.config.{Config, ConfigFactory}
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._

import scala.concurrent.duration._

case class RESTApiSettings(bindAddress: InetSocketAddress,
                           apiKeyHash: Option[String],
                           corsAllowedOrigin: Option[String],
                           timeout: FiniteDuration)

case class NetworkSettings(addedMaxDelay: Option[FiniteDuration],
                           agentName: String,
                           appVersion: String,
                           bindAddress: InetSocketAddress,
                           connectionTimeout: FiniteDuration,
                           controllerTimeout: Option[FiniteDuration],
                           declaredAddress: Option[InetSocketAddress],
                           deliveryTimeout: FiniteDuration,
                           desiredInvObjects: Int,
                           getPeersInterval: FiniteDuration,
                           handshakeTimeout: FiniteDuration,
                           knownPeers: Seq[InetSocketAddress],
                           localOnly: Boolean,
                           magicBytes: Array[Byte],
                           maxConnections: Int,
                           maxDeliveryChecks: Int,
                           maxHandshakeSize: Int,
                           maxInvObjects: Int,
                           maxModifiersCacheSize: Int,
                           maxPacketSize: Int,
                           maxPeerSpecObjects: Int,
                           nodeName: String,
                           penaltySafeInterval: FiniteDuration,
                           penaltyScoreThreshold: Int,
                           syncInterval: FiniteDuration,
                           syncIntervalStable: FiniteDuration,
                           syncStatusRefresh: FiniteDuration,
                           syncStatusRefreshStable: FiniteDuration,
                           syncTimeout: Option[FiniteDuration],
                           temporalBanDuration: FiniteDuration,
                           upnpDiscoverTimeout: Option[FiniteDuration],
                           upnpEnabled: Boolean,
                           upnpGatewayTimeout: Option[FiniteDuration])

case class ForgingSettings(offlineGeneration: Boolean,
                           posAttachmentSize: Int,
                           targetBlockTime: Long,
                           blockGenerationDelay: Long,
                           version: Byte,
                           forkHeight: Long)

case class AppSettings(walletSeed: String,
                       keyFileDir: Option[String],
                       walletDir: Option[String],
                       dataDir: Option[String],
                       pbrDir: Option[String],
                       tbrDir: Option[String],
                       logDir: Option[String],
                       rpcPort: Int,
                       cors: Boolean,
                       verboseAPI: Boolean,
                       version: Version,
                       network: NetworkSettings,
                       forgingSettings: ForgingSettings,
                       restApi: RESTApiSettings,
                       ntp: NetworkTimeProviderSettings)

object AppSettings extends Logging with SettingsReaders {

  protected val configPath: String = "bifrost"

  def readConfig(path: Option[String], configPath: String): Config = {

    val fileOpt: Option[File] = path.map(fileName ⇒ new File(fileName)).filter(_.exists())
      .orElse(path.flatMap(fileName ⇒ Option(getClass.getClassLoader.getResource(fileName)))
        .map(r ⇒ new File(r.toURI)).filter(_.exists()))

    val config = fileOpt match {
      case None ⇒
        log.warn("No configuration file was provided, using default configuration")
        ConfigFactory.load()
      case Some(file) ⇒
        val configFile = ConfigFactory.parseFile(file)
        if(!configFile.hasPath(configPath)) {
          throw new Error("Configuration file does not contain Bifrost settings object")
        }
        ConfigFactory
        .defaultOverrides()
        .withFallback(configFile)
        .withFallback(ConfigFactory.defaultApplication())
        .withFallback(ConfigFactory.defaultReference())
        .resolve()
    }

    config
  }

  def read(userConfigPath: Option[String]): AppSettings = {
    fromConfig(readConfig(userConfigPath, configPath))
  }

  def fromConfig(config: Config): AppSettings = {
    config.as[AppSettings](configPath)
  }
}