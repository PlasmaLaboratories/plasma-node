package org.plasmalabs.byzantine.util

import cats.Applicative
import cats.effect._
import cats.effect.implicits._
import cats.implicits._
import com.spotify.docker.client.messages.{ContainerConfig, HostConfig, NetworkConfig, NetworkCreation}
import com.spotify.docker.client.{DefaultDockerClient, DockerClient}
import fs2.io.file.{Files, Path}
import org.plasmalabs.buildinfo.node.BuildInfo
import org.plasmalabs.consensus.models.StakingAddress
import org.plasmalabs.typeclasses.implicits._

import java.time.Instant
import scala.jdk.CollectionConverters._

trait DockerSupport[F[_]] {

  def createNode(
    name:          String,
    nodeGroupName: String,
    config:        TestNodeConfig
  ): Resource[F, DockerNode]
}

object DockerSupport {

  private def loggingEnabledFromEnvironment: Boolean = {
    import scala.jdk.CollectionConverters._
    val env = System.getenv().asScala
    env
      .get("ACTIONS_STEP_DEBUG") // GitHub Actions will set this env variable if "Re-Run with Debug Logging" is selected
      .orElse(env.get("DEBUG")) // This is just a general-purpose environment variable
      .exists(_.toBoolean)
  }

  def make[F[_]: Async](
    containerLogsDirectory: Option[Path] = Some(Path("byzantine-it") / "target" / "logs"),
    debugLoggingEnabled:    Boolean = loggingEnabledFromEnvironment
  ): Resource[F, (DockerSupport[F], DockerClient)] =
    for {
      given DockerClient <- Resource.make(Sync[F].blocking(DefaultDockerClient.fromEnv().build()))(c =>
        Sync[F].blocking(c.close())
      )
      nodeCache <- Resource.make[F, Ref[F, Set[DockerNode]]](Ref.of(Set.empty[DockerNode]))(
        _.get.flatMap(
          _.toList
            .traverse(node =>
              Sync[F]
                .blocking(
                  summon[DockerClient].removeContainer(node.containerId, DockerClient.RemoveContainerParam.forceKill)
                )
                .voidError
            )
            .void
        )
      )
      networkCache <- Resource.make[F, Ref[F, Set[NetworkCreation]]](Ref.of(Set.empty[NetworkCreation]))(
        _.get.flatMap(
          _.toList
            .traverse(network => Sync[F].blocking(summon[DockerClient].removeNetwork(network.id())).voidError)
            .void
        )
      )
      _ <- containerLogsDirectory.fold(Resource.unit[F])(Files.forAsync[F].createDirectories(_).toResource)
      dockerSupport = new Impl[F](containerLogsDirectory, debugLoggingEnabled, nodeCache, networkCache)
    } yield (dockerSupport, summon[DockerClient])

  private class Impl[F[_]: Async](
    containerLogsDirectory: Option[Path],
    debugLoggingEnabled:    Boolean,
    nodeCache:              Ref[F, Set[DockerNode]],
    networkCache:           Ref[F, Set[NetworkCreation]]
  )(implicit dockerClient: DockerClient)
      extends DockerSupport[F] {

    def createNode(name: String, nodeGroupName: String, config: TestNodeConfig): Resource[F, DockerNode] =
      for {
        node <- Resource.make(createContainer(name, nodeGroupName, config))(node =>
          Sync[F].defer(node.stop[F]) >>
          containerLogsDirectory
            .map(_ / s"$name-${node.containerId.take(8)}.log")
            .fold(Applicative[F].unit)(node.saveContainerLogs[F]) >>
          deleteContainer(node)
        )
        _ <- Resource.onFinalize(Sync[F].defer(nodeCache.update(_ - node)))
        _ <- node.configure(config.yaml).toResource
      } yield node

    private def createContainer(
      name:          String,
      nodeGroupName: String,
      config:        TestNodeConfig
    ): F[DockerNode] = {
      val networkNamePrefix: String = "node-it"
      for {
        networkName <- (networkNamePrefix + nodeGroupName).pure[F]
        environment = Map("NODE_LOG_LEVEL" -> (if (debugLoggingEnabled) "DEBUG" else "INFO"))
        containerConfig   <- buildContainerConfig(name, environment, config).pure[F]
        containerCreation <- Sync[F].blocking(dockerClient.createContainer(containerConfig, name))
        node              <- DockerNode(containerCreation.id(), name, config).pure[F]
        _                 <- nodeCache.update(_ + node)
        networkId <- Sync[F].blocking(dockerClient.listNetworks().asScala.find(_.name == networkName)).flatMap {
          case Some(network) => network.id().pure[F]
          case None =>
            Sync[F]
              .blocking(dockerClient.createNetwork(NetworkConfig.builder().name(networkName).build()))
              .flatTap(n => networkCache.update(_ + n))
              .map(_.id())
        }
        _ <- Sync[F].blocking(dockerClient.connectToNetwork(node.containerId, networkId))
      } yield node
    }

    private def deleteContainer(node: DockerNode): F[Unit] =
      Sync[F].blocking(
        dockerClient.removeContainer(node.containerId, DockerClient.RemoveContainerParam.forceKill)
      )

    private def buildContainerConfig(
      name:        String,
      environment: Map[String, String],
      config:      TestNodeConfig
    ): ContainerConfig = {
      val nodeImage: String = s"stratalab/plasma-node:${BuildInfo.version}"
      val exposedPorts: Seq[String] = List(config.rpcPort, config.p2pPort, config.jmxRemotePort).map(_.toString)
      val env =
        environment.toList.map { case (key, value) => s"$key=$value" }

      val cmd =
        List(
          s"-Dcom.sun.management.jmxremote.port=${config.jmxRemotePort}",
          s"-Dcom.sun.management.jmxremote.rmi.port=${config.jmxRemotePort}",
          "-Dcom.sun.management.jmxremote.ssl=false",
          "-Dcom.sun.management.jmxremote.local.only=false",
          "-Dcom.sun.management.jmxremote.authenticate=false",
          "--logbackFile",
          "/node/config/logback.xml",
          "--debug"
        )

      val hostConfig =
        config.stakingBindSourceDir
          .foldLeft(HostConfig.builder().privileged(true))((b, sourceDir) =>
            b.appendBinds(
              HostConfig.Bind
                .builder()
                .from(sourceDir)
                .to("/node-staking")
                .selinuxLabeling(true)
                .build()
            )
          )
          .build()

      ContainerConfig
        .builder()
        .image(nodeImage)
        .env(env*)
        .cmd(cmd*)
        .hostname(name)
        .hostConfig(hostConfig)
        .exposedPorts(exposedPorts*)
        .build()
    }
  }
}

case class TestNodeConfig(
  bigBangTimestamp:     Instant = Instant.now().plusSeconds(5),
  stakerCount:          Int = 1,
  localStakerIndex:     Int = 0,
  knownPeers:           List[String] = Nil,
  stakes:               Option[List[BigInt]] = None,
  rpcPort:              Int = 9084,
  p2pPort:              Int = 9085,
  jmxRemotePort:        Int = 9083,
  indexerEnabled:       Boolean = false,
  stakingBindSourceDir: Option[String] = None,
  serverHost:           Option[String] = None,
  serverPort:           Option[Int] = None,
  stakingAddress:       Option[StakingAddress] = None
) {

  def yaml: String = {
    val stakesStr = stakes.fold("")(
      _.map(v => s"\"$v\"").mkString("stakes: [", ",", "]")
    )
    s"""
       |node:
       |  rpc:
       |    bind-host: 0.0.0.0
       |    port: "$rpcPort"
       |  staking:
       |    staking-address: ${stakingAddress.fold("")(_.show)}
       |  p2p:
       |    bind-host: 0.0.0.0
       |    ${serverHost.map(sh => s"public-host: $sh").getOrElse("")}
       |    port: "$p2pPort"
       |    ${serverPort.map(sp => s"public-port: $sp").getOrElse("")}
       |    known-peers: "${knownPeers.map(p => s"$p:9085").mkString(",")}"
       |  big-bang:
       |    staker-count: $stakerCount
       |    local-staker-index: $localStakerIndex
       |    timestamp: ${bigBangTimestamp.toEpochMilli}
       |    $stakesStr
       |  protocols:
       |    0:
       |      slot-duration: 500 milli
       |      chain-selection-k-lookback: 6
       |      operational-periods-per-epoch: 2
       |indexer:
       |  enable: "$indexerEnabled"
       |""".stripMargin
  }

}

object TestNodeConfig {
  val epochSlotLength: Long = 150 // See org.plasmalabs.node.ApplicationConfig.Node.Protocol
}
