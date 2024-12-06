package org.plasmalabs.indexer

import cats.Show
import cats.effect.IO
import cats.implicits.*
import com.typesafe.config.Config
import kamon.Kamon
import mainargs.{Flag, ParserForClass, arg, main}
import org.plasmalabs.algebras.Stats
import org.plasmalabs.common.application.{ContainsDebugFlag, ContainsUserConfigs, IOBaseApp}
import org.plasmalabs.grpc.{Grpc, HealthCheckGrpc}
import org.plasmalabs.interpreters.KamonStatsRef
import org.plasmalabs.node.services.NodeRpcFs2Grpc
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import pureconfig.{ConfigSource, *}
import monocle.*
import monocle.macros.*

import scala.concurrent.duration.Duration

object IndexerApp
    extends IOBaseApp[IndexerArgs, IndexerApplicationConfig](
      createArgs = a => IO.delay(IndexerArgs.parserArgs.constructOrThrow(a)),
      createConfig = IOBaseApp.createTypesafeConfig(_, Option("INDEXER_CONFIG_FILE")),
      parseConfig = (args, conf) => IO.delay(IndexerApplicationConfig.unsafe(args, conf)),
      preInitFunction = config => IO.delay(if (config.kamon.enable) Kamon.init())
    ) {

  implicit val logger: Logger[F] = Slf4jLogger.getLoggerFromName("IndexerApp")

  override def run(cmdArgs: IndexerArgs, config: Config, appConfig: IndexerApplicationConfig): IO[Unit] = (
    for {
      _              <- Logger[F].info(show"Indexer args=$cmdArgs").toResource
      given Stats[F] <- KamonStatsRef.make[F]
      nodeRpcProxy <- NodeRpcProxy
        .make[IO](appConfig.nodeRpcHost, appConfig.nodeRpcPort, appConfig.nodeRpcTls)
        .flatMap(NodeRpcFs2Grpc.bindServiceResource[IO])
      indexer <-
        Indexer
          .make[F](
            appConfig.nodeRpcHost,
            appConfig.nodeRpcPort,
            appConfig.nodeRpcTls,
            appConfig.dataDir,
            appConfig.orientDbPassword,
            ttlCacheCheck = appConfig.ttlCacheCheck
          )
      indexerServices <-
        IndexerGrpc.Server.services(
          indexer.blockFetcher,
          indexer.transactionFetcher,
          indexer.vertexFetcher,
          indexer.valueFetcher,
          indexer.replicatorStatus
        )
      healthCheck <- IndexerHealthCheck.make[IO]().map(_.healthChecker).flatMap(HealthCheckGrpc.Server.services[IO])
      _ <- Grpc.Server.serve[IO](appConfig.rpcBindHost, appConfig.rpcBindPort)(
        nodeRpcProxy :: healthCheck ++ indexerServices
      )
      _ <- IO.whenA(appConfig.enableReplicator)(Replicator.stream(indexer).compile.drain).toResource
    } yield ()
  ).useForever
}

@main
case class IndexerArgs(startup: IndexerArgs.Startup, runtime: IndexerArgs.Runtime)

object IndexerArgs {

  @main
  case class Startup(
    @arg(
      doc = "Zero or more config files (.conf, .json, .yaml) to apply to the node." +
        "  Config files stack such that the last config file takes precedence." +
        "  To specify an internal resource, prefix the value with \"resource://\"."
    )
    config: List[String] = Nil,
    @arg(
      doc = "An optional flag to enable debug mode on this node."
    )
    debug: Flag
  )

  @main case class Runtime(
    @arg(doc = "The host to bind for the RPC layer (i.e. 0.0.0.0)")
    rpcBindHost: Option[String] = None,
    @arg(doc = "The port to bind for the RPC layer (i.e. 9084)")
    rpcBindPort: Option[Int] = None,
    @arg(doc = "The host for the Node RPC Client (i.e. localhost)")
    nodeRpcHost: Option[String] = None,
    @arg(doc = "The port for the Node RPC Client (i.e. 9084)")
    nodeRpcPort: Option[Int] = None,
    @arg(doc = "Flag indicating if TLS should be used when connecting to the node.")
    nodeRpcTls: Option[Boolean] = None,
    @arg(doc = "Directory to use for the local database")
    dataDir: Option[String] = None,
    @arg(doc = "The password to use when interacting with OrientDB")
    orientDbPassword: Option[String] = None,
    @arg(doc = "Flag indicating if data should be copied from the node to the local database")
    enableReplicator: Option[Boolean] = None,
    @arg(doc = "Ttl cache indexer rpc call sync with node check")
    ttlCacheCheck: Option[String] = None
  )

  implicit val parserStartupArgs: ParserForClass[Startup] =
    ParserForClass[Startup]

  implicit val parserRuntimeArgs: ParserForClass[Runtime] =
    ParserForClass[Runtime]

  implicit val parserArgs: ParserForClass[IndexerArgs] =
    ParserForClass[IndexerArgs]

  implicit val argsContainsUserConfigs: ContainsUserConfigs[IndexerArgs] =
    _.startup.config

  implicit val argsContainsDebugFlag: ContainsDebugFlag[IndexerArgs] =
    _.startup.debug.value

  implicit val showArgs: Show[IndexerArgs] =
    args =>
      show"IndexerApplicationConfig(" +
      show"rpcBindHost=${args.runtime.rpcBindHost}" +
      show"rpcBindPort=${args.runtime.rpcBindPort}" +
      show"nodeRpcHost=${args.runtime.nodeRpcHost}" +
      show"nodeRpcPort=${args.runtime.nodeRpcPort}" +
      show"dataDir=${args.runtime.dataDir}" +
      show"enableReplicator=${args.runtime.enableReplicator}" +
      show"ttlCacheCheck=${args.runtime.ttlCacheCheck}" +
      // NOTE: Do not show orientDbPassword
      show")"
}

case class IndexerApplicationConfig(
  rpcBindHost:      String = "0.0.0.0",
  rpcBindPort:      Int = 9084,
  nodeRpcHost:      String = "localhost",
  nodeRpcPort:      Int = 9084,
  nodeRpcTls:       Boolean = false,
  dataDir:          String,
  orientDbPassword: String,
  enableReplicator: Boolean = false,
  enableMetrics:    Boolean = false,
  ttlCacheCheck:    Duration,
  kamon:            KamonConfig
) derives ConfigReader

case class KamonConfig(enable: Boolean)

object IndexerApplicationConfig {

  def unsafe(args: IndexerArgs, config: Config): IndexerApplicationConfig = {

    val base = ConfigSource.fromConfig(config).loadOrThrow[IndexerApplicationConfig]
    println(config)
    def createF[B](lens: Lens[IndexerApplicationConfig, B])(
      value: B
    ): IndexerApplicationConfig => IndexerApplicationConfig =
      (appConf: IndexerApplicationConfig) => lens.replace(value)(appConf)

    val argsAsConfig =
      List[Option[IndexerApplicationConfig => IndexerApplicationConfig]](
        args.runtime.rpcBindHost.map(createF(GenLens[IndexerApplicationConfig](_.rpcBindHost))),
        args.runtime.rpcBindPort.map(createF(GenLens[IndexerApplicationConfig](_.rpcBindPort))),
        args.runtime.nodeRpcHost.map(createF(GenLens[IndexerApplicationConfig](_.nodeRpcHost))),
        args.runtime.nodeRpcPort.map(createF(GenLens[IndexerApplicationConfig](_.nodeRpcPort))),
        args.runtime.nodeRpcTls.map(createF(GenLens[IndexerApplicationConfig](_.nodeRpcTls))),
        args.runtime.dataDir.map(createF(GenLens[IndexerApplicationConfig](_.dataDir))),
        args.runtime.orientDbPassword.map(createF(GenLens[IndexerApplicationConfig](_.orientDbPassword))),
        args.runtime.enableReplicator.map(createF(GenLens[IndexerApplicationConfig](_.enableReplicator)))
      ).flatten.foldLeft(base)((appConf, f) => f(appConf))

    argsAsConfig
  }

  implicit val showApplicationConfig: Show[IndexerApplicationConfig] =
    config =>
      show"IndexerApplicationConfig(" +
      show"rpcBindHost=${config.rpcBindHost}" +
      show"rpcBindPort=${config.rpcBindPort}" +
      show"nodeRpcHost=${config.nodeRpcHost}" +
      show"nodeRpcPort=${config.nodeRpcPort}" +
      show"dataDir=${config.dataDir}" +
      show"enableReplicator=${config.enableReplicator}" +
      show"ttlCacheCheck=${config.ttlCacheCheck}" +
      // NOTE: Do not show orientDbPassword
      show")"
}
