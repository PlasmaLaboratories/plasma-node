package org.plasmalabs.node.cli

import cats.ApplicativeThrow
import cats.data.OptionT
import cats.effect.Async
import cats.effect.std.{Console, Env}
import cats.implicits.*
import fs2.io.file.{Files, Path}
import io.circe.*
import io.circe.syntax.*
import org.plasmalabs.config.ApplicationConfig
import org.plasmalabs.consensus.models.{BlockId, StakingAddress}
import org.plasmalabs.node.AbstractNodeApp
import org.plasmalabs.sdk.models.LockAddress
import org.plasmalabs.typeclasses.implicits.*

import java.nio.charset.StandardCharsets

object ConfigureCommand {

  def apply[F[_]: Async: Console](appConfig: ApplicationConfig): StageResultT[F, Unit] =
    new ConfigureCommandImpl[F](appConfig).command

}

class ConfigureCommandImpl[F[_]: Async: Console](appConfig: ApplicationConfig) {

  private val intro: StageResultT[F, Unit] =
    writeMessage[F](
      "This utility helps configure your node." +
      " Unless otherwise specified, all prompts can be left blank to preserve the existing settings."
    )

  private val promptDataDir =
    writeMessage[F](
      s"Where should blockchain data be saved? (current=${appConfig.node.data.directory})"
    ) >>
    readInput[F].map(_.some.filterNot(_.isEmpty))

  /**
   * Returns: Tuple (optional directory, optional reward address, optional staking address)
   */
  private val promptStakingSettings: StageResultT[F, (Option[String], Option[LockAddress], Option[StakingAddress])] =
    readYesNo("Enable staking operations?", No.some)(
      ifYes = for {
        _ <- writeMessage[F](
          s"Where should staking data be saved? (current=${appConfig.node.staking.directory})"
        )
        stakingDir <- readInput[F].map(_.some.filterNot(_.isEmpty))
        rewardAddress <- readOptionalParameter[F, LockAddress](
          "Reward Address",
          List("ptetP7jshHTwEg9Fz9Xa1AmmzhYHDHo1zZRde7mnw3fddcXPjV14RPcgVgy7")
        )
        stakingAddress <- readOptionalParameter[F, StakingAddress](
          "Staking Address",
          List("28WBHmFRzamCRZYKp2QBBedy4Mnxu1Lj9gzMDAHsMcKF")
        )
      } yield (stakingDir, rewardAddress, stakingAddress),
      ifNo = (none[String], none[LockAddress], none[StakingAddress]).pure[StageResultT[F, *]]
    )

  /**
   * Returns Tuple (optional genesis ID, optional source-path)
   */
  private val promptGenesis: StageResultT[F, (Option[BlockId], Option[String])] =
    readYesNo("Configure genesis settings?", Yes.some)(
      ifYes = for {
        blockId <- readOptionalParameter[F, BlockId](
          "Genesis Block ID",
          List("b_EyNPwteBBfESqrLKPUQ3xRaxaDPESTTENKgMrkTMYGYa")
        )
        sourcePath <- readOptionalParameter[F, String](
          "Genesis Data Source",
          List("https://raw.githubusercontent.com/Topl/Genesis/main", "/home/alice/Downloads/testnet37")
        )
      } yield (blockId, sourcePath),
      ifNo = (none[BlockId], none[String]).pure[StageResultT[F, *]]
    )

  /**
   * Returns Tuple (rpc bind host, rpc bind port, enable indexer)
   */
  private val promptRpc =
    for {
      rpcHost <- readOptionalParameter[F, String](
        "RPC Bind Host",
        List("0.0.0.0", "localhost")
      )
      rpcPort <- readOptionalParameter[F, Int](
        "RPC Bind Port",
        List("9084", "8080")
      )
      enableIndexer <- readOptionalParameter[F, Boolean](
        "Enable Indexer",
        List("true", "false")
      )
    } yield (rpcHost, rpcPort, enableIndexer)

  /**
   * Returns Tuple (p2p expose server port, p2p bind host, p2p bind port, p2p known peers)
   */
  private val promptP2P =
    for {
      host <- readOptionalParameter[F, String](
        "P2P Bind Host",
        List("0.0.0.0", "localhost")
      )
      port <- readOptionalParameter[F, Int](
        "P2P Bind Port",
        List("9084")
      )
      publicHost <- readOptionalParameter[F, String](
        "P2P exposed server host",
        List(host.getOrElse("127.0.0.1"))
      )
      publicPort <- readOptionalParameter[F, Int](
        "P2P exposed server port",
        List(port.map(_.toString).getOrElse("9084"))
      )
      peers <-
        OptionT(
          readOptionalParameter[F, String](
            "P2P Known Peers",
            List("testnet.topl.co:9085,192.168.1.50:9085")
          )
        )
          .map(_.split(",").toList)
          .value
    } yield (host, port, publicHost, publicPort, peers)

  private val promptSettings =
    for {
      dataDir                                                         <- promptDataDir
      (stakingDir, stakingRewardAddress, stakingAddress)              <- promptStakingSettings
      (genesisBlockId, genesisSourcePath)                             <- promptGenesis
      (rpcHost, rpcPort, enableIndexer)                               <- promptRpc
      (p2pHost, p2pPort, p2pPublicHost, p2pPublicPort, p2pKnownPeers) <- promptP2P
    } yield ConfigureCommandInput(
      dataDir,
      stakingDir,
      stakingRewardAddress,
      stakingAddress,
      genesisBlockId,
      genesisSourcePath,
      rpcHost,
      rpcPort,
      enableIndexer,
      p2pHost,
      p2pPort,
      p2pPublicHost,
      p2pPublicPort,
      p2pKnownPeers
    )

  private def printConfig(configContents: String) =
    writeMessage[F]("Configuration contents:") >> writeMessage[F](configContents)

  private val promptDestination =
    StageResultT
      .liftF(OptionT(Env.make[F].get(AbstractNodeApp.ConfigFileEnvironmentVariable)).fold(Path("./user.yaml"))(Path(_)))
      .flatMap(default =>
        readDefaultedOptional[F, String]("Save location", List(default.toString, "/conf/app.yaml"), default.toString)
      )
      .map(Path(_))

  private def saveConfig(destination: Path, configContents: String): StageResultT[F, Unit] =
    StageResultT
      .liftF(Files.forAsync[F].exists(destination))
      .ifM(
        readYesNo("Destination file exists.  Merge-override contents?", Yes.some)(
          ifYes = StageResultT
            .liftF(Files.forAsync[F].readUtf8(destination).compile.foldMonoid)
            .flatMap(mergeConfigs(_, configContents)),
          ifNo = configContents.pure[StageResultT[F, *]]
        ),
        configContents.pure[StageResultT[F, *]]
      )
      .map(_.getBytes(StandardCharsets.UTF_8))
      .flatMap(
        writeFile(destination.parent.getOrElse(Path(".")))(_)(
          "Config File",
          destination.fileName.toString
        )
      )

  private def outro(configFile: Path): StageResultT[F, Unit] =
    writeMessage[F]("Configuration complete.") >>
    writeMessage[F](s"The node can be launched by passing the following argument at launch: --config $configFile")

  private val outroNoConfig =
    writeMessage[F]("No configuration required. The node can be launched without arguments.")

  private def mergeConfigs(existingConfigContents: String, newConfigContents: String): StageResultT[F, String] =
    StageResultT.liftF(
      ApplicativeThrow[F]
        .fromEither(
          (io.circe.yaml.parser.parse(existingConfigContents), io.circe.yaml.parser.parse(newConfigContents))
            .mapN(_.deepMerge(_))
        )
        .map(io.circe.yaml.printer.pretty)
    )

  val command: StageResultT[F, Unit] =
    for {
      _        <- intro
      settings <- promptSettings
      _ <- OptionT
        .fromOption[StageResultT[F, *]](settings.toYaml)
        .foldF(outroNoConfig)(configYaml =>
          for {
            _               <- printConfig(configYaml)
            saveDestination <- promptDestination
            _               <- saveConfig(saveDestination, configYaml)
            _               <- outro(saveDestination)
          } yield ()
        )
    } yield ()
}

private[cli] case class ConfigureCommandInput(
  dataDir:              Option[String],
  stakingDir:           Option[String],
  stakingRewardAddress: Option[LockAddress],
  stakingAddress:       Option[StakingAddress],
  genesisBlockId:       Option[BlockId],
  genesisSourcePath:    Option[String],
  rpcHost:              Option[String],
  rpcPort:              Option[Int],
  enableIndexer:        Option[Boolean],
  p2pBindHost:          Option[String],
  p2pBindPort:          Option[Int],
  p2pPublicHost:        Option[String],
  p2pPublicPort:        Option[Int],
  p2pKnownPeers:        Option[List[String]]
) {

  def toJson: Option[Json] =
    Json
      .obj(
        "node" -> Json
          .obj(
            "data" -> Json
              .obj(
                "directory" -> dataDir.asJson
              )
              .dropNullValues,
            "staking" -> Json
              .obj(
                "directory" -> stakingDir.asJson,
                "reward-address" -> stakingRewardAddress
                  .map(org.plasmalabs.sdk.codecs.AddressCodecs.encodeAddress)
                  .asJson,
                "staking-address" -> stakingAddress
                  .map(_.value.toByteArray)
                  .map(org.plasmalabs.sdk.utils.Encoding.encodeToBase58)
                  .asJson
              )
              .dropNullValues,
            "big-bang" -> Json
              .obj(
                "type"        -> genesisBlockId.void.orElse(genesisSourcePath.void).as("public").asJson,
                "genesis-id"  -> genesisBlockId.map(_.show).asJson,
                "source-path" -> genesisSourcePath.asJson
              )
              .dropNullValues,
            "rpc" -> Json
              .obj(
                "bind-host" -> rpcHost.asJson,
                "bind-port" -> rpcPort.asJson
              )
              .dropNullValues,
            "p2p" -> Json
              .obj(
                "bind-host"   -> p2pBindHost.asJson,
                "bind-port"   -> p2pBindPort.asJson,
                "public-host" -> p2pPublicHost.asJson,
                "public-port" -> p2pPublicPort.asJson,
                "known-peers" -> p2pKnownPeers.map(_.mkString(",")).asJson
              )
              .dropNullValues
          )
          .dropEmptyValues,
        "indexer" -> Json
          .obj(
            "enable" -> enableIndexer.asJson
          )
          .dropNullValues
      )
      .dropEmptyValues
      .asObject
      .filter(_.nonEmpty)
      .map(_.asJson)

  def toYaml: Option[String] =
    toJson.map(io.circe.yaml.printer.pretty)
}
