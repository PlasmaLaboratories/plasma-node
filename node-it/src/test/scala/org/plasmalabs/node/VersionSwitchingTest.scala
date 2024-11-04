package org.plasmalabs.node

import cats.MonadThrow
import cats.effect._
import cats.effect.implicits._
import cats.effect.std.{Random, SecureRandom}
import cats.implicits._
import fs2.io.file.{Files, Path}
import fs2.{io => _, _}
import munit._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.plasmalabs.grpc.NodeRegTestGrpc
import org.plasmalabs.indexer.services._
import org.plasmalabs.interpreters.NodeRpcOps.clientAsNodeRpcApi
import org.plasmalabs.models.ProposalId
import org.plasmalabs.models.protocol.{ConfigConverter, ConfigGenesis}
import org.plasmalabs.models.utility.Ratio
import org.plasmalabs.node.Util._
import org.plasmalabs.sdk.models.box.{Lock, Value}
import org.plasmalabs.sdk.models.transaction.UnspentTransactionOutput
import org.plasmalabs.sdk.syntax._
import org.plasmalabs.transactiongenerator.interpreters.Fs2TransactionGenerator
import org.plasmalabs.typeclasses.implicits._
import org.plasmalabs.models.protocol.RatioCodec.ratioToProtoRatio
import org.plasmalabs.sdk.constants.NetworkConstants
import org.plasmalabs.sdk.models.LockAddress
import org.plasmalabs.ledger.interpreters.ProposalEventSourceState
import scala.concurrent.duration._

class VersionSwitchingTest extends CatsEffectSuite {

  override val munitIOTimeout: Duration = 5.minutes

  private val defaultNewConfig = ConfigGenesis(
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

  private def configProposalId(config: ConfigGenesis): ProposalId = {
    val configProposal = ConfigConverter.pack(config)
    ProposalEventSourceState.getProposalId(configProposal)
  }

  val lock: Lock = Lock().withPredicate(Lock.Predicate())
  val lockAddress: LockAddress = lock.lockAddress(NetworkConstants.PRIVATE_NETWORK_ID, NetworkConstants.MAIN_LEDGER_ID)

  def proposalAsOutput(proposal: ConfigGenesis): UnspentTransactionOutput = {
    val configProposal = ConfigConverter.pack(proposal)
    val valueValueProposal: org.plasmalabs.sdk.models.box.Value.Value = Value.Value.ConfigProposal(configProposal)
    val value: org.plasmalabs.sdk.models.box.Value = new org.plasmalabs.sdk.models.box.Value(value = valueValueProposal)
    UnspentTransactionOutput(lockAddress, value)
  }

  test("One node which change version based on voting") {
    def configNodeAPrivate(
      dataDir:           Path,
      stakingDir:        Path
    ): String =
      s"""
         |node:
         |  data:
         |    directory: $dataDir
         |  staking:
         |    directory: $stakingDir
         |  p2p:
         |    bind-port: 9150
         |    public-port: 9150
         |  rpc:
         |    bind-port: 9151
         |  big-bang:
         |    type: private
         |    stakerCount: 2
         |    stakes: [10000000]
         |    local-staker-index: 0
         |    regtest-config:
         |      permissive-block-production: false
         |  protocols:
         |    0:
         |      slot-duration: 150 milli
         |      chain-selection-k-lookback: 6
         |      operational-periods-per-epoch: 2
         |  mempool:
         |    protection:
         |      enabled: false
         |  max-supported-version: 2
         |indexer:
         |  enable: true
         |""".stripMargin

    val resource =
      for {
        configALocation <- (
          Files.forAsync[F].tempDirectory,
          Files
            .forAsync[F]
            .tempDirectory
        ).tupled
          .map { case (dataDir, stakingDir) =>
            configNodeAPrivate(dataDir, stakingDir)
          }
          .flatMap(saveLocalConfig(_, "nodeA"))
          .map(_.toString)
        _ <- (launch(configALocation), emptyLaunch())
          .parMapN(_.race(_).map(_.merge).flatMap(_.embedNever))
          .flatMap(nodeCompletion =>
            nodeCompletion.toResource.race(for {
              rpcClientA <- NodeRegTestGrpc.Client.make[F]("127.0.0.2", 9151, tls = false)
              rpcClients = List(rpcClientA)
              given Logger[F] <- Slf4jLogger.fromName[F]("NodeAppTest").toResource
              _                            <- rpcClients.parTraverse(_.waitForRpcStartUp).toResource
              indexerChannelA              <- org.plasmalabs.grpc.makeChannel[F]("localhost", 9151, tls = false)
              indexerTxServiceA            <- TransactionServiceFs2Grpc.stubResource[F](indexerChannelA)
              wallet                       <- makeWallet(indexerTxServiceA)
              _                            <- IO(wallet.spendableBoxes.nonEmpty).assert.toResource
              given Random[F] <- SecureRandom.javaSecuritySecureRandom[F].toResource
              transactionGenerator1 <-
                Fs2TransactionGenerator
                  .make[F](wallet, _ => 1000L, Fs2TransactionGenerator.emptyMetadata[F])
                  .toResource
              configProposal = proposalAsOutput(defaultNewConfig)
              transactionGraph1 <- Stream
                .force(transactionGenerator1.generateTransactions(List(configProposal)))
                .take(10)
                .compile
                .toList
                .toResource

              _ <- rpcClients.parTraverse(fetchUntilHeight(_, 2)).toResource

              _ <-
                Stream
                  .repeatEval(Random[F].elementOf(rpcClients))
                  .zip(Stream.evalSeq(Random[F].shuffleList(transactionGraph1)))
                  .evalMap { case (client, tx) => client.broadcastTransaction(tx) }
                  .compile
                  .drain
                  .toResource

              _ <- rpcClients.parTraverse(fetchUntilEpoch(_, 2)).toResource
              proposalId = configProposalId(defaultNewConfig)
              _                       <- rpcClientA.setVoting(0, proposalId).toResource
              _                       <- rpcClients.parTraverse(fetchUntilEpoch(_, 3)).toResource
              blockWithProposalVoting <- rpcClientA.canonicalHeadFullBlock(implicitly[MonadThrow[F]]).toResource
              _ <- IO(blockWithProposalVoting.header.version.thirdDigit == proposalId).assert.toResource
              _ <- rpcClients.parTraverse(fetchUntilEpoch(_, 4)).toResource
              _ <- rpcClientA.setVoting(2, 0).toResource
              _ <- rpcClients.parTraverse(fetchUntilEpoch(_, 9)).toResource
              blockWithNewVersion <- rpcClientA.canonicalHeadFullBlock(implicitly[MonadThrow[F]]).toResource
              _                   <- IO(blockWithNewVersion.header.version.firstDigit == 2).assert.toResource
            } yield ())
          )
      } yield ()
    resource.use_
  }
}
