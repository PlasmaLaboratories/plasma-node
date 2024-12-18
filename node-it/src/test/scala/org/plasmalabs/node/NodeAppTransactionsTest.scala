package org.plasmalabs.node

import cats.data.Chain
import cats.effect._
import cats.effect.implicits._
import cats.effect.std.{Random, SecureRandom}
import cats.implicits._
import fs2.io.file.{Files, Path}
import fs2.{io => _, _}
import munit._
import org.plasmalabs.algebras.NodeRpc
import org.plasmalabs.codecs.bytes.tetra.instances._
import org.plasmalabs.consensus.models.BlockId
import org.plasmalabs.grpc.NodeGrpc
import org.plasmalabs.indexer.services._
import org.plasmalabs.interpreters.NodeRpcOps.clientAsNodeRpcApi
import org.plasmalabs.ledger.models._
import org.plasmalabs.node.Util._
import org.plasmalabs.sdk.models.transaction.IoTransaction
import org.plasmalabs.sdk.syntax._
import org.plasmalabs.sdk.validation.algebras.TransactionCostCalculator
import org.plasmalabs.sdk.validation.{TransactionCostCalculatorInterpreter, TransactionCostConfig}
import org.plasmalabs.transactiongenerator.interpreters.Fs2TransactionGenerator
import org.plasmalabs.typeclasses.implicits._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration._

// scalastyle:off magic.number
class NodeAppTransactionsTest extends CatsEffectSuite {

  type F[A] = IO[A]

  type RpcClient = NodeRpc[F, Stream[F, *]]

  override val munitIOTimeout: Duration = 3.minutes

  val maxMempoolSize = 1024 * 20

  val sizeCalculator: TransactionCostCalculator =
    TransactionCostCalculatorInterpreter.make[F](TransactionCostConfig())

  def configNodeA(
    dataDir:                     Path,
    stakingDir:                  Path,
    genesisBlockId:              BlockId,
    genesisSourcePath:           String,
    rpcPort:                     Int,
    ethRpcPort:                  Int,
    useMempoolForSemanticIfLess: Double = 100
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
       |    bind-port: $rpcPort
       |  ethereum-json-rpc:
       |    bind-port: $ethRpcPort
       |  big-bang:
       |    type: public
       |    genesis-id: ${genesisBlockId.show}
       |    source-path: $genesisSourcePath
       |  mempool:
       |    protection:
       |      enabled: true
       |      protection-enabled-threshold-percent: 0
       |      max-mempool-size: $maxMempoolSize
       |      use-mempool-for-semantic-threshold-percent: $useMempoolForSemanticIfLess
       |indexer:
       |  enable: true
       |""".stripMargin

  def configNodeB(
    dataDir:                     Path,
    stakingDir:                  Path,
    genesisBlockId:              BlockId,
    genesisSourcePath:           String,
    rpcPort:                     Int,
    ethRpcPort:                  Int,
    NodeAIp:                     String,
    useMempoolForSemanticIfLess: Double = 100
  ): String =
    s"""
       |node:
       |  data:
       |    directory: $dataDir
       |  staking:
       |    directory: $stakingDir
       |  p2p:
       |    bind-port: 9152
       |    public-port: 9152
       |    known-peers: $NodeAIp:9150
       |  rpc:
       |    bind-port: $rpcPort
       |  ethereum-json-rpc:
       |    bind-port: $ethRpcPort
       |  big-bang:
       |    type: public
       |    genesis-id: ${genesisBlockId.show}
       |    source-path: $genesisSourcePath
       |  mempool:
       |    protection:
       |      enabled: true
       |      protection-enabled-threshold-percent: 0
       |      max-mempool-size: $maxMempoolSize
       |      use-mempool-for-semantic-threshold-percent: $useMempoolForSemanticIfLess
       |indexer:
       |  enable: false
       |""".stripMargin

  val rpcPortA: Int = 1951
  val rpcPortB: Int = 1953
  val ethRpcPortA = 2951
  val ethRpcPortB = 2953

  test("Enabled memory pool protection, accept and broadcast transaction chain, tx in mempool is considered") {

    val height: Int = 3

    val resource =
      for {
        testnetConfig     <- createTestnetConfig.toResource
        genesisServerPort <- serveGenesisBlock(testnetConfig.genesis)
        genesisSourcePath = s"http://localhost:$genesisServerPort/${testnetConfig.genesis.header.id.show}"
        configALocation <- (
          Files.forAsync[F].tempDirectory,
          Files
            .forAsync[F]
            .tempDirectory
            .evalTap(saveStaker(_)(testnetConfig.stakers(0)._1, testnetConfig.stakers(0)._2))
        ).tupled
          .map { case (dataDir, stakingDir) =>
            configNodeA(dataDir, stakingDir, testnetConfig.genesis.header.id, genesisSourcePath, rpcPortA, ethRpcPortA)
          }
          .flatMap(saveLocalConfig(_, "nodeA"))
          .map(_.toString)
        configBLocation <- (
          Files.forAsync[F].tempDirectory,
          Files
            .forAsync[F]
            .tempDirectory
            .evalTap(saveStaker(_)(testnetConfig.stakers(1)._1, testnetConfig.stakers(1)._2))
        ).tupled
          .map { case (dataDir, stakingDir) =>
            configNodeB(
              dataDir,
              stakingDir,
              testnetConfig.genesis.header.id,
              genesisSourcePath,
              rpcPortB,
              ethRpcPortB,
              "127.0.0.2"
            )
          }
          .flatMap(serveConfig(_, "nodeB.yaml"))
        // Run the nodes in separate fibers, but use the fibers' outcomes as an error signal to
        // the test by racing the computation
        _ <- (launch(configALocation), launch(configBLocation))
          .parMapN(_.race(_).map(_.merge).flatMap(_.embedNever))
          .flatMap(nodeCompletion =>
            nodeCompletion.toResource.race(for {
              rpcClientA <- NodeGrpc.Client.make[F]("127.0.0.2", rpcPortA, tls = false)
              rpcClientB <- NodeGrpc.Client.make[F]("127.0.0.3", rpcPortB, tls = false)
              rpcClients = List(rpcClientA, rpcClientB)
              given Logger[F]   <- Slf4jLogger.fromName[F]("NodeAppTest").toResource
              _                 <- rpcClients.parTraverse(_.waitForRpcStartUp).toResource
              indexerChannelA   <- org.plasmalabs.grpc.makeChannel[F]("127.0.0.2", rpcPortA, tls = false)
              indexerTxServiceA <- TransactionServiceFs2Grpc.stubResource[F](indexerChannelA)
              wallet            <- makeWallet(indexerTxServiceA)
              _                 <- IO(wallet.spendableBoxes.nonEmpty).assert.toResource
              given Random[F]   <- SecureRandom.javaSecuritySecureRandom[F].toResource

              transactionGenerator <-
                Fs2TransactionGenerator
                  .make[F](wallet, _ => 1000L, Fs2TransactionGenerator.emptyMetadata[F])
                  .toResource
              transactionGraph <- Stream
                .force(transactionGenerator.generateTransactions())
                .take(10)
                .compile
                .toList
                .toResource
              _ <- IO(transactionGraph.sizeIs == 10).assert.toResource

              _ <- rpcClients.parTraverse(fetchUntilHeight(_, height)).toResource
              // Broadcast transactions to the node
              _ <- Stream
                .emits(transactionGraph)
                .covaryAll[F, IoTransaction]
                .evalMap(tx => rpcClientA.broadcastTransaction(tx))
                .compile
                .drain
                .toResource

              // Verify that the transactions were confirmed by both nodes
              _ <- rpcClients
                .parTraverse(client =>
                  Async[F].timeout(confirmTransactions(client)(transactionGraph.map(_.id).toSet), 60.seconds)
                )
                .toResource

            } yield ())
          )
      } yield ()
    resource.use_
  }

  test("Enabled memory pool protection, send tx chain to different nodes, thus only head tx will be accepted") {

    val height: Int = 3

    val resource =
      for {
        testnetConfig     <- createTestnetConfig.toResource
        genesisServerPort <- serveGenesisBlock(testnetConfig.genesis)
        genesisSourcePath = s"http://localhost:$genesisServerPort/${testnetConfig.genesis.header.id.show}"
        configALocation <- (
          Files.forAsync[F].tempDirectory,
          Files
            .forAsync[F]
            .tempDirectory
            .evalTap(saveStaker(_)(testnetConfig.stakers(0)._1, testnetConfig.stakers(0)._2))
        ).tupled
          .map { case (dataDir, stakingDir) =>
            configNodeA(dataDir, stakingDir, testnetConfig.genesis.header.id, genesisSourcePath, rpcPortA, ethRpcPortA)
          }
          .flatMap(saveLocalConfig(_, "nodeA"))
          .map(_.toString)
        configBLocation <- (
          Files.forAsync[F].tempDirectory,
          Files
            .forAsync[F]
            .tempDirectory
            .evalTap(saveStaker(_)(testnetConfig.stakers(1)._1, testnetConfig.stakers(1)._2))
        ).tupled
          .map { case (dataDir, stakingDir) =>
            configNodeB(
              dataDir,
              stakingDir,
              testnetConfig.genesis.header.id,
              genesisSourcePath,
              rpcPortB,
              ethRpcPortB,
              "127.0.0.4"
            )
          }
          .flatMap(serveConfig(_, "nodeB.yaml"))
        // Run the nodes in separate fibers, but use the fibers' outcomes as an error signal to
        // the test by racing the computation
        _ <- (launch(configALocation), launch(configBLocation))
          .parMapN(_.race(_).map(_.merge).flatMap(_.embedNever))
          .flatMap(nodeCompletion =>
            nodeCompletion.toResource.race(for {
              rpcClientA <- NodeGrpc.Client.make[F]("127.0.0.4", rpcPortA, tls = false)
              rpcClientB <- NodeGrpc.Client.make[F]("127.0.0.5", rpcPortB, tls = false)
              rpcClients = List(rpcClientA, rpcClientB)
              given Logger[F]   <- Slf4jLogger.fromName[F]("NodeAppTest").toResource
              _                 <- rpcClients.parTraverse(_.waitForRpcStartUp).toResource
              indexerChannelA   <- org.plasmalabs.grpc.makeChannel[F]("127.0.0.4", rpcPortA, tls = false)
              indexerTxServiceA <- TransactionServiceFs2Grpc.stubResource[F](indexerChannelA)
              given Random[F]   <- SecureRandom.javaSecuritySecureRandom[F].toResource

              _ <- rpcClients.parTraverse(fetchUntilHeight(_, height)).toResource

              wallet <- makeWallet(indexerTxServiceA)
              _      <- IO(wallet.spendableBoxes.nonEmpty).assert.toResource
              transactionGenerator <-
                Fs2TransactionGenerator
                  .make[F](wallet, _ => 1000L, Fs2TransactionGenerator.emptyMetadata[F])
                  .toResource
              transactionGraph <- Stream
                .force(transactionGenerator.generateTransactions())
                .take(20)
                .compile
                .toList
                .toResource

              _ <- Logger[F].info(show"Generated txs: ${transactionGraph.map(_.id)}").toResource

              // send txs chain to different nodes,
              // most of them will be rejected because "parent" tx is sent to other node
              _ <- Stream
                .repeatEval(Random[F].elementOf(rpcClients))
                .zip(Stream.evalSeq(Random[F].shuffleList(transactionGraph)))
                .evalMap { case (client, tx) => client.broadcastTransaction(tx) }
                .compile
                .drain
                .toResource

              // verify that first transaction had been confirmed
              _ <- Async[F].timeout(fetchUntilTx(rpcClientB, transactionGraph.head.id), 30.seconds).toResource

              _ <- rpcClients.parTraverse { client =>
                Async[F].timeout(confirmTransactions(client)(Set(transactionGraph.head.id)), 30.seconds)
              }.toResource

              // verify that last transaction had not been confirmed
              _ <- rpcClients.parTraverse(verifyNotConfirmed(_)(Set(transactionGraph.last.id))).toResource

            } yield ())
          )
      } yield ()
    resource.use_
  }

  test("Enabled memory pool protection consider only first N transaction for semantic check") {
    val height: Int = 3

    val useMempoolForSemanticIfLess: Double = 10
    val useMempoolForSemanticIfLessSize = maxMempoolSize * (useMempoolForSemanticIfLess / 100)

    val resource =
      for {
        testnetConfig     <- createTestnetConfig.toResource
        genesisServerPort <- serveGenesisBlock(testnetConfig.genesis)
        genesisSourcePath = s"http://localhost:$genesisServerPort/${testnetConfig.genesis.header.id.show}"
        configALocation <- (
          Files.forAsync[F].tempDirectory,
          Files
            .forAsync[F]
            .tempDirectory
            .evalTap(saveStaker(_)(testnetConfig.stakers(0)._1, testnetConfig.stakers(0)._2))
        ).tupled
          .map { case (dataDir, stakingDir) =>
            configNodeA(
              dataDir,
              stakingDir,
              testnetConfig.genesis.header.id,
              genesisSourcePath,
              rpcPortA,
              ethRpcPortA,
              useMempoolForSemanticIfLess
            )
          }
          .flatMap(saveLocalConfig(_, "nodeA"))
          .map(_.toString)
        configBLocation <- (
          Files.forAsync[F].tempDirectory,
          Files
            .forAsync[F]
            .tempDirectory
            .evalTap(saveStaker(_)(testnetConfig.stakers(1)._1, testnetConfig.stakers(1)._2))
        ).tupled
          .map { case (dataDir, stakingDir) =>
            configNodeB(
              dataDir,
              stakingDir,
              testnetConfig.genesis.header.id,
              genesisSourcePath,
              rpcPortB,
              ethRpcPortB,
              "127.0.0.6",
              useMempoolForSemanticIfLess
            )
          }
          .flatMap(serveConfig(_, "nodeB.yaml"))
        // Run the nodes in separate fibers, but use the fibers' outcomes as an error signal to
        // the test by racing the computation
        _ <- (launch(configALocation), launch(configBLocation))
          .parMapN(_.race(_).map(_.merge).flatMap(_.embedNever))
          .flatMap(nodeCompletion =>
            nodeCompletion.toResource.race(for {
              rpcClientA <- NodeGrpc.Client.make[F]("127.0.0.6", rpcPortA, tls = false)
              rpcClientB <- NodeGrpc.Client.make[F]("127.0.0.7", rpcPortB, tls = false)
              rpcClients = List(rpcClientA, rpcClientB)
              given Logger[F]   <- Slf4jLogger.fromName[F]("NodeAppTest").toResource
              _                 <- rpcClients.parTraverse(_.waitForRpcStartUp).toResource
              indexerChannelA   <- org.plasmalabs.grpc.makeChannel[F]("127.0.0.6", rpcPortA, tls = false)
              indexerTxServiceA <- TransactionServiceFs2Grpc.stubResource[F](indexerChannelA)
              _                 <- rpcClients.parTraverse(fetchUntilHeight(_, height)).toResource

              wallet          <- makeWallet(indexerTxServiceA)
              _               <- IO(wallet.spendableBoxes.nonEmpty).assert.toResource
              given Random[F] <- SecureRandom.javaSecuritySecureRandom[F].toResource

              transactionGenerator <-
                Fs2TransactionGenerator
                  .make[F](wallet, _ => 10L, Fs2TransactionGenerator.emptyMetadata[F])
                  .toResource
              // get transactions of such size that last transaction
              // shall no longer take in consideration transactions im memory pool
              transactionGraph <- Stream
                .force(transactionGenerator.generateTransactions())
                .map(tx => IoTransactionEx(tx, RewardQuantities(), sizeCalculator.costOf(tx)))
                .scan((Chain.empty[IoTransactionEx], 0L)) { case ((txs, totalSize), tx) =>
                  (txs.append(tx), totalSize + tx.size)
                }
                .takeWhile { case (txs, totalSize) =>
                  (totalSize - txs.lastOption.fold(0L)(_.size)) < useMempoolForSemanticIfLessSize
                }
                .last
                .evalTap { txs =>
                  val fr = txs.get
                  Logger[F].info(s"total size: ${fr._2}") >>
                  Logger[F].info(show"${fr._1.map(tx => (tx.tx.id, tx.size))}")
                }
                .map(_.get._1.map(_.tx))
                .compile
                .last
                .map(_.get)
                .map(_.toList)
                .toResource

              // Broadcast _all_ of the good transactions to the nodes
              _ <- Stream
                .emits(transactionGraph)
                .covaryAll[F, IoTransaction]
                .evalMap(tx => rpcClientA.broadcastTransaction(tx))
                .compile
                .drain
                .toResource

              _ <- Async[F]
                .timeout(fetchUntilTx(rpcClientB, transactionGraph.init.last.id), 70.seconds)
                .toResource
              _ <- rpcClients
                .parTraverse(verifyNotConfirmed(_)(Set(transactionGraph.last.id)))
                .toResource
            } yield ())
          )
      } yield ()
    resource.use_
  }
}
// scalastyle:on magic.number
