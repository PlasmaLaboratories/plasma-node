package org.plasmalabs.node

import cats.data.OptionT
import cats.effect._
import cats.effect.implicits._
import cats.implicits._
import fs2.io.file.{Files, Path}
import fs2.{io => _}
import munit._
import org.plasmalabs.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import org.plasmalabs.consensus.models.BlockId
import org.plasmalabs.grpc.NodeGrpc
import org.plasmalabs.indexer.services._
import org.plasmalabs.interpreters.NodeRpcOps.clientAsNodeRpcApi
import org.plasmalabs.node.Util._
import org.plasmalabs.typeclasses.implicits._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration._

class NodeNetworkControlTest extends CatsEffectSuite {

  override val munitIOTimeout: Duration = 3.minutes

  val nodeAIp = "127.0.0.2"
  val nodeAP2PPort = 9150
  val nodeARpcPort = 9151
  val nodeAEthRpcPort = 8545

  val nodeBIp = "localhost"
  val nodeBP2PPort = 9152
  val nodeBRpcPort = 9153
  val nodeBEthRpcPort = 8546

  test("Two block-producing nodes that maintain consensus") {
    def configNodeA(dataDir: Path, stakingDir: Path, genesisBlockId: BlockId, genesisSourcePath: String): String =
      s"""
         |node:
         |  data:
         |    directory: $dataDir
         |  staking:
         |    directory: $stakingDir
         |  p2p:
         |    bind-port: $nodeAP2PPort
         |    public-port: $nodeAP2PPort
         |  rpc:
         |    bind-port: $nodeARpcPort
         |    network-control: true
         |  ethereum-json-rpc:
         |    bind-port: $nodeAEthRpcPort
         |  big-bang:
         |    type: public
         |    genesis-id: ${genesisBlockId.show}
         |    source-path: $genesisSourcePath
         |  mempool:
         |    protection:
         |      enabled: false
         |indexer:
         |  enable: true
         |""".stripMargin

    def configNodeB(dataDir: Path, stakingDir: Path, genesisBlockId: BlockId, genesisSourcePath: String): String =
      s"""
         |node:
         |  data:
         |    directory: $dataDir
         |  staking:
         |    directory: $stakingDir
         |  p2p:
         |    bind-port: $nodeBP2PPort
         |    public-port: $nodeBP2PPort
         |    known-peers: $nodeAIp:$nodeAP2PPort
         |  rpc:
         |    bind-port: $nodeBRpcPort
         |    network-control: true
         |  ethereum-json-rpc:
         |    bind-port: $nodeBEthRpcPort
         |  big-bang:
         |    type: public
         |    genesis-id: ${genesisBlockId.show}
         |    source-path: $genesisSourcePath
         |  mempool:
         |    protection:
         |      enabled: false
         |indexer:
         |  enable: false
         |""".stripMargin

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
            .evalTap(saveStaker(_)(testnetConfig.stakers.head._1, testnetConfig.stakers.head._2))
        ).tupled
          .map { case (dataDir, stakingDir) =>
            configNodeA(dataDir, stakingDir, testnetConfig.genesis.header.id, genesisSourcePath)
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
            configNodeB(dataDir, stakingDir, testnetConfig.genesis.header.id, genesisSourcePath)
          }
          .flatMap(serveConfig(_, "nodeB.yaml"))
        // Run the nodes in separate fibers, but use the fibers' outcomes as an error signal to
        // the test by racing the computation
        _ <- (launch(configALocation), launch(configBLocation))
          .parMapN(_.race(_).map(_.merge).flatMap(_.embedNever))
          .flatMap(nodeCompletion =>
            nodeCompletion.toResource.race(for {
              rpcClientA      <- NodeGrpc.Client.make[F](nodeAIp, nodeARpcPort, tls = false)
              rpcClientB      <- NodeGrpc.Client.make[F](nodeBIp, nodeBRpcPort, tls = false)
              networkControlA <- NetworkControlClient.make[F](nodeAIp, nodeARpcPort, tls = false)
              networkControlB <- NetworkControlClient.make[F](nodeBIp, nodeBRpcPort, tls = false)
              rpcClients = List(rpcClientA, rpcClientB)
              given Logger[F]   <- Slf4jLogger.fromName[F]("NodeNetworkControlTest").toResource
              _                 <- rpcClients.parTraverse(_.waitForRpcStartUp).toResource
              indexerChannelA   <- org.plasmalabs.grpc.makeChannel[F](nodeAIp, nodeARpcPort, tls = false)
              indexerTxServiceA <- TransactionServiceFs2Grpc.stubResource[F](indexerChannelA)
              wallet            <- makeWallet(indexerTxServiceA)
              _                 <- IO(wallet.spendableBoxes.nonEmpty).assert.toResource

              // check consensus
              firstHeight = 3
              _ <- rpcClients.parTraverse(fetchUntilHeight(_, firstHeight)).toResource
              idsAtTargetHeight <- rpcClients
                .traverse(client => OptionT(client.blockIdAtHeight(firstHeight)).getOrRaise(new IllegalStateException))
                .toResource
              _ <- IO(idsAtTargetHeight.toSet.size == 1).assert.toResource
              // forget nodes
              hostA <- networkControlA.getHostId().toResource
              hostB <- networkControlB.getHostId().toResource
              _     <- networkControlA.forgetPeer(hostB).toResource
              _     <- networkControlB.forgetPeer(hostA).toResource

              // check there is no consensus
              secondHeight = firstHeight + 4
              _ <- rpcClients.parTraverse(fetchUntilHeight(_, secondHeight)).toResource
              idsAtTargetHeight2 <- rpcClients
                .traverse(client => OptionT(client.blockIdAtHeight(secondHeight)).getOrRaise(new IllegalStateException))
                .toResource
              _ <- IO(idsAtTargetHeight2.toSet.size == 2).assert.toResource

              // remind nodes
              _ <- networkControlB.addPeer(nodeAIp, nodeAP2PPort, hostA.some).toResource
              _ <- networkControlA.addPeer(nodeBIp, nodeBP2PPort, hostA.some).toResource

              // check there is consensus
              thirdHeight = secondHeight + 2
              _ <- rpcClients.parTraverse(fetchUntilHeight(_, thirdHeight)).toResource
              idsAtTargetHeight3 <- rpcClients
                .traverse(client => OptionT(client.blockIdAtHeight(thirdHeight)).getOrRaise(new IllegalStateException))
                .toResource
              _ <- IO(idsAtTargetHeight3.toSet.size == 1).assert.toResource
            } yield ())
          )
      } yield ()
    resource.use_
  }
}
