package org.plasmalabs.byzantine

import cats.effect.Async
import cats.effect.implicits._
import cats.implicits._
import com.spotify.docker.client.DockerClient
import org.plasmalabs.byzantine.util._
import org.plasmalabs.interpreters.NodeRpcOps._
import org.typelevel.log4cats.Logger

import java.time.Instant
import scala.concurrent.duration._

/**
 * This test creates ~3 nodes that share the same seed but are not configured to talk to each other (meaning they
 * are siloed from one another).  The first two nodes are launched and allowed to forge their own unique chains for
 * two epochs.  Then, they are shutdown.  A third node is created, and all 3 nodes are reconnected and launched.
 * The first two nodes should reach consensus, while the third node should be able to synchronize with the chain.
 * By the end of the test, all 3 nodes should be in sync and producing blocks.
 */
class ChainSelectionTest extends IntegrationSuite {

  override def munitIOTimeout: Duration = 12.minutes

  test("Disconnected nodes can forge independently and later sync up to a proper chain") {
    val bigBang = Instant.now().plusSeconds(30)
    val stakes = List(BigInt(500), BigInt(400), BigInt(300)).some
    val config0 =
      TestNodeConfig(bigBang, stakerCount = 3, localStakerIndex = 0, knownPeers = Nil, stakes = stakes)
    val config1 =
      TestNodeConfig(bigBang, stakerCount = 3, localStakerIndex = 1, knownPeers = Nil, stakes = stakes)
    val config2 =
      TestNodeConfig(bigBang, stakerCount = 3, localStakerIndex = 2, knownPeers = Nil, stakes = stakes)

    val nodesWithknownPeers = List(
      config0,
      config1.copy(knownPeers = List("ChainSelectionTest-node0")),
      config2.copy(knownPeers = List("ChainSelectionTest-node1"))
    )

    val resource = for {
      (dockerSupport, _dockerClient) <- DockerSupport.make[F]()
      given DockerClient = _dockerClient
      node0 <- dockerSupport.createNode("ChainSelectionTest-node0", "ChainSelectionTest", config0)
      node1 <- dockerSupport.createNode("ChainSelectionTest-node1", "ChainSelectionTest", config1)

      initialNodes = List(node0, node1)
      _ <- initialNodes.parTraverse(_.startContainer[F]).toResource
      _ <- initialNodes
        .parTraverse(node => node.rpcClient[F](node.config.rpcPort, tls = false).use(_.waitForRpcStartUp))
        .toResource
      _ <- Logger[F].info("Waiting for nodes to complete epoch=0.  This may take several minutes.").toResource
      blockHeaders <- Async[F]
        .andWait(
          initialNodes
            .parTraverse(node =>
              node
                .rpcClient[F](node.config.rpcPort, tls = false)
                .use(
                  _.adoptedHeaders
                    .takeWhile(_.slot < TestNodeConfig.epochSlotLength)
                    .timeout(9.minutes)
                    .compile
                    .lastOrError
                )
            ),
          5.seconds
        )
        .toResource

      _ <- blockHeaders.size.pure[F].assertEquals(2).toResource
      _ <- blockHeaders.distinct.size.pure[F].assertEquals(2).toResource

      // Create a third node
      node2 <- dockerSupport.createNode("ChainSelectionTest-node2", "ChainSelectionTest", config2)
      // Stop, configure with KnownPeers, (re)start containers and run again
      allNodes = List(node0, node1, node2)
      _ <- allNodes.parTraverse(_.stop[F]).toResource
      _ <- allNodes
        .zip(nodesWithknownPeers)
        .parTraverse { case (node, newConfig) => node.configure[F](newConfig.yaml) }
        .toResource
      // Note: Launch nodes in-order so that the P2P connections properly initialize.  Once proper peer management and
      // retry logic is implemented, the relaunches can happen in parallel.
      _ <- allNodes
        .zip(nodesWithknownPeers)
        .parTraverse { case (node, newConfig) =>
          node.restartContainer[F] >> node.rpcClient[F](newConfig.rpcPort, tls = false).use(_.waitForRpcStartUp)
        }
        .toResource
      _ <- Logger[F]
        .info("Waiting for nodes to complete epoch=2.  This may take several minutes.")
        .toResource
      thirdEpochHeads <- allNodes
        .zip(nodesWithknownPeers)
        .parTraverse { case (node, newConfig) =>
          node
            .rpcClient[F](newConfig.rpcPort, tls = false)
            .use(
              _.adoptedHeaders
                .takeWhile(_.slot < (TestNodeConfig.epochSlotLength * 3))
                .timeout(9.minutes)
                .compile
                .lastOrError
            )
        }
        .toResource
      _ <- Logger[F].info("Nodes have reached target epoch").toResource
      heights = thirdEpochHeads.map(_.height)
      // All nodes should be at _roughly_ equal height
      _ <- Logger[F].info(heights.mkString(",")).toResource
      _ <- (heights.max - heights.min <= 5).pure[F].assert.toResource
      // All nodes should have a shared common ancestor near the tip of the chain
      _ <- allNodes
        .zip(nodesWithknownPeers)
        .parTraverse { case (node, newConfig) =>
          node
            .rpcClient[F](newConfig.rpcPort, tls = false)
            .use(
              _.blockIdAtHeight(heights.min - 5)
            )
        }
        .map(_.toSet.size)
        .assertEquals(1)
        .toResource

    } yield ()

    resource.use_
  }

}
