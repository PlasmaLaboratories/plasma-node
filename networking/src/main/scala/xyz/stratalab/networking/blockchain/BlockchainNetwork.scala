package xyz.stratalab.networking.blockchain

import cats.effect._
import cats.effect.implicits._
import cats.effect.std.{Mutex, Random}
import cats.implicits._
import fs2._
import fs2.concurrent.Topic
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import xyz.stratalab.crypto.signing.Ed25519
import xyz.stratalab.networking.multiplexer.MultiplexedReaderWriter
import xyz.stratalab.networking.p2p._
import xyz.stratalab.typeclasses.implicits._

import scala.concurrent.duration._

object BlockchainNetwork {

  /**
   * Launches a P2P Network that runs blockchain operations
   * @param host The host to bind to
   * @param bindPort The port to bind to
   * @param localPeer The local peer description
   * @param remotePeers A stream of remote peers to connect to
   * @param clientHandler A handler for each peer client
   * @param serverF A server of data to each peer
   * @param peersStatusChangesTopic topic for notifying about changes in remote peers
   * @param networkTimeout a default timeout duration for reads/writes
   * @return A P2PNetwork
   */
  def make[F[_]: Async: Random](
    host:                    String,
    bindPort:                Int,
    localPeer:               LocalPeer,
    remotePeers:             Stream[F, DisconnectedPeer],
    clientHandler:           BlockchainPeerHandlerAlgebra[F],
    serverF:                 ConnectedPeer => Resource[F, BlockchainPeerServerAlgebra[F]],
    peersStatusChangesTopic: Topic[F, PeerConnectionChange],
    ed25519Resource:         Resource[F, Ed25519],
    networkTimeout:          FiniteDuration
  ): Resource[F, P2PServer[F]] =
    for {
      implicit0(logger: Logger[F]) <- Slf4jLogger.fromName("Node.P2P.Blockchain").toResource
      p2pServer <- FS2P2PServer.make[F](
        host,
        bindPort,
        localPeer,
        remotePeers,
        (peer, socket) =>
          peer.networkVersion match {
            case NetworkProtocolVersions.V0 =>
              Logger[F].error(show"Using legacy network protocol for peer=${peer.p2pVK}").toResource
            case NetworkProtocolVersions.V1 =>
              for {
                portQueues   <- BlockchainMultiplexedBuffers.make[F]
                readerWriter <- MultiplexedReaderWriter.make(socket, networkTimeout)
                peerCache    <- PeerStreamBuffer.make[F]
                server       <- serverF(peer)
                requestMutex <- Mutex[F].toResource
                socketHandler = new BlockchainSocketHandler[F](
                  server,
                  portQueues,
                  readerWriter,
                  peerCache,
                  requestMutex,
                  peer,
                  networkTimeout
                )
                _ <- socketHandler.client
                  .evalMap(clientHandler.usePeer(_).use_)
                  .compile
                  .drain
                  .toResource
              } yield ()
            case _ =>
              Logger[F].error(show"Using unknow network protocol for peer=${peer.p2pVK}").toResource
          },
        peersStatusChangesTopic,
        ed25519Resource
      )
    } yield p2pServer

}
