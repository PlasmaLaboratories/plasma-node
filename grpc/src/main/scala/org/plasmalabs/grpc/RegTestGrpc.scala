package org.plasmalabs.grpc

import cats.effect.kernel.{Async, Resource}
import cats.implicits.*
import fs2.Stream
import io.grpc.Metadata
import org.plasmalabs.algebras.*
import org.plasmalabs.models.{ProposalId, VersionId}
import org.plasmalabs.node.services.*

object RegTestGrpc {

  object Client {

    /**
     * Creates a RPC Client for interacting with a Node in regtest mode, that include usual rpc client
     * @param host Node node host/IP
     * @param port Node node port
     * @param tls Should the connection use TLS?
     */
    def make[F[_]: Async](host: String, port: Int, tls: Boolean): Resource[F, NodeRegTestRpc[F, Stream[F, *]]] =
      makeChannel(host, port, tls)
        .flatMap(RegtestRpcFs2Grpc.stubResource[F])
        .map(client =>
          new NodeRegTestRpc[F, Stream[F, *]] {

            override def setVoting(votingVersion: VersionId, votingProposal: ProposalId): F[Unit] =
              client.setVoting(SetVotingReq(votingVersion, votingProposal), new Metadata()).void
          }
        )
  }
}
