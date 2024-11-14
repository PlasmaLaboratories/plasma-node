package org.plasmalabs.grpc

import cats.effect.kernel.{Async, Resource}
import fs2.Stream
import org.plasmalabs.algebras.{NodeRegTestRpc, NodeRpc, SynchronizationTraversalStep}
import org.plasmalabs.consensus.models.{BlockHeader, BlockId}
import org.plasmalabs.models.{Epoch, ProposalId, VersionId}
import org.plasmalabs.node.models.BlockBody
import org.plasmalabs.proto.node.{EpochData, NodeConfig}
import org.plasmalabs.sdk.models.TransactionId
import org.plasmalabs.sdk.models.transaction.IoTransaction

trait NodeRegTestClient[F[_], S[_]] extends NodeRpc[F, S] with NodeRegTestRpc[F, S]

object NodeRegTestGrpc {

  object Client {

    def make[F[_]: Async](host: String, port: Int, tls: Boolean): Resource[F, NodeRegTestClient[F, Stream[F, *]]] =
      for {
        rpcClient     <- NodeGrpc.Client.make(host, port, tls)
        regTestClient <- RegTestGrpc.Client.make(host, port, tls)
      } yield new NodeRegTestClient[F, Stream[F, *]] {

        override def setVoting(votingVersion: VersionId, votingProposal: ProposalId): F[Unit] =
          regTestClient.setVoting(votingVersion, votingProposal)

        override def broadcastTransaction(transaction: IoTransaction): F[Unit] =
          rpcClient.broadcastTransaction(transaction)

        override def currentMempool(): F[Set[TransactionId]] = rpcClient.currentMempool()

        override def currentMempoolContains(transactionId: TransactionId): F[Boolean] =
          rpcClient.currentMempoolContains(transactionId)

        override def fetchBlockHeader(blockId: BlockId): F[Option[BlockHeader]] = rpcClient.fetchBlockHeader(blockId)

        override def fetchBlockBody(blockId: BlockId): F[Option[BlockBody]] = rpcClient.fetchBlockBody(blockId)

        override def fetchTransaction(transactionId: TransactionId): F[Option[IoTransaction]] =
          rpcClient.fetchTransaction(transactionId)

        override def blockIdAtHeight(height: Long): F[Option[BlockId]] = rpcClient.blockIdAtHeight(height)

        override def blockIdAtDepth(depth: Long): F[Option[BlockId]] = rpcClient.blockIdAtDepth(depth)

        override def synchronizationTraversal(): F[Stream[F, SynchronizationTraversalStep]] =
          rpcClient.synchronizationTraversal()

        override def fetchProtocolConfigs(): F[Stream[F, NodeConfig]] = rpcClient.fetchProtocolConfigs()

        override def fetchEpochData(epoch: Option[Epoch]): F[Option[EpochData]] = rpcClient.fetchEpochData(epoch)

        override def fetchCanonicalHeadId(): F[Option[BlockId]] = rpcClient.fetchCanonicalHeadId()
      }
  }
}
