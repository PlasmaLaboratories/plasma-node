package org.plasmalabs.algebras

import org.plasmalabs.consensus.models.{BlockHeader, BlockId}
import org.plasmalabs.models.Epoch
import org.plasmalabs.node.models.BlockBody
import org.plasmalabs.proto.node.{EpochData, NodeConfig}
import org.plasmalabs.sdk.models.TransactionId
import org.plasmalabs.sdk.models.transaction.IoTransaction

/**
 * Node Rpc
 * An interaction layer intended for users/clients of a blockchain node.
 * @tparam F Effect type
 * @tparam S Canonical head changes Synchronization Traversal Container, Ex: Stream, Seq
 */
trait NodeRpc[F[_], S[_]] {
  def broadcastTransaction(transaction: IoTransaction): F[Unit]

  def currentMempool(): F[Set[TransactionId]]
  def currentMempoolContains(transactionId: TransactionId): F[Boolean]

  def fetchBlockHeader(blockId: BlockId): F[Option[BlockHeader]]

  def fetchBlockBody(blockId: BlockId): F[Option[BlockBody]]

  def fetchTransaction(transactionId: TransactionId): F[Option[IoTransaction]]

  def blockIdAtHeight(height: Long): F[Option[BlockId]]

  def blockIdAtDepth(depth: Long): F[Option[BlockId]]

  def synchronizationTraversal(): F[S[SynchronizationTraversalStep]]
  def fetchProtocolConfigs(): F[S[NodeConfig]]

  def fetchEpochData(epoch: Option[Epoch]): F[Option[EpochData]]

  def fetchCanonicalHeadId(): F[Option[BlockId]]
}
