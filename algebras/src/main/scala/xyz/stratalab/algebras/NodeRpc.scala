package xyz.stratalab.algebras

import xyz.stratalab.consensus.models.{BlockHeader, BlockId}
import xyz.stratalab.models.Epoch
import xyz.stratalab.node.models.BlockBody
import xyz.stratalab.proto.node.{EpochData, NodeConfig}
import xyz.stratalab.sdk.models.TransactionId
import xyz.stratalab.sdk.models.transaction.IoTransaction

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
}
