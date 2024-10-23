package org.plasmalabs.algebras

import org.plasmalabs.indexer.services.BlockResponse

/**
 * Indexer Rpc
 * An interaction layer intended for users/clients of a blockchain node.
 *
 * @tparam F Effect type
 * @tparam S Canonical head changes Synchronization Traversal Container, Ex: Stream, Seq
 */

/**
 * TODO Indexer Rpc is used by IndexerGrpc Client Implementation, which is used only right now by Byzantine test
 * In future PRs, we should complete the Client Implementation, and this trait
 */
trait IndexerRpc[F[_]] {
  def blockIdAtHeight(height: Long): F[BlockResponse]
}
