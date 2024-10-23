package org.plasmalabs.indexer.algebras

import org.plasmalabs.indexer.model.GE

/**
 * Algebra which defines the Indexer replication status against the stored Ledger.
 *
 * @tparam F the effect-ful context to retrieve the value in
 */
trait GraphReplicationStatusAlgebra[F[_]] {

  /**
   * Fetch Canonical head vertex on the stored indexer Ledger, and compare height Ids with the Node Rpc stored ledger
   *
   * @return Optional header vertex, None if it was not found
   */
  def canonicalHeadSynced: F[Either[GE, Boolean]]

}
