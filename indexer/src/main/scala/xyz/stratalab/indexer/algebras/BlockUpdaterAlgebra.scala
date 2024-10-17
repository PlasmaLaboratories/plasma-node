package org.plasmalabs.indexer.algebras

import org.plasmalabs.indexer.model.GE
import org.plasmalabs.indexer.services.BlockData

/**
 * Inserter of blocks to the chain in the data store.
 * @tparam F the effect-ful context to retrieve the value in
 */
trait BlockUpdaterAlgebra[F[_]] {

  /**
   * Inserts a block to the chain in the data store.
   * @param block the block to be inserted in the data store.
   * @return Unit
   */
  def insert(block: BlockData): F[Either[GE, Unit]]

  /**
   * Removes a block from the chain in the data store.
   *
   * @param block the block to be removed in the data store.
   * @return Unit
   */
  def remove(block: BlockData): F[Either[GE, Unit]]

}
