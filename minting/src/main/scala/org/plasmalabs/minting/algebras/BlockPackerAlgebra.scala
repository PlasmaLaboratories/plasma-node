package org.plasmalabs.minting.algebras

import org.plasmalabs.consensus.models.BlockId
import org.plasmalabs.node.models.FullBlockBody

/**
 * Assembles block bodies
 */
trait BlockPackerAlgebra[F[_]] {

  /**
   * Returns a Stream of incrementally-improving FullBlockBodies using the given parent block information
   */
  def blockImprover(parentBlockId: BlockId, height: Long, slot: Long): fs2.Stream[F, FullBlockBody]
}
