package xyz.stratalab.minting.algebras

import xyz.stratalab.consensus.models.BlockId
import xyz.stratalab.node.models.FullBlockBody

/**
 * Assembles block bodies
 */
trait BlockPackerAlgebra[F[_]] {

  /**
   * Returns a Stream of incrementally-improving FullBlockBodies using the given parent block information
   */
  def blockImprover(parentBlockId: BlockId, height: Long, slot: Long): fs2.Stream[F, FullBlockBody]
}
