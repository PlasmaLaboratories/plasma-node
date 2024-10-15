package xyz.stratalab.minting.algebras

import fs2.Stream
import xyz.stratalab.node.models.FullBlock

/**
 * Perpetually mints new blocks
 * @tparam F[_] Base type constructor
 */
trait BlockProducerAlgebra[F[_]] {
  def blocks: F[Stream[F, FullBlock]]
}
