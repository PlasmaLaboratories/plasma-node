package xyz.stratalab.blockchain.algebras

import xyz.stratalab.models.Epoch
import xyz.stratalab.proto.node.EpochData

/**
 * Provides epoch-level statistics
 */
trait EpochDataAlgebra[F[_]] {

  /**
   * Constructs the EpochData for the requested epoch.  The "current" epoch is updated as blocks are adopted.
   * @param epoch the epoch number to request
   * @return EpochData
   */
  def dataOf(epoch: Epoch): F[Option[EpochData]]

}
