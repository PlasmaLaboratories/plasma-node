package org.plasmalabs.consensus.algebras

import org.plasmalabs.consensus.models.BlockId
import org.plasmalabs.models.{Bytes, Slot}

/**
 * A cache of recently used VRF eligibilities
 */
trait EligibilityCacheAlgebra[F[_]] {

  /**
   * Attempt to include the given eligibility in the cache
   * @param blockId the block ID associated with the eligibility
   * @param vrfVK The staker's VRF VK
   * @param slot The slot for which the eligibility was generated
   * @return true if the eligibility was inserted, false if it was already in the cache
   */
  def tryInclude(blockId: BlockId, vrfVK: Bytes, slot: Slot): F[Boolean]

}
