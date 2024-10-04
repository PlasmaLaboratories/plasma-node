package xyz.stratalab.consensus.algebras

import xyz.stratalab.consensus.models.SlotId
import xyz.stratalab.models.{Eta, Slot}

trait EtaCalculationAlgebra[F[_]] {

  /**
   * Determines the eta value at the requested `childSlot` along the tine containing the `parentSlotId`.  In cases
   * where epochOf(childSlot) =!= epochOf(parentSlotId.slot), a new eta value may be calculated.
   */
  def etaToBe(parentSlotId: SlotId, childSlot: Slot): F[Eta]
}
