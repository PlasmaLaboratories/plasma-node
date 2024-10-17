package org.plasmalabs.consensus.algebras

import org.plasmalabs.consensus.models.SlotId
import org.plasmalabs.models.{Eta, Slot}

trait EtaCalculationAlgebra[F[_]] {

  /**
   * Determines the eta value at the requested `childSlot` along the tine containing the `parentSlotId`.  In cases
   * where epochOf(childSlot) =!= epochOf(parentSlotId.slot), a new eta value may be calculated.
   */
  def etaToBe(parentSlotId: SlotId, childSlot: Slot): F[Eta]
}
