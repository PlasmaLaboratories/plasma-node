package org.plasmalabs.minting.algebras

import org.plasmalabs.consensus.models.SlotId
import org.plasmalabs.minting.models.OperationalKeyOut
import org.plasmalabs.models.{Eta, Slot}

/**
 * A KeyEvolverAlgebra is responsible for encapsulating a key locally and emitting an evolved version of
 * it for some particular slot on demand
 */
trait OperationalKeyMakerAlgebra[F[_]] {
  def operationalKeyForSlot(slot: Slot, parentSlotId: SlotId, eta: Eta): F[Option[OperationalKeyOut]]
}
