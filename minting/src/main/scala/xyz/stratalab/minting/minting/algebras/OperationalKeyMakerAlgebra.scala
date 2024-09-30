package xyz.stratalab.minting.algebras

import co.topl.consensus.models.SlotId
import xyz.stratalab.minting.models.OperationalKeyOut
import xyz.stratalab.models.{Eta, Slot}

/**
 * A KeyEvolverAlgebra is responsible for encapsulating a key locally and emitting an evolved version of
 * it for some particular slot on demand
 */
trait OperationalKeyMakerAlgebra[F[_]] {
  def operationalKeyForSlot(slot: Slot, parentSlotId: SlotId, eta: Eta): F[Option[OperationalKeyOut]]
}
