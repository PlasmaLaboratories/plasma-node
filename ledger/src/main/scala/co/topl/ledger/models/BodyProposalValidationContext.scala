package co.topl.ledger.models

import co.topl.consensus.models.BlockId
import co.topl.models.Slot

case class BodyProposalValidationContext(id: BlockId, slot: Slot)
