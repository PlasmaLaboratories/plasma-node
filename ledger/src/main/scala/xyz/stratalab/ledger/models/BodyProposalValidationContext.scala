package xyz.stratalab.ledger.models

import co.topl.consensus.models.BlockId
import xyz.stratalab.models.Slot

case class BodyProposalValidationContext(id: BlockId, slot: Slot)
