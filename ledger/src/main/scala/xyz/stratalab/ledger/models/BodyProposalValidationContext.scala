package xyz.stratalab.ledger.models

import xyz.stratalab.consensus.models.BlockId
import xyz.stratalab.models.Slot

case class BodyProposalValidationContext(id: BlockId, slot: Slot)
