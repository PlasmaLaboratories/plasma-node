package org.plasmalabs.ledger.models

import org.plasmalabs.consensus.models.BlockId
import org.plasmalabs.models.Slot

case class BodyProposalValidationContext(id: BlockId, slot: Slot)
