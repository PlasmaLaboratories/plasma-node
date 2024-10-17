package org.plasmalabs.consensus.models

import org.plasmalabs.models.TxRoot

sealed abstract class BlockHeaderToBodyValidationFailure

object BlockHeaderToBodyValidationFailure {
  case class IncorrectTxRoot(headerTxRoot: TxRoot, bodyTxRoot: TxRoot) extends BlockHeaderToBodyValidationFailure
}
