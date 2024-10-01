package xyz.stratalab.consensus.models

import xyz.stratalab.models.TxRoot

sealed abstract class BlockHeaderToBodyValidationFailure

object BlockHeaderToBodyValidationFailure {
  case class IncorrectTxRoot(headerTxRoot: TxRoot, bodyTxRoot: TxRoot) extends BlockHeaderToBodyValidationFailure
}
