package xyz.stratalab.testnetsimulationorchestrator.models

import co.topl.brambl.models.transaction.IoTransaction
import co.topl.consensus.models.{BlockHeader, BlockId}
import co.topl.node.models.BlockBody
import xyz.stratalab.models.Timestamp

case class AdoptionDatum(blockId: BlockId, timestamp: Timestamp)
case class BlockDatum(header: BlockHeader, body: BlockBody)
case class TransactionDatum(transaction: IoTransaction)
