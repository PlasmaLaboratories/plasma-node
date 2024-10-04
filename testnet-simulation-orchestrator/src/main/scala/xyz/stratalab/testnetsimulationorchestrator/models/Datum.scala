package xyz.stratalab.testnetsimulationorchestrator.models

import xyz.stratalab.consensus.models.{BlockHeader, BlockId}
import xyz.stratalab.models.Timestamp
import xyz.stratalab.node.models.BlockBody
import xyz.stratalab.sdk.models.transaction.IoTransaction

case class AdoptionDatum(blockId: BlockId, timestamp: Timestamp)
case class BlockDatum(header: BlockHeader, body: BlockBody)
case class TransactionDatum(transaction: IoTransaction)
