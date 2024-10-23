package org.plasmalabs.testnetsimulationorchestrator.models

import org.plasmalabs.consensus.models.{BlockHeader, BlockId}
import org.plasmalabs.models.Timestamp
import org.plasmalabs.node.models.BlockBody
import org.plasmalabs.sdk.models.transaction.IoTransaction

case class AdoptionDatum(blockId: BlockId, timestamp: Timestamp)
case class BlockDatum(header: BlockHeader, body: BlockBody)
case class TransactionDatum(transaction: IoTransaction)
