package org.plasmalabs.indexer.orientDb.instances

import com.orientechnologies.orient.core.metadata.schema.OType
import org.plasmalabs.indexer.orientDb.schema.OTyped.Instances.*
import org.plasmalabs.indexer.orientDb.schema.{GraphDataEncoder, OIndexable, VertexSchema}
import org.plasmalabs.sdk.common.ContainsImmutable.instances.ioTransactionImmutable
import org.plasmalabs.sdk.models.transaction.IoTransaction
import org.plasmalabs.sdk.syntax.*

object SchemaIoTransaction {

  /**
   * IoTransaction model:
   * @see https://github.com/Topl/protobuf-specs/blob/main/proto/brambl/models/transaction/io_transaction.proto
   */
  object Field {
    val SchemaName = "Transaction"
    val TransactionId = "transactionId"
    val Transaction = "transaction"
    val Size = "size"
    val TransactionIndex = "transactionIdIndex"
    val IsReward = "isReward"
    val ParentBlock = "blockId"
  }

  def size(ioTransaction: IoTransaction): Long =
    ioTransactionImmutable.immutableBytes(ioTransaction).value.size

  def make(): VertexSchema[IoTransaction] =
    VertexSchema.create(
      Field.SchemaName,
      GraphDataEncoder[IoTransaction]
        // transactionID is not stored in a transaction, but computed
        .withProperty(
          Field.TransactionId,
          _.id.value.toByteArray,
          mandatory = false,
          readOnly = true,
          notNull = true
        )
        .withProperty(Field.Transaction, _.toByteArray, mandatory = false, readOnly = false, notNull = true)
        .withProperty(
          Field.Size,
          ioTransaction => java.lang.Long.valueOf(size(ioTransaction)),
          mandatory = true,
          readOnly = true,
          notNull = true
        )
        .withProperty(
          Field.IsReward,
          _ => java.lang.Boolean.valueOf(false),
          mandatory = false,
          readOnly = false,
          notNull = true
        )
        // TODO add index by field Field.IsReward
        .withIndex[IoTransaction](Field.TransactionIndex, Field.TransactionId)(using OIndexable.Instances.ioTransaction)
        .withLink(Field.ParentBlock, OType.LINK, SchemaBlockHeader.SchemaName),
      v => IoTransaction.parseFrom(v(Field.Transaction): Array[Byte])
    )

}
