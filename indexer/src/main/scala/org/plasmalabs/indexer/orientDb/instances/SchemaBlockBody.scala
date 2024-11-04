package org.plasmalabs.indexer.orientDb.instances

import com.orientechnologies.orient.core.metadata.schema.OType
import org.plasmalabs.indexer.orientDb.schema.OIndexable.Instances
import org.plasmalabs.indexer.orientDb.schema.OTyped.Instances._
import org.plasmalabs.indexer.orientDb.schema.{GraphDataEncoder, VertexSchema}
import org.plasmalabs.node.models.BlockBody

object SchemaBlockBody {

  /**
   * BlockBody model fields:
   * @see https://github.com/Topl/protobuf-specs/blob/main/proto/node/models/block.proto
   */
  object Field {
    val SchemaName: String = "BlockBody"
    // Legacy-compatibility, otherwise "header" would be a better name since the field itself is a link
    val Header: String = SchemaBlockHeader.Field.BlockId
    val TransactionIds: String = "transactionIds"

    val BodyHeaderIndex: String = "bodyHeaderIndex"
  }

  def make(): VertexSchema[BlockBody] = VertexSchema.create(
    Field.SchemaName,
    GraphDataEncoder[BlockBody]
      .withProperty(Field.TransactionIds, _.toByteArray, mandatory = false, readOnly = false, notNull = false)
      .withLink(Field.Header, OType.LINK, SchemaBlockHeader.SchemaName)
      .withIndex[BlockBody](Field.BodyHeaderIndex, Field.Header)(using Instances.bodyHeader),
    v => BlockBody.parseFrom(v(Field.TransactionIds): Array[Byte])
  )

}
