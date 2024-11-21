package org.plasmalabs.indexer.orientDb.instances

import com.orientechnologies.orient.core.sql.OCommandSQL
import com.tinkerpop.blueprints.impls.orient.{OrientDynaElementIterable, OrientGraph, OrientVertex}
import com.tinkerpop.blueprints.{Direction, Vertex}
import org.plasmalabs.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import org.plasmalabs.consensus.models.BlockHeader
import org.plasmalabs.indexer.orientDb.schema.EdgeSchemaInstances.*
import org.plasmalabs.indexer.orientDb.schema.VertexSchema
import org.plasmalabs.indexer.services.Txo
import org.plasmalabs.node.models.BlockBody
import org.plasmalabs.sdk.models.*
import org.plasmalabs.sdk.models.transaction.IoTransaction

import scala.jdk.CollectionConverters.*
import scala.language.implicitConversions

/**
 * Metadata describing the schema used for the Indexer graph in OrientDB
 */
object VertexSchemaInstances {

  trait Instances {
    val blockHeaderSchema: VertexSchema[BlockHeader] = SchemaBlockHeader.make()
    val blockBodySchema: VertexSchema[BlockBody] = SchemaBlockBody.make()
    val ioTransactionSchema: VertexSchema[IoTransaction] = SchemaIoTransaction.make()
    val lockAddressSchema: VertexSchema[LockAddress] = SchemaLockAddress.make()
    val txoSchema: VertexSchema[Txo] = SchemaTxo.make()
    val groupPolicySchema: VertexSchema[GroupPolicy] = SchemaGroupPolicy.make()
    val seriesPolicySchema: VertexSchema[SeriesPolicy] = SchemaSeriesPolicy.make()
  }

  trait Implicits {
    implicit def orientGraphAsOrientGraphOps(graph: OrientGraph): OrientGraphOps = new OrientGraphOps(graph)
  }
  object instances extends Instances
  object implicits extends Instances with Implicits
}

class OrientGraphOps(val graph: OrientGraph) extends AnyVal {
  import VertexSchemaInstances.instances._

  def addBlockHeader(blockHeader: BlockHeader): OrientVertex =
    graph.addVertex(s"class:${blockHeaderSchema.name}", blockHeaderSchema.encode(blockHeader).asJava)

  def addBody(blockBody: BlockBody): OrientVertex =
    graph.addVertex(s"class:${blockBodySchema.name}", blockBodySchema.encode(blockBody).asJava)

  def addIoTx(ioTx: IoTransaction): OrientVertex =
    graph.addVertex(s"class:${ioTransactionSchema.name}", ioTransactionSchema.encode(ioTx).asJava)

  def addLockAddress(address: LockAddress): OrientVertex =
    graph.addVertex(s"class:${lockAddressSchema.name}", lockAddressSchema.encode(address).asJava)

  def addTxo(txo: Txo): OrientVertex =
    graph.addVertex(s"class:${txoSchema.name}", txoSchema.encode(txo).asJava)

  def addGroupPolicy(groupPolicy: GroupPolicy): OrientVertex =
    graph.addVertex(s"class:${groupPolicySchema.name}", groupPolicySchema.encode(groupPolicy).asJava)

  def addSeriesPolicy(seriesPolicy: SeriesPolicy): OrientVertex =
    graph.addVertex(s"class:${seriesPolicySchema.name}", seriesPolicySchema.encode(seriesPolicy).asJava)

  def getBlockHeader(blockHeader: BlockHeader): Option[Vertex] =
    graph
      .command(
        new OCommandSQL(
          s"SELECT FROM ${blockHeaderSchema.name} WHERE ${SchemaBlockHeader.Field.BlockId} = ? LIMIT 1"
        )
      )
      .execute[OrientDynaElementIterable](blockHeader.id.value.toByteArray)
      .iterator()
      .asScala
      .collectFirst { case v: Vertex @unchecked => v }

  def getBody(blockHeaderVertex: Vertex): Option[Vertex] =
    blockHeaderVertex.getVertices(Direction.OUT, blockHeaderBodyEdge.label).asScala.headOption

  def getIoTxs(blockHeaderVertex: Vertex): Seq[Vertex] =
    blockHeaderVertex.getVertices(Direction.OUT, blockHeaderTxIOEdge.label).asScala.toSeq ++
    blockHeaderVertex.getVertices(Direction.OUT, blockHeaderRewardEdge.label).asScala.toSeq

  def getTxo(address: TransactionOutputAddress): Option[Vertex] =
    graph
      .getVertices(SchemaTxo.Field.TxoId, address.id.value.toByteArray :+ address.index.toByte)
      .asScala
      .headOption

  def getGroupPolicy(groupId: GroupId): Option[Vertex] =
    graph
      .getVertices(SchemaGroupPolicy.Field.GroupPolicyId, groupId.value.toByteArray)
      .asScala
      .headOption

  def getSeriesPolicy(seriesId: SeriesId): Option[Vertex] =
    graph
      .getVertices(SchemaSeriesPolicy.Field.SeriesPolicyId, seriesId.value.toByteArray)
      .asScala
      .headOption
}
