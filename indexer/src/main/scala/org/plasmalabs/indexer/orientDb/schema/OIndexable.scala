package org.plasmalabs.indexer.orientDb.schema

import com.orientechnologies.orient.core.metadata.schema.OClass
import com.orientechnologies.orient.core.metadata.schema.OClass.INDEX_TYPE
import org.plasmalabs.consensus.models.BlockHeader
import org.plasmalabs.indexer.services.Txo
import org.plasmalabs.node.models.BlockBody
import org.plasmalabs.sdk.models.transaction.IoTransaction
import org.plasmalabs.sdk.models.{GroupPolicy, LockAddress, SeriesPolicy}

trait OIndexable[T] {
  def indexType: OClass.INDEX_TYPE
}

object OIndexable {

  def apply[A](implicit instance: OIndexable[A]): OIndexable[A] = instance

  trait Instances {

    val blockHeader: OIndexable[BlockHeader] = new OIndexable[BlockHeader] {
      override def indexType: OClass.INDEX_TYPE = INDEX_TYPE.UNIQUE
    }

    val blockHeightHeader: OIndexable[BlockHeader] = new OIndexable[BlockHeader] {
      override def indexType: OClass.INDEX_TYPE = INDEX_TYPE.NOTUNIQUE
    }

    val bodyHeader: OIndexable[BlockBody] = new OIndexable[BlockBody] {
      override def indexType: INDEX_TYPE = INDEX_TYPE.UNIQUE
    }

    val ioTransaction: OIndexable[IoTransaction] = new OIndexable[IoTransaction] {
      override def indexType: OClass.INDEX_TYPE = INDEX_TYPE.UNIQUE
    }

    val address: OIndexable[LockAddress] = new OIndexable[LockAddress] {
      override def indexType: OClass.INDEX_TYPE = INDEX_TYPE.UNIQUE
    }

    val txo: OIndexable[Txo] = new OIndexable[Txo] {
      override def indexType: OClass.INDEX_TYPE = INDEX_TYPE.UNIQUE
    }

    val groupPolicy: OIndexable[GroupPolicy] = new OIndexable[GroupPolicy] {
      override def indexType: OClass.INDEX_TYPE = INDEX_TYPE.UNIQUE
    }

    val seriesPolicy: OIndexable[SeriesPolicy] = new OIndexable[SeriesPolicy] {
      override def indexType: OClass.INDEX_TYPE = INDEX_TYPE.UNIQUE
    }
  }
  object Instances extends Instances
}
