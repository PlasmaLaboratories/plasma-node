package org.plasmalabs.indexer.orientDb.instances

import cats.implicits._
import com.orientechnologies.orient.core.metadata.schema.OType
import munit.{CatsEffectFunFixtures, CatsEffectSuite, ScalaCheckEffectSuite}
import org.plasmalabs.indexer.DbFixtureUtil
import org.plasmalabs.indexer.orientDb.instances.SchemaTxo
import org.plasmalabs.indexer.orientDb.instances.SchemaTxo.Field
import org.plasmalabs.indexer.orientDb.instances.VertexSchemaInstances.instances.txoSchema
import org.plasmalabs.indexer.orientDb.{OrientDBMetadataFactory, OrientThread}
import org.plasmalabs.indexer.services.{Txo, TxoState}
import org.plasmalabs.models.ModelGenerators.GenHelper
import org.plasmalabs.sdk.generators.{ModelGenerators => BramblGens}
import org.scalamock.munit.AsyncMockFactory

import scala.jdk.CollectionConverters._

class SchemaTxoTest
    extends CatsEffectSuite
    with ScalaCheckEffectSuite
    with AsyncMockFactory
    with CatsEffectFunFixtures
    with DbFixtureUtil {

  orientDbFixture.test("Txo Schema Metadata") { case (odbFactory, implicit0(oThread: OrientThread[F])) =>
    val res = for {
      databaseDocumentTx <- oThread.delay(odbFactory.getNoTx.getRawGraph).toResource
      schema = SchemaTxo.make()
      _ <- OrientDBMetadataFactory.createVertex[F](databaseDocumentTx, schema).toResource

      oClass <- oThread.delay(databaseDocumentTx.getClass(schema.name)).toResource

      _ <- assertIO(oClass.getName.pure[F], schema.name, s"${schema.name} Class was not created").toResource

      networkProperty <- oClass.getProperty(Field.TransactionOutput).pure[F].toResource
      _ <- (
        assertIO(networkProperty.getName.pure[F], Field.TransactionOutput) &>
        assertIO(networkProperty.isMandatory.pure[F], true) &>
        assertIO(networkProperty.isReadonly.pure[F], true) &>
        assertIO(networkProperty.isNotNull.pure[F], true) &>
        assertIO(networkProperty.getType.pure[F], OType.BINARY)
      ).toResource

      ledgerProperty <- oClass.getProperty(Field.State).pure[F].toResource
      _ <- (
        assertIO(ledgerProperty.getName.pure[F], Field.State) &>
        assertIO(ledgerProperty.isMandatory.pure[F], true) &>
        assertIO(ledgerProperty.isReadonly.pure[F], false) &>
        assertIO(ledgerProperty.isNotNull.pure[F], true) &>
        assertIO(ledgerProperty.getType.pure[F], OType.INTEGER)
      ).toResource

      idProperty <- oClass.getProperty(Field.OutputAddress).pure[F].toResource
      _ <- (
        assertIO(idProperty.getName.pure[F], Field.OutputAddress) &>
        assertIO(idProperty.isMandatory.pure[F], true) &>
        assertIO(idProperty.isReadonly.pure[F], true) &>
        assertIO(idProperty.isNotNull.pure[F], true) &>
        assertIO(idProperty.getType.pure[F], OType.BINARY)
      ).toResource

    } yield ()

    res.use_

  }

  orientDbFixture.test("Txo Schema Add vertex") { case (odbFactory, implicit0(oThread: OrientThread[F])) =>
    val res = for {
      dbTx <- oThread.delay(odbFactory.getTx).toResource

      outputAddress <- BramblGens.arbitraryTransactionOutputAddress.arbitrary.first
        .pure[F]
        .toResource

      transactionOutput <- BramblGens.arbitraryUnspentTransactionOutput.arbitrary.first
        .pure[F]
        .toResource

      txo = Txo(transactionOutput, TxoState.UNSPENT, outputAddress)

      vertex <- oThread
        .delay(
          dbTx
            .addVertex(s"class:${Field.SchemaName}", txoSchema.encode(txo).asJava)
        )
        .toResource

      _ <- assertIO(
        vertex
          .getProperty[Array[Byte]](txoSchema.properties.filter(_.name == Field.TransactionOutput).head.name)
          .toSeq
          .pure[F],
        txo.transactionOutput.toByteArray.toSeq
      ).toResource

      _ <- assertIO(
        vertex.getProperty[Int](txoSchema.properties.filter(_.name == Field.State).head.name).pure[F],
        txo.state.value
      ).toResource

      _ <- assertIO(
        vertex
          .getProperty[Array[Byte]](txoSchema.properties.filter(_.name == Field.OutputAddress).head.name)
          .toSeq
          .pure[F],
        txo.outputAddress.toByteArray.toSeq
      ).toResource

      _ <- assertIO(
        oThread.delay(
          dbTx
            .getVertices(
              SchemaTxo.Field.TxoId,
              txo.outputAddress.id.value.toByteArray :+ txo.outputAddress.index.toByte
            )
            .iterator()
            .next()
            .getProperty[Array[Byte]](Field.TxoId)
            .toSeq
        ),
        (txo.outputAddress.id.value.toByteArray :+ txo.outputAddress.index.toByte).toSeq
      ).toResource

    } yield ()
    res.use_

  }

}
