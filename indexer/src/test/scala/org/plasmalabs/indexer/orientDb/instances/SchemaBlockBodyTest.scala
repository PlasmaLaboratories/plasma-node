package org.plasmalabs.indexer.orientDb.instances

import cats.implicits._
import com.orientechnologies.orient.core.metadata.schema.OType
import munit.{CatsEffectFunFixtures, CatsEffectSuite, ScalaCheckEffectSuite}
import org.plasmalabs.indexer.DbFixtureUtil
import org.plasmalabs.indexer.orientDb.OrientThread
import org.plasmalabs.indexer.orientDb.instances.SchemaBlockBody.Field
import org.plasmalabs.indexer.orientDb.instances.VertexSchemaInstances.instances.blockBodySchema
import org.plasmalabs.node.models.BlockBody
import org.scalamock.munit.AsyncMockFactory

import scala.jdk.CollectionConverters._

class SchemaBlockBodyTest
    extends CatsEffectSuite
    with ScalaCheckEffectSuite
    with AsyncMockFactory
    with CatsEffectFunFixtures
    with DbFixtureUtil {

  orientDbFixtureNew.test("Block body Schema Metadata") { case (odbFactory, implicit0(oThread: OrientThread[F])) =>
    val res = for {
      dbNoTx <- oThread.delay(odbFactory.getNoTx).toResource

      databaseDocumentTx <- oThread.delay(dbNoTx.getRawGraph).toResource

      oClass <- oThread.delay(databaseDocumentTx.getClass(Field.SchemaName)).toResource

      _ <- assertIO(oClass.getName.pure[F], Field.SchemaName, s"${Field.SchemaName} Class was not created").toResource

      txIdsProperty <- oClass.getProperty(Field.TransactionIds).pure[F].toResource
      _ <- (
        assertIO(txIdsProperty.getName.pure[F], Field.TransactionIds) &>
        assertIO(txIdsProperty.isMandatory.pure[F], false) &>
        assertIO(txIdsProperty.isReadonly.pure[F], false) &>
        assertIO(txIdsProperty.isNotNull.pure[F], false) &>
        assertIO(txIdsProperty.getType.pure[F], OType.BINARY)
      ).toResource

    } yield ()

    res.use_

  }

  orientDbFixtureNew.test("Block Body Schema Add vertex") { case (odbFactory, implicit0(oThread: OrientThread[F])) =>
    val res = for {

      dbTx <- oThread.delay(odbFactory.getTx).toResource

      vertex <- oThread
        .delay(dbTx.addVertex(s"class:${blockBodySchema.name}", blockBodySchema.encode(BlockBody(Seq.empty)).asJava))
        .toResource

      _ <- assertIO(
        vertex
          .getProperty[Array[Byte]](blockBodySchema.properties.filter(_.name == Field.TransactionIds).head.name)
          .toSeq
          .pure[F],
        Array.empty[Byte].toSeq
      ).toResource

    } yield ()
    res.use_

  }

}
