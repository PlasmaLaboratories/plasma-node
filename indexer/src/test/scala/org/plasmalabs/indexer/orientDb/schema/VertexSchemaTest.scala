package org.plasmalabs.indexer.orientDb.schema

import cats.implicits._
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.plasmalabs.indexer.DbFixtureUtil
import org.plasmalabs.indexer.orientDb.schema.OTyped.Instances._
import org.plasmalabs.indexer.orientDb.schema.{GraphDataEncoder, VertexSchema}
import org.plasmalabs.indexer.orientDb.{OrientDBMetadataFactory, OrientThread}
import org.scalamock.munit.AsyncMockFactory

import scala.jdk.CollectionConverters._

class VertexSchemaTest extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory with DbFixtureUtil {

//

  val StringParamName = "strParam"
  case class TestClass(strParam: String)

  // TODO
  // create 1 field for each OTyped Instance. Test them
  // create index, links. Test them
  private val testSchema: VertexSchema[TestClass] = VertexSchema.create(
    "SchemaNameTest",
    GraphDataEncoder[TestClass]
      .withProperty(StringParamName, _.strParam, mandatory = true, readOnly = true, notNull = false),
    v => TestClass(v(StringParamName): String)
  )

  orientDbFixtureNew.test("Test Schema Metadata") { case (odbFactory, implicit0(oThread: OrientThread[F])) =>
    val res = for {
      databaseDocumentTx <- oThread.delay(odbFactory.getNoTx.getRawGraph).toResource
      _                  <- OrientDBMetadataFactory.createVertex[F](databaseDocumentTx, testSchema).toResource

      oClass <- oThread.delay(databaseDocumentTx.getClass(testSchema.name)).toResource

      _ <- assertIO(oClass.getName.pure[F], testSchema.name, "Test Class was not created").toResource
      _ <- assertIO(oClass.getProperty(StringParamName).getName.pure[F], StringParamName).toResource
      _ <- assertIOBoolean(oClass.getProperty(StringParamName).isMandatory.pure[F]).toResource
      _ <- assertIOBoolean(oClass.getProperty(StringParamName).isReadonly.pure[F]).toResource
      _ <- assertIO(oClass.getProperty(StringParamName).isNotNull.pure[F], false).toResource

    } yield ()

    res.use_

  }

  orientDbFixtureNew.test("Test Schema Add Vertex") { case (odbFactory, implicit0(oThread: OrientThread[F])) =>
    val res = for {
      databaseDocumentTx <- oThread.delay(odbFactory.getNoTx.getRawGraph).toResource
      _                  <- OrientDBMetadataFactory.createVertex[F](databaseDocumentTx, testSchema).toResource

      dbTx <- oThread.delay(odbFactory.getTx).toResource
      _    <- oThread.delay(dbTx.makeActive()).toResource

      vertex <- oThread
        .delay(
          dbTx.addVertex(s"class:${testSchema.name}", testSchema.encode(TestClass("test string value")).asJava)
        )
        .toResource

      _ <- assertIO(vertex.getProperty[String](StringParamName).pure[F], "test string value").toResource

    } yield ()

    res.use_

  }

}
