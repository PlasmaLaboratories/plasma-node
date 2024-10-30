package org.plasmalabs.indexer

import cats.effect.{IO, Resource, SyncIO}
import cats.implicits._
import com.orientechnologies.orient.core.db.{ODatabaseType, OrientDB, OrientDBConfigBuilder}
import com.tinkerpop.blueprints.impls.orient.OrientGraphFactoryV2
import munit.{CatsEffectSuite, FunSuite}
import org.plasmalabs.indexer.orientDb.instances.VertexSchemaInstances.instances._
import org.plasmalabs.indexer.orientDb.schema.EdgeSchemaInstances.{
  addressTxIOEdge,
  addressTxoEdge,
  blockHeaderBodyEdge,
  blockHeaderEdge,
  blockHeaderRewardEdge,
  blockHeaderTxIOEdge
}
import org.plasmalabs.indexer.orientDb.{OrientDBMetadataFactory, OrientThread}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.util.Random

trait DbFixtureUtil { self: FunSuite with CatsEffectSuite =>

  type F[A] = IO[A]
  implicit val logger: Logger[F] = Slf4jLogger.getLoggerFromClass[F](this.getClass)

  def orientDbFixtureNew: SyncIO[FunFixture[(OrientGraphFactoryV2, OrientThread[F])]] = {
    val dbName = "testDb" + Random.alphanumeric.take(3).mkString
    val dbConfig = new OrientDBConfigBuilder()
      .addGlobalUser("testUser", "testPass", "*")
      .build()

    val graphFactoryandThreadResource =
      OrientThread
        .create[F]
        .flatMap(orientThread =>
          Resource
            .make[F, OrientGraphFactoryV2](
              for {

                odb                <- orientThread.delay(new OrientDB("memory", "root", "root", dbConfig))
                _                  <- orientThread.delay(odb.createIfNotExists(dbName, ODatabaseType.MEMORY))
                graph              <- orientThread.delay(new OrientGraphFactoryV2(odb, dbName, "testUser", "testPass"))
                databaseDocumentTx <- orientThread.delay(graph.getNoTx.getRawGraph)

                _ <- Seq(
                  blockHeaderSchema,
                  blockBodySchema,
                  ioTransactionSchema,
                  lockAddressSchema,
                  txoSchema,
                  groupPolicySchema,
                  seriesPolicySchema
                ).traverse(OrientDBMetadataFactory.createVertex[F](databaseDocumentTx, _)).void

                _ <- Seq(
                  blockHeaderEdge,
                  blockHeaderBodyEdge,
                  blockHeaderTxIOEdge,
                  blockHeaderRewardEdge,
                  addressTxIOEdge,
                  addressTxoEdge
                )
                  .traverse(e => OrientDBMetadataFactory.createEdge[F](databaseDocumentTx, e))
                  .void

              } yield graph
            )(graph =>
              orientThread.delay {
                graph.drop()
                graph.close()
              }
            )
            .tupleRight(orientThread)
        )

    ResourceFunFixture(graphFactoryandThreadResource)
  }

}
