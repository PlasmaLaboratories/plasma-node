package org.plasmalabs.indexer

import cats.effect.{IO, Resource, SyncIO}
import cats.implicits.*
import com.orientechnologies.orient.core.db.{ODatabaseType, OrientDB, OrientDBConfigBuilder}
import com.tinkerpop.blueprints.impls.orient.OrientGraphFactoryV2
import munit.{CatsEffectSuite, FunSuite, TestOptions}
import org.plasmalabs.indexer.orientDb.instances.VertexSchemaInstances.instances.*
import org.plasmalabs.indexer.orientDb.{OrientDBMetadataFactory, OrientThread}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

trait DbFixtureUtil { self: FunSuite & CatsEffectSuite =>

  type F[A] = IO[A]
  implicit val logger: Logger[F] = Slf4jLogger.getLoggerFromClass[F](this.getClass)

  val orientDbFixture: SyncIO[FunFixture[(OrientGraphFactoryV2, OrientThread[F])]] = {
    val dbName = "testDb"
    val dbConfig = new OrientDBConfigBuilder()
      .addGlobalUser("testUser", "testPass", "*")
      .build()

    val factoryR =
      OrientThread
        .create[F]
        .flatMap(orientThread =>
          Resource
            .make[F, OrientGraphFactoryV2](
              orientThread
                .delay {
                  val odb = new OrientDB("memory", "root", "root", dbConfig)
                  odb.createIfNotExists(dbName, ODatabaseType.MEMORY)

                  new OrientGraphFactoryV2(odb, dbName, "testUser", "testPass")
                }
            )(oDb => orientThread.delay(oDb.close()))
            .tupleRight(orientThread)
        )

    def setup(t: TestOptions, odb: (OrientGraphFactoryV2, OrientThread[F])): F[Unit] =
      for {
        databaseDocumentTx <- odb._2.delay(odb._1.getNoTx.getRawGraph)
        r <- Seq(
          blockHeaderSchema,
          blockBodySchema,
          ioTransactionSchema,
          lockAddressSchema,
          txoSchema,
          groupPolicySchema,
          seriesPolicySchema
        )
          .traverse(OrientDBMetadataFactory.createVertex[F](databaseDocumentTx, _))
          .void
      } yield r

    def teardown(odb: (OrientGraphFactoryV2, OrientThread[F])): IO[Unit] =
      odb._2
        .delay(odb._1.drop())
        .flatTap(_ => logger.info("Teardown, db dropped"))

    ResourceFixture(factoryR, setup, teardown)
  }

}
