package org.plasmalabs.indexer.orientDb

import cats.implicits._
import fs2.Stream
import munit.{CatsEffectFunFixtures, CatsEffectSuite, ScalaCheckEffectSuite}
import org.plasmalabs.consensus.models.BlockHeader
import org.plasmalabs.indexer.DbFixtureUtil
import org.plasmalabs.indexer.algebras.{BlockFetcherAlgebra, NodeBlockFetcherAlgebra}
import org.plasmalabs.indexer.interpreter.GraphBlockUpdater
import org.plasmalabs.indexer.orientDb.OrientThread
import org.plasmalabs.indexer.orientDb.instances.VertexSchemaInstances.implicits._
import org.plasmalabs.indexer.orientDb.schema.EdgeSchemaInstances._
import org.plasmalabs.indexer.services.BlockData
import org.plasmalabs.models.generators.consensus.ModelGenerators._
import org.plasmalabs.models.generators.node.ModelGenerators._
import org.plasmalabs.node.models.FullBlockBody
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.jdk.CollectionConverters.IterableHasAsScala

class GraphBlockUpdaterFixtureTest
    extends CatsEffectSuite
    with ScalaCheckEffectSuite
    with AsyncMockFactory
    with CatsEffectFunFixtures
    with DbFixtureUtil {

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMaxSize(3)
      .withMinSuccessfulTests(5)

  override def munitIOTimeout: Duration =
    new FiniteDuration(10, TimeUnit.SECONDS)

  orientDbFixture.test("Insert and remove genesis block") { case (odbFactory, given OrientThread[F]) =>
    PropF.forAllF { (blockHeader: BlockHeader, blockBody: FullBlockBody) =>
      withMock {
        val nodeBlockFetcher = mock[NodeBlockFetcherAlgebra[F, Stream[F, *]]]
        val blockFetcher = mock[BlockFetcherAlgebra[F]]

        val res = for {
          databaseDocumentTx <- OrientThread[F].delay(odbFactory.getNoTx.getRawGraph).toResource

          _ <- Seq(
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
            .toResource

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
            .toResource

          blockData = BlockData(
            blockHeader.copy(height = 1),
            blockBody
          )

          dbTx <- OrientThread[F].delay(odbFactory.getTx).toResource

          graphBlockUpdater <- GraphBlockUpdater.make[F](dbTx, blockFetcher, nodeBlockFetcher)
          _                 <- graphBlockUpdater.insert(blockData).rethrow.toResource

          _ <- OrientThread[F]
            .delay(dbTx.getBlockHeader(blockData.header).flatMap(dbTx.getBody))
            .map(_.isDefined)
            .assert
            .toResource

          _ <- graphBlockUpdater.remove(blockData).rethrow.toResource

          // When we remove headerVertex, the canonicalHead schema is not updated, insertions handle it
          // When we remove txoVertex, lockAddress schema is not updated, because lockAddress references many txo
          // Eventually could create an orphan lockAddress, it is not a problem cause some future txo will reference it
          _ <- assertIOBoolean(OrientThread[F].delay(dbTx.getBlockHeader(blockData.header).isEmpty)).toResource
          _ <- assertIOBoolean(
            OrientThread[F].delay(dbTx.getVerticesOfClass(blockBodySchema.name).asScala.isEmpty)
          ).toResource
          _ <- assertIOBoolean(
            OrientThread[F].delay(dbTx.getVerticesOfClass(ioTransactionSchema.name).asScala.isEmpty)
          ).toResource
          _ <- assertIOBoolean(
            OrientThread[F].delay(dbTx.getVerticesOfClass(txoSchema.name).asScala.isEmpty)
          ).toResource
          _ <- assertIOBoolean(
            OrientThread[F].delay(dbTx.getVerticesOfClass(groupPolicySchema.name).asScala.isEmpty)
          ).toResource
          _ <- assertIOBoolean(
            OrientThread[F].delay(dbTx.getVerticesOfClass(seriesPolicySchema.name).asScala.isEmpty)
          ).toResource

        } yield ()

        res.use_
      }
    }
  }
}
