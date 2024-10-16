package xyz.stratalab.indexer.orientDb

import cats.implicits._
import fs2.Stream
import munit.{CatsEffectFunFixtures, CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory
import xyz.stratalab.consensus.models.BlockHeader
import xyz.stratalab.indexer.DbFixtureUtil
import xyz.stratalab.indexer.algebras.{BlockFetcherAlgebra, NodeBlockFetcherAlgebra}
import xyz.stratalab.indexer.interpreter.GraphBlockUpdater
import xyz.stratalab.indexer.orientDb.instances.VertexSchemaInstances.implicits._
import xyz.stratalab.indexer.orientDb.schema.EdgeSchemaInstances._
import xyz.stratalab.indexer.orientDb.{OrientDBMetadataFactory, OrientThread}
import xyz.stratalab.indexer.services.BlockData
import xyz.stratalab.models.generators.consensus.ModelGenerators._
import xyz.stratalab.models.generators.node.ModelGenerators._
import xyz.stratalab.node.models.FullBlockBody

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

  orientDbFixture.test("Insert and remove genesis block") { case (odbFactory, implicit0(oThread: OrientThread[F])) =>
    PropF.forAllF { (blockHeader: BlockHeader, blockBody: FullBlockBody) =>
      withMock {
        val nodeBlockFetcher = mock[NodeBlockFetcherAlgebra[F, Stream[F, *]]]
        val blockFetcher = mock[BlockFetcherAlgebra[F]]

        val res = for {
          databaseDocumentTx <- oThread.delay(odbFactory.getNoTx.getRawGraph).toResource

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

          dbTx <- oThread.delay(odbFactory.getTx).toResource

          graphBlockUpdater <- GraphBlockUpdater.make[F](dbTx, blockFetcher, nodeBlockFetcher)
          _                 <- graphBlockUpdater.insert(blockData).rethrow.toResource

          _ <- oThread
            .delay(dbTx.getBlockHeader(blockData.header).flatMap(dbTx.getBody))
            .map(_.isDefined)
            .assert
            .toResource

          _ <- graphBlockUpdater.remove(blockData).rethrow.toResource

          // When we remove headerVertex, the canonicalHead schema is not updated, insertions handle it
          // When we remove txoVertex, lockAddress schema is not updated, because lockAddress references many txo
          // Eventually could create an orphan lockAddress, it is not a problem cause some future txo will reference it
          _ <- assertIOBoolean(oThread.delay(dbTx.getBlockHeader(blockData.header).isEmpty)).toResource
          _ <- assertIOBoolean(oThread.delay(dbTx.getVerticesOfClass(blockBodySchema.name).asScala.isEmpty)).toResource
          _ <- assertIOBoolean(
            oThread.delay(dbTx.getVerticesOfClass(ioTransactionSchema.name).asScala.isEmpty)
          ).toResource
          _ <- assertIOBoolean(oThread.delay(dbTx.getVerticesOfClass(txoSchema.name).asScala.isEmpty)).toResource
          _ <- assertIOBoolean(
            oThread.delay(dbTx.getVerticesOfClass(groupPolicySchema.name).asScala.isEmpty)
          ).toResource
          _ <- assertIOBoolean(
            oThread.delay(dbTx.getVerticesOfClass(seriesPolicySchema.name).asScala.isEmpty)
          ).toResource

        } yield ()

        res.use_
      }
    }
  }
}
