package xyz.stratalab.genus.interpreter

import cats.effect.{IO, Resource}
import cats.implicits._
import co.topl.consensus.models.BlockHeader
import co.topl.genus.services.BlockData
import com.tinkerpop.blueprints.Vertex
import com.tinkerpop.blueprints.impls.orient.{OrientEdge, OrientGraph, OrientVertex}
import fs2.Stream
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import xyz.stratalab.genus.algebras.{BlockFetcherAlgebra, NodeBlockFetcherAlgebra}
import xyz.stratalab.genus.interpreter.GraphBlockUpdater
import xyz.stratalab.genus.model.{GE, GEs}
import xyz.stratalab.genus.orientDb.OrientThread
import xyz.stratalab.models.generators.consensus.ModelGenerators._

import java.lang
import scala.annotation.nowarn
import scala.jdk.CollectionConverters._

class GraphBlockUpdaterTest extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  implicit private val logger: Logger[F] = Slf4jLogger.getLoggerFromClass[F](this.getClass)

  test("Insert genesis block, should fail, if we can not add the vertex") {
    val orientGraph: OrientGraph = new OrientGraph("memory:test") {}

    PropF.forAllF { (blockHeader: BlockHeader) =>
      withMock {
        val nodeBlockFetcher = mock[NodeBlockFetcherAlgebra[F, Stream[F, *]]]
        val blockFetcher = mock[BlockFetcherAlgebra[F]]
        val res = for {
          given OrientThread[F] <- OrientThread.create[F]
          blockData                                <- Resource.pure(BlockData(blockHeader.copy(height = 1), null, null))
          graphBlockUpdater <- GraphBlockUpdater.make[F](orientGraph, blockFetcher, nodeBlockFetcher)
          _ <- assertIO(
            graphBlockUpdater.insert(blockData),
            (GEs.InternalMessage("boom!"): GE).asLeft[Unit]
          ).toResource
        } yield ()
        res.use_
      }
    }
    orientGraph.shutdown()
  }

  test("Insert genesis block, should work, if we add the vertex") {

    val orientGraph: OrientGraph = new OrientGraph("memory:test") {

      @nowarn
      override def addVertex(id: Object, prop: AnyRef*): OrientVertex = new OrientVertex()
    }

    PropF.forAllF { (blockHeader: BlockHeader) =>
      withMock {
        val nodeBlockFetcher = mock[NodeBlockFetcherAlgebra[F, Stream[F, *]]]
        val blockFetcher = mock[BlockFetcherAlgebra[F]]
        val res = for {
          given OrientThread[F] <- OrientThread.create[F]
          blockData                                <- Resource.pure(BlockData(blockHeader.copy(height = 1), null, null))
          graphBlockUpdater <- GraphBlockUpdater.make[F](orientGraph, blockFetcher, nodeBlockFetcher)
          _ <- assertIO(
            graphBlockUpdater.insert(blockData),
            ().asRight[GE]
          ).toResource
        } yield ()

        res.use_

      }
    }
    orientGraph.shutdown()
  }

  test("Insert no genesis block, should fail, if we can not add the vertex") {
    val orientGraph: OrientGraph = new OrientGraph("memory:test") {}
    PropF.forAllF { (blockHeader: BlockHeader) =>
      withMock {
        val nodeBlockFetcher = mock[NodeBlockFetcherAlgebra[F, Stream[F, *]]]
        val blockFetcher = mock[BlockFetcherAlgebra[F]]
        val res = for {
          given OrientThread[F] <- OrientThread.create[F]
          blockData                                <- Resource.pure(BlockData(blockHeader.copy(height = 2), null, null))
          graphBlockUpdater <- GraphBlockUpdater.make[F](orientGraph, blockFetcher, nodeBlockFetcher)
          _ <- assertIO(
            graphBlockUpdater.insert(blockData),
            (GEs.InternalMessage("boom!"): GE).asLeft[Unit]
          ).toResource
        } yield ()
        res.use_

      }
    }
    orientGraph.shutdown()
  }

  test("Insert no genesis block, should work, if we add the vertex and the edge") {

    val orientGraph: OrientGraph = new OrientGraph("memory:test") {

      @nowarn
      override def addVertex(id: Object, prop: AnyRef*): OrientVertex = new OrientVertex()

      override def getVertices(id: String, prop: Object): lang.Iterable[Vertex] =
        Iterable(new OrientVertex(): Vertex).asJava

      override def addEdge(id: Object, o: Vertex, i: Vertex, l: String): OrientEdge =
        new OrientEdge()
    }

    PropF.forAllF { (blockHeader: BlockHeader) =>
      withMock {
        val nodeBlockFetcher = mock[NodeBlockFetcherAlgebra[F, Stream[F, *]]]
        val blockFetcher = mock[BlockFetcherAlgebra[F]]
        val res = for {
          given OrientThread[F] <- OrientThread.create[F]
          blockData                                <- Resource.pure(BlockData(blockHeader.copy(height = 2), null, null))
          graphBlockUpdater <- GraphBlockUpdater.make[F](orientGraph, blockFetcher, nodeBlockFetcher)
          _ <- assertIO(
            graphBlockUpdater.insert(blockData),
            ().asRight[GE]
          ).toResource
        } yield ()
        res.use_
      }
    }
    orientGraph.shutdown()
  }

  test("Insert no genesis block, should fail, if we add the vertex but no the edge") {
    val orientGraph: OrientGraph = new OrientGraph("memory:test") {

      @nowarn
      override def addVertex(id: Object, prop: AnyRef*): OrientVertex = new OrientVertex()

      override def getVertices(id: String, prop: Object): lang.Iterable[Vertex] =
        Iterable(new OrientVertex(): Vertex).asJava

      override def addEdge(id: Object, o: Vertex, i: Vertex, l: String): OrientEdge =
        throw new IllegalStateException("boom!")
    }

    PropF.forAllF { (blockHeader: BlockHeader) =>
      withMock {
        val nodeBlockFetcher = mock[NodeBlockFetcherAlgebra[F, Stream[F, *]]]
        val blockFetcher = mock[BlockFetcherAlgebra[F]]
        val res = for {
          given OrientThread[F] <- OrientThread.create[F]
          blockData                                <- Resource.pure(BlockData(blockHeader.copy(height = 2), null, null))
          graphBlockUpdater <- GraphBlockUpdater.make[F](orientGraph, blockFetcher, nodeBlockFetcher)
          _ <- assertIO(
            graphBlockUpdater.insert(blockData),
            (GEs.InternalMessage("boom!"): GE).asLeft[Unit]
          ).toResource
        } yield ()
        res.use_

      }
    }
    orientGraph.shutdown()
  }
}