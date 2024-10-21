package xyz.stratalab.indexer.interpreter

import cats.data.EitherT
import cats.effect.Resource
import cats.effect.kernel.Async
import cats.implicits._
import com.tinkerpop.blueprints.Vertex
import xyz.stratalab.consensus.models.{BlockHeader, BlockId}
import xyz.stratalab.indexer.algebras.{BlockFetcherAlgebra, VertexFetcherAlgebra}
import xyz.stratalab.indexer.model.GE
import xyz.stratalab.indexer.orientDb.OrientThread
import xyz.stratalab.indexer.orientDb.instances.VertexSchemaInstances.instances._
import xyz.stratalab.indexer.services.BlockData
import xyz.stratalab.node.models.{BlockBody, FullBlockBody}

object GraphBlockFetcher {

  def make[F[_]: Async: OrientThread](vertexFetcher: VertexFetcherAlgebra[F]): Resource[F, BlockFetcherAlgebra[F]] =
    Resource.pure {
      new BlockFetcherAlgebra[F] {

        override def fetchCanonicalHead(): F[Either[GE, Option[BlockHeader]]] =
          EitherT(vertexFetcher.fetchCanonicalHead())
            .semiflatMap(_.traverse(v => OrientThread[F].delay(blockHeaderSchema.decode(v))))
            .value

        override def fetchHeader(blockId: BlockId): F[Either[GE, Option[BlockHeader]]] =
          EitherT(vertexFetcher.fetchHeader(blockId))
            .semiflatMap(_.traverse(v => OrientThread[F].delay(blockHeaderSchema.decode(v))))
            .value

        override def fetchHeaderByHeight(height: Long): F[Either[GE, Option[BlockHeader]]] =
          EitherT(vertexFetcher.fetchHeaderByHeight(height))
            .semiflatMap(_.traverse(v => OrientThread[F].delay(blockHeaderSchema.decode(v))))
            .value

        override def fetchBody(blockId: BlockId): F[Either[GE, Option[BlockBody]]] =
          EitherT(vertexFetcher.fetchHeader(blockId)).flatMap {
            case Some(headerVertex) =>
              EitherT(vertexFetcher.fetchBody(headerVertex))
                .semiflatMap(_.traverse(v => OrientThread[F].delay(blockBodySchema.decode(v))))
            case None =>
              EitherT.pure[F, GE](Option.empty[BlockBody])
          }.value

        /**
         * Auxiliary def used by fetch BlockById and BlockByHeight
         * @param f a function that returns a Header Vertex
         * @return
         */
        private def fetchBlockFromVertex(f: () => F[Either[GE, Option[Vertex]]]): F[Either[GE, Option[BlockData]]] =
          EitherT(f())
            .flatMap(_.traverse { headerVertex =>
              EitherT
                .liftF(OrientThread[F].delay(blockHeaderSchema.decode(headerVertex)))
                .flatMap(header =>
                  EitherT(vertexFetcher.fetchBody(headerVertex))
                    .flatMap(
                      _.fold(EitherT.pure[F, GE](BlockData(header, FullBlockBody())))(_ =>
                        (
                          EitherT(vertexFetcher.fetchTransactions(headerVertex))
                            .semiflatMap(vs => OrientThread[F].delay(vs.map(ioTransactionSchema.decode))),
                          EitherT(vertexFetcher.fetchRewardTransaction(headerVertex))
                            .semiflatMap(vs => OrientThread[F].delay(vs.map(ioTransactionSchema.decode)))
                        )
                          .mapN((transactions, reward) => BlockData(header, FullBlockBody(transactions, reward)))
                      )
                    )
                )
            })
            .value

        override def fetchBlock(blockId: BlockId): F[Either[GE, Option[BlockData]]] =
          fetchBlockFromVertex(() => vertexFetcher.fetchHeader(blockId))

        override def fetchBlockByHeight(height: Long): F[Either[GE, Option[BlockData]]] =
          fetchBlockFromVertex(() => vertexFetcher.fetchHeaderByHeight(height))

        override def fetchBlockByDepth(depth: Long): F[Either[GE, Option[BlockData]]] =
          fetchBlockFromVertex(() => vertexFetcher.fetchHeaderByDepth(depth))

      }
    }
}
