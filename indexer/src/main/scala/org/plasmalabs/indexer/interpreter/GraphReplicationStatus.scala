package org.plasmalabs.indexer.interpreter

import cats.data.EitherT
import cats.effect.Resource
import cats.effect.kernel.Async
import cats.implicits._
import com.github.benmanes.caffeine.cache.Caffeine
import fs2.Stream
import org.plasmalabs.indexer.algebras.{GraphReplicationStatusAlgebra, NodeBlockFetcherAlgebra, VertexFetcherAlgebra}
import org.plasmalabs.indexer.model.{GE, GEs}
import org.plasmalabs.indexer.orientDb.OrientThread
import org.plasmalabs.indexer.orientDb.instances.VertexSchemaInstances.instances._
import scalacache.Entry
import scalacache.caffeine.CaffeineCache

import scala.concurrent.duration._

object GraphReplicationStatus {

  def make[F[_]: Async: OrientThread](
    vertexFetcher:           VertexFetcherAlgebra[F],
    nodeBlockFetcherAlgebra: NodeBlockFetcherAlgebra[F, Stream[F, *]],
    ttlCacheCheck:           Duration
  ): Resource[F, GraphReplicationStatusAlgebra[F]] =
    Resource.pure {
      new GraphReplicationStatusAlgebra[F] {

        private val cache = CaffeineCache[F, Unit, Either[GE, Boolean]](
          Caffeine.newBuilder
            .maximumSize(1)
            .build[Unit, Entry[Either[GE, Boolean]]]()
        )

        override def canonicalHeadSynced: F[Either[GE, Boolean]] =
          cache.cachingF(())(Some(ttlCacheCheck))(fecthAndCompareCanonicalHeads)

        private def fecthAndCompareCanonicalHeads: F[Either[GE, Boolean]] =
          EitherT(vertexFetcher.fetchCanonicalHead())
            .semiflatMap(_.traverse(v => OrientThread[F].delay(blockHeaderSchema.decode(v))))
            .biflatMap[GE, Boolean](
              ge => EitherT.fromEither(ge.asLeft[Boolean]),
              maybeBlockHeader =>
                maybeBlockHeader match {
                  case Some(indexerBlockHeader) =>
                    EitherT {
                      nodeBlockFetcherAlgebra.fetchHeight().map {
                        case Some(height) if height == indexerBlockHeader.height =>
                          true.asRight[GE]
                        case Some(height) =>
                          (GEs
                            .Internal(
                              new IllegalStateException(
                                s"Indexer canonical head height:[${indexerBlockHeader.height}] differs to Node head[${height}]"
                              )
                            ))
                            .asLeft[Boolean]
                        case None =>
                          (GEs
                            .Internal(new IllegalStateException("Node empty response at fetchHeight")))
                            .asLeft[Boolean]
                      }
                    }
                  case _ =>
                    EitherT(
                      (GEs
                        .Internal(new IllegalStateException("Indexer empty response at fetchCanonicalHead")): GE)
                        .asLeft[Boolean]
                        .pure[F]
                    )
                }
            )
            .value
      }
    }
}
