package org.plasmalabs.indexer.interpreter

import cats.data.EitherT
import cats.effect.Resource
import cats.effect.kernel.Async
import cats.implicits._
import com.github.benmanes.caffeine.cache.Caffeine
import fs2.Stream
import org.plasmalabs.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import org.plasmalabs.indexer.algebras.{GraphReplicationStatusAlgebra, NodeBlockFetcherAlgebra, VertexFetcherAlgebra}
import org.plasmalabs.indexer.model.{GE, GEs}
import org.plasmalabs.indexer.orientDb.OrientThread
import org.plasmalabs.indexer.orientDb.instances.VertexSchemaInstances.instances._
import org.plasmalabs.typeclasses.implicits.showBlockId
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
                      nodeBlockFetcherAlgebra.fetchCanonicalHeadId().map {
                        case Some(nodeHeadBlockId) if nodeHeadBlockId == indexerBlockHeader.id =>
                          true.asRight[GE]
                        case Some(nodeHeadBlockId) =>
                          (GEs
                            .Internal(
                              new IllegalStateException(
                                show"Indexer canonical head [${indexerBlockHeader.id}] differs to Node [${nodeHeadBlockId}]"
                              )
                            ))
                            .asLeft[Boolean]
                        case None =>
                          (GEs
                            .Internal(new IllegalStateException("Node empty response at fetchCanonicalHeadId")))
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
