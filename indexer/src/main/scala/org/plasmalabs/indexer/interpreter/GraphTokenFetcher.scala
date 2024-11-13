package org.plasmalabs.indexer.interpreter

import cats.data.EitherT
import cats.effect.Resource
import cats.effect.kernel.Async
import org.plasmalabs.indexer.algebras.{TokenFetcherAlgebra, VertexFetcherAlgebra}
import org.plasmalabs.indexer.model.GE
import org.plasmalabs.indexer.orientDb.instances.VertexSchemaInstances.instances._
import org.plasmalabs.sdk.models._

object GraphTokenFetcher {

  def make[F[_]: Async](vertexFetcher: VertexFetcherAlgebra[F]): Resource[F, TokenFetcherAlgebra[F]] =
    Resource.pure {
      new TokenFetcherAlgebra[F] {

        def fetchGroupPolicy(groupId: GroupId): F[Either[GE, Option[GroupPolicy]]] =
          EitherT(vertexFetcher.fetchGroupPolicy(groupId))
            .map(_.map(groupPolicySchema.decode))
            .value

        override def fetchSeriesPolicy(seriesId: SeriesId): F[Either[GE, Option[SeriesPolicy]]] =
          EitherT(vertexFetcher.fetchSeriesPolicy(seriesId))
            .map(_.map(seriesPolicySchema.decode))
            .value

      }
    }
}
