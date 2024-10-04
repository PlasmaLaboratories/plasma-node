package xyz.stratalab.indexer.interpreter

import cats.data.EitherT
import cats.effect.Resource
import cats.effect.kernel.Async
import xyz.stratalab.indexer.algebras.{TokenFetcherAlgebra, VertexFetcherAlgebra}
import xyz.stratalab.indexer.model.GE
import xyz.stratalab.indexer.orientDb.instances.VertexSchemaInstances.instances._
import xyz.stratalab.sdk.models.Event.GroupPolicy
import xyz.stratalab.sdk.models.{Event, GroupId, SeriesId}

object GraphTokenFetcher {

  def make[F[_]: Async](vertexFetcher: VertexFetcherAlgebra[F]): Resource[F, TokenFetcherAlgebra[F]] =
    Resource.pure {
      new TokenFetcherAlgebra[F] {

        def fetchGroupPolicy(groupId: GroupId): F[Either[GE, Option[GroupPolicy]]] =
          EitherT(vertexFetcher.fetchGroupPolicy(groupId))
            .map(_.map(groupPolicySchema.decode))
            .value

        override def fetchSeriesPolicy(seriesId: SeriesId): F[Either[GE, Option[Event.SeriesPolicy]]] =
          EitherT(vertexFetcher.fetchSeriesPolicy(seriesId))
            .map(_.map(seriesPolicySchema.decode))
            .value

      }
    }
}
