package org.plasmalabs.indexer

import cats.data.EitherT
import cats.effect.kernel.Async
import io.grpc.Metadata
import org.plasmalabs.indexer.algebras.TokenFetcherAlgebra
import org.plasmalabs.indexer.services.*

class GrpcTokenService[F[_]: Async](
  tokenFetcherAlgebra: TokenFetcherAlgebra[F]
) extends TokenServiceFs2Grpc[F, Metadata] {

  override def getGroupPolicy(request: QueryByGroupIdRequest, ctx: Metadata): F[GroupPolicyResponse] =
    EitherT(tokenFetcherAlgebra.fetchGroupPolicy(request.groupId))
      .map(GroupPolicyResponse(_))
      .rethrowT
      .adaptErrorsToGrpc

  override def getSeriesPolicy(request: QueryBySeriesIdRequest, ctx: Metadata): F[SeriesPolicyResponse] =
    EitherT(tokenFetcherAlgebra.fetchSeriesPolicy(request.seriesId))
      .map(SeriesPolicyResponse(_))
      .rethrowT
      .adaptErrorsToGrpc
}
