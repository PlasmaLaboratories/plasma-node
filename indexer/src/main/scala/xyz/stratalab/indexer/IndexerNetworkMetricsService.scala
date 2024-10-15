package xyz.stratalab.indexer

import cats.data.EitherT
import cats.effect.kernel.Async
import io.grpc.Metadata
import xyz.stratalab.indexer.algebras.VertexFetcherAlgebra
import xyz.stratalab.indexer.services._

class GrpcNetworkMetricsService[F[_]: Async](vertexFetcher: VertexFetcherAlgebra[F])
    extends NetworkMetricsServiceFs2Grpc[F, Metadata] {

  def getTxoStats(request: GetTxoStatsReq, ctx: Metadata): F[GetTxoStatsRes] =
    EitherT(vertexFetcher.fetchTxoStats())
      .map(GetTxoStatsRes(_))
      .rethrowT
      .adaptErrorsToGrpc

  def getBlockchainSizeStats(request: BlockchainSizeStatsReq, ctx: Metadata): F[BlockchainSizeStatsRes] =
    EitherT(vertexFetcher.fetchBlockchainSizeStats())
      .map(BlockchainSizeStatsRes(_))
      .rethrowT
      .adaptErrorsToGrpc

  def getBlockStats(request: BlockStatsReq, ctx: Metadata): F[BlockStatsRes] =
    EitherT(vertexFetcher.fetchBlockStats())
      .map(BlockStatsRes(_))
      .rethrowT
      .adaptErrorsToGrpc
}
