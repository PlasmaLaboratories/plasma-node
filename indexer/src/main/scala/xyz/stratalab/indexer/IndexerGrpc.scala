package xyz.stratalab.indexer

import cats.effect.kernel.{Async, Resource}
import cats.implicits._
import cats.{Eval, Now}
import fs2.grpc.syntax.all._
import io.grpc.netty.shaded.io.grpc.netty.NettyChannelBuilder
import io.grpc.{Metadata, ServerServiceDefinition}
import xyz.stratalab.algebras.IndexerRpc
import xyz.stratalab.indexer.algebras.{
  BlockFetcherAlgebra,
  TokenFetcherAlgebra,
  TransactionFetcherAlgebra,
  VertexFetcherAlgebra
}
import xyz.stratalab.indexer.services._

object IndexerGrpc {

  object Client {

    /**
     * Creates a Indexer RPC Client for interacting with a Node node
     *
     * @param host Indexer node host/IP
     * @param port Indexer node port
     * @param tls  Should the connection use TLS?
     */
    def make[F[_]: Async](host: String, port: Int, tls: Boolean): Resource[F, IndexerRpc[F]] =
      Eval
        .now(NettyChannelBuilder.forAddress(host, port))
        .flatMap(ncb =>
          Eval
            .now(tls)
            .ifM(
              Now(ncb.useTransportSecurity()),
              Now(ncb.usePlaintext())
            )
        )
        .value
        .resource[F]
        .flatMap(BlockServiceFs2Grpc.stubResource[F])
        .map(client =>
          new IndexerRpc[F] {

            override def blockIdAtHeight(height: Long): F[BlockResponse] =
              client.getBlockByHeight(
                GetBlockByHeightRequest(
                  ChainDistance(height),
                  confidenceFactor = None
                ),
                new Metadata()
              )
          }
        )

  }

  object Server {

    def services[F[_]: Async](
      blockFetcher:       BlockFetcherAlgebra[F],
      transactionFetcher: TransactionFetcherAlgebra[F],
      vertexFetcher:      VertexFetcherAlgebra[F],
      valueFetcher:       TokenFetcherAlgebra[F]
    ): Resource[F, List[ServerServiceDefinition]] =
      List(
        BlockServiceFs2Grpc.bindServiceResource(new GrpcBlockService(blockFetcher)),
        TransactionServiceFs2Grpc.bindServiceResource(new GrpcTransactionService(transactionFetcher)),
        NetworkMetricsServiceFs2Grpc.bindServiceResource(new GrpcNetworkMetricsService(vertexFetcher)),
        TokenServiceFs2Grpc.bindServiceResource(new GrpcTokenService(valueFetcher))
      ).sequence

  }
}
