package org.plasmalabs.indexer

import cats.data.EitherT
import cats.effect.kernel.Async
import cats.implicits._
import io.grpc.Metadata
import org.plasmalabs.indexer.algebras.{BlockFetcherAlgebra, GraphReplicationStatusAlgebra}
import org.plasmalabs.indexer.model.GEs
import org.plasmalabs.indexer.services._
import org.plasmalabs.node.models.FullBlock
import org.plasmalabs.typeclasses.implicits._

class GrpcBlockService[F[_]: Async](
  blockFetcher:     BlockFetcherAlgebra[F],
  replicatorStatus: GraphReplicationStatusAlgebra[F]
) extends BlockServiceFs2Grpc[F, Metadata] {

  private def withSyncStatus[T](f: => F[T]): F[T] =
    replicatorStatus.canonicalHeadSynced.flatMap {
      case Right(true) => f
      case Right(false) =>
        Async[F]
          .raiseError[T](GEs.InternalMessage("Indexer replication job is not up to date with the Node"))
      case Left(ge) =>
        Async[F].raiseError[T](ge)

    }

  override def getBlockById(request: GetBlockByIdRequest, ctx: Metadata): F[BlockResponse] =
    withSyncStatus {
      EitherT(blockFetcher.fetchBlock(request.blockId))
        .foldF(
          ge => Async[F].raiseError[BlockData](GEs.Internal(ge)),
          _.map(_.pure[F])
            .getOrElse(
              Async[F]
                .raiseError[BlockData](GEs.NotFound(s"BlockId:${request.blockId.show}"))
            )
        )
        .map(blockData => BlockResponse(FullBlock(blockData.header, blockData.body)))
        .adaptErrorsToGrpc
    }

  override def getBlockByHeight(request: GetBlockByHeightRequest, ctx: Metadata): F[BlockResponse] =
    withSyncStatus {
      EitherT(blockFetcher.fetchBlockByHeight(request.height.value))
        .foldF(
          ge => Async[F].raiseError[BlockData](GEs.Internal(ge)),
          _.map(_.pure[F])
            .getOrElse(
              Async[F].raiseError[BlockData](GEs.NotFound(s"Height:${request.height.value.show}"))
            )
        )
        .map(blockData => BlockResponse(FullBlock(blockData.header, blockData.body)))
        .adaptErrorsToGrpc
    }

  override def getBlockByDepth(request: GetBlockByDepthRequest, ctx: Metadata): F[BlockResponse] =
    withSyncStatus {
      EitherT(blockFetcher.fetchBlockByDepth(request.depth.value))
        .foldF(
          ge => Async[F].raiseError[BlockData](GEs.Internal(ge)),
          _.map(_.pure[F])
            .getOrElse(
              Async[F].raiseError[BlockData](GEs.NotFound(s"Depth:${request.depth.value.show}"))
            )
        )
        .map(blockData => BlockResponse(FullBlock(blockData.header, blockData.body)))
        .adaptErrorsToGrpc
    }

}
