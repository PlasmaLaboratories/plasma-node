package xyz.stratalab.indexer

import cats.data.EitherT
import cats.effect.kernel.Async
import cats.implicits._
import io.grpc.Metadata
import xyz.stratalab.indexer.algebras.BlockFetcherAlgebra
import xyz.stratalab.indexer.model.GEs
import xyz.stratalab.indexer.services._
import xyz.stratalab.node.models.FullBlock
import xyz.stratalab.typeclasses.implicits._

class GrpcBlockService[F[_]: Async](blockFetcher: BlockFetcherAlgebra[F]) extends BlockServiceFs2Grpc[F, Metadata] {

  override def getBlockById(request: GetBlockByIdRequest, ctx: Metadata): F[BlockResponse] =
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

  override def getBlockByHeight(request: GetBlockByHeightRequest, ctx: Metadata): F[BlockResponse] =
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

  override def getBlockByDepth(request: GetBlockByDepthRequest, ctx: Metadata): F[BlockResponse] =
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
