package xyz.stratalab.indexer.interpreter

import cats.data.{EitherT, OptionT}
import cats.effect._
import cats.effect.implicits._
import cats.implicits._
import fs2.Stream
import org.typelevel.log4cats.Logger
import xyz.stratalab.algebras.NodeRpc
import xyz.stratalab.consensus.models.BlockId
import xyz.stratalab.indexer.algebras.NodeBlockFetcherAlgebra
import xyz.stratalab.indexer.model.{GE, GEs}
import xyz.stratalab.indexer.services.BlockData
import xyz.stratalab.node.models.FullBlockBody
import xyz.stratalab.sdk.models.TransactionId

import scala.collection.immutable.ListSet

import EitherT._

object NodeBlockFetcher {

  def make[F[_]: Async: Logger](
    nodeRpc:          NodeRpc[F, Stream[F, *]],
    fetchConcurrency: Int
  ): Resource[F, NodeBlockFetcherAlgebra[F, Stream[F, *]]] =
    Resource.pure {

      new NodeBlockFetcherAlgebra[F, Stream[F, *]] {

        override def fetch(startHeight: Long, endHeight: Long): F[Stream[F, BlockData]] =
          Async[F].delay {
            Stream
              // Range from given start height to either defined max height or "positive infinity".
              // If start height is one, then the range would be [1, 2, 3, ...]
              .range(startHeight, endHeight)
              .covary[F]
              .parEvalMap(fetchConcurrency)(fetch)
              .takeWhile(_.exists(_.nonEmpty), takeFailure = true)
              .evalMapFilter {
                case Left(ex) =>
                  Logger[F]
                    .error(s"Unexpected error while fetching block. Error=[$ex]")
                    .as(Option.empty[BlockData])
                case Right(None) =>
                  Logger[F]
                    .info("No block found.")
                    .as(Option.empty[BlockData])
                case Right(blockData @ Some(_)) =>
                  (blockData: Option[BlockData]).pure[F]
              }
          }

        override def fetch(height: Long): F[Either[GE, Option[BlockData]]] =
          nodeRpc
            .blockIdAtHeight(height)
            .flatMap {
              case Some(blockId) =>
                EitherT(fetch(blockId)).map(_.some).value
              case None =>
                Option.empty[BlockData].asRight[GE].pure[F]
            }

        override def fetch(blockId: BlockId): F[Either[GE, BlockData]] =
          (
            OptionT(nodeRpc.fetchBlockHeader(blockId)).toRight(GEs.HeaderNotFound(blockId): GE),
            OptionT(nodeRpc.fetchBlockBody(blockId))
              .toRight(GEs.BodyNotFound(blockId): GE)
              .flatMap(body =>
                (
                  fs2.Stream
                    .iterable[EitherT[F, GE, *], TransactionId](body.transactionIds)
                    .parEvalMap(fetchConcurrency)(id =>
                      OptionT(nodeRpc.fetchTransaction(id))
                        .toRight(GEs.TransactionsNotFound(ListSet(id)): GE)
                    )
                    .compile
                    .toList,
                  body.rewardTransactionId.traverse(id =>
                    OptionT(nodeRpc.fetchTransaction(id))
                      .toRight(GEs.TransactionsNotFound(ListSet(id)): GE)
                  )
                ).parMapN((transactions, reward) => FullBlockBody(transactions, reward))(
                  catsDataParallelForEitherTWithParallelEffect2
                )
              )
          ).parMapN(BlockData(_, _))(catsDataParallelForEitherTWithParallelEffect2).value

        def fetchHeight(): F[Option[Long]] =
          (for {
            headBlockId <- OptionT(nodeRpc.blockIdAtDepth(depth = 0))
            blockHeader <- OptionT(nodeRpc.fetchBlockHeader(headBlockId))
          } yield blockHeader.height).value

      }
    }
}
