package org.plasmalabs.networking.fsnetwork

import cats.MonadThrow
import cats.data.{EitherT, NonEmptyChain, OptionT}
import cats.effect.{Async, Resource}
import cats.implicits.*
import fs2.Stream
import org.plasmalabs.actor.{Actor, Fsm}
import org.plasmalabs.algebras.Store
import org.plasmalabs.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import org.plasmalabs.consensus.algebras.BlockHeaderToBodyValidationAlgebra
import org.plasmalabs.consensus.models.BlockHeaderToBodyValidationFailure.IncorrectTxRoot
import org.plasmalabs.consensus.models.{BlockHeader, BlockId}
import org.plasmalabs.models.p2p.*
import org.plasmalabs.networking.blockchain.BlockchainPeerClient
import org.plasmalabs.networking.fsnetwork.BlockDownloadError.BlockBodyOrTransactionError
import org.plasmalabs.networking.fsnetwork.BlockDownloadError.BlockBodyOrTransactionError.*
import org.plasmalabs.networking.fsnetwork.P2PShowInstances.*
import org.plasmalabs.networking.fsnetwork.RequestsProxy.RequestsProxyActor
import org.plasmalabs.node.models.{Block, BlockBody}
import org.plasmalabs.sdk.models.TransactionId
import org.plasmalabs.sdk.models.transaction.IoTransaction
import org.plasmalabs.sdk.validation.algebras.TransactionSyntaxVerifier
import org.plasmalabs.typeclasses.implicits.*
import org.typelevel.log4cats.Logger

object PeerBlockBodyFetcher {
  sealed trait Message

  object Message {
    case object StartActor extends Message
    case object StopActor extends Message

    /**
     * Request to download block bodies from peer, downloaded bodies will be sent to block checker directly
     *
     * @param blockHeaders bodies block header to download
     */
    case class DownloadBlocks(blockHeaders: NonEmptyChain[BlockHeader]) extends Message
  }

  case class State[F[_]](
    hostId:                 HostId,
    client:                 BlockchainPeerClient[F],
    requestsProxy:          RequestsProxyActor[F],
    transactionStore:       Store[F, TransactionId, IoTransaction],
    headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F],
    transactionFetcher:     TransactionFetcher[F]
  )

  type Response[F[_]] = State[F]
  type PeerBlockBodyFetcherActor[F[_]] = Actor[F, Message, Response[F]]

  def getFsm[F[_]: Async: Logger]: Fsm[F, State[F], Message, Response[F]] = Fsm {
    case (state, Message.DownloadBlocks(blockHeadersToDownload)) => downloadBodies(state, blockHeadersToDownload)
    case (state, Message.StartActor)                             => startActor(state)
    case (state, Message.StopActor)                              => stopActor(state)
  }

  def makeActor[F[_]: Async: Logger](
    hostId:                      HostId,
    client:                      BlockchainPeerClient[F],
    requestsProxy:               RequestsProxyActor[F],
    transactionStore:            Store[F, TransactionId, IoTransaction],
    headerToBodyValidation:      BlockHeaderToBodyValidationAlgebra[F],
    transactionSyntaxValidation: TransactionSyntaxVerifier[F]
  ): Resource[F, PeerBlockBodyFetcherActor[F]] = {
    val transactionFetcher =
      new TransactionFetcher[F](hostId, transactionSyntaxValidation, transactionStore, client)
    val initialState =
      State(hostId, client, requestsProxy, transactionStore, headerToBodyValidation, transactionFetcher)

    val actorName = show"Body fetcher actor for peer $hostId"
    Actor.makeWithFinalize(actorName, initialState, getFsm[F], finalizer[F])
  }

  private def downloadBodies[F[_]: Async: Logger](
    state:                  State[F],
    blockHeadersToDownload: NonEmptyChain[BlockHeader]
  ): F[(State[F], Response[F])] =
    for {
      headerToBody <- Stream.foldable(blockHeadersToDownload).evalMap(downloadBlockBody(state)).compile.toList
      message = RequestsProxy.Message.DownloadBodiesResponse(state.hostId, NonEmptyChain.fromSeq(headerToBody).get)
      _ <- state.requestsProxy.sendNoWait(message)
    } yield (state, state)

  private def downloadBlockBody[F[_]: Async: Logger](
    state: State[F]
  )(blockHeader: BlockHeader): F[(BlockHeader, Either[BlockBodyOrTransactionError, UnverifiedBlockBody])] = {
    val blockId = blockHeader.id

    val body: F[UnverifiedBlockBody] =
      for {
        _                    <- Logger[F].debug(show"Fetching remote body id=$blockId from peer ${state.hostId}")
        (downloadTime, body) <- Async[F].timed(downloadBlockBody(state, blockId))
        _ <- Logger[F].info(show"Fetched body id=$blockId from ${state.hostId} for ${downloadTime.toMillis} ms")
        _ <- checkBody(state, Block(blockHeader, body))
        txAndDownloadTime <- downloadingMissingTransactions(state, body)
        _ <- Logger[F].info(show"Download ${txAndDownloadTime.size} txs from ${state.hostId} for block $blockId")
        allTxDownloadTime = txAndDownloadTime.collect { case (_, Some(time)) => time }
      } yield UnverifiedBlockBody(state.hostId, body, downloadTime.toMillis, allTxDownloadTime)

    body
      .map(blockBody => Either.right[BlockBodyOrTransactionError, UnverifiedBlockBody](blockBody))
      .handleError {
        case e: BlockBodyOrTransactionError => Either.left[BlockBodyOrTransactionError, UnverifiedBlockBody](e)
        case unknownError                   => Either.left(UnknownError(unknownError))
      }
      .flatTap {
        case Right(_) =>
          Logger[F].debug(show"Successfully download block $blockId from peer ${state.hostId}")
        case Left(error) =>
          Logger[F].error(show"Failed download block $blockId from peer ${state.hostId} because of: ${error.toString}")
      }
      .map((blockHeader, _))
  }

  private def downloadBlockBody[F[_]: Async](state: State[F], blockId: BlockId): F[BlockBody] =
    OptionT(state.client.getRemoteBody(blockId)).getOrElseF(MonadThrow[F].raiseError(BodyNotFoundInPeer))

  private def checkBody[F[_]: Async](state: State[F], block: Block): F[BlockBody] =
    EitherT(state.headerToBodyValidation.validate(block))
      .map(_.body)
      .leftMap { case e: IncorrectTxRoot =>
        BodyHaveIncorrectTxRoot(e.headerTxRoot, e.bodyTxRoot)
      }
      .rethrowT

  private def downloadingMissingTransactions[F[_]: Async](
    state:     State[F],
    blockBody: BlockBody
  ): F[List[(TransactionId, Option[Long])]] =
    (Stream
      .iterable[F, TransactionId](blockBody.transactionIds)
      .evalMap(downloadCheckSaveTransaction(state, checkSyntax = true)) ++
      Stream
        .iterable[F, TransactionId](blockBody.rewardTransactionId)
        .evalMap(downloadCheckSaveTransaction(state, checkSyntax = false))).compile.toList

  private def downloadCheckSaveTransaction[F[_]: Async](
    state:       State[F],
    checkSyntax: Boolean
  )(id: TransactionId): F[(TransactionId, Option[Long])] =
    state.transactionStore
      .contains(id)
      .flatMap {
        case true  => (id, Option.empty[Long]).pure[F]
        case false => state.transactionFetcher.downloadCheckSaveTransaction(id, checkSyntax)
      }

  private def startActor[F[_]: Async: Logger](state: State[F]): F[(State[F], Response[F])] =
    Logger[F].info(show"Start body fetcher actor for peer ${state.hostId}") >>
    (state, state).pure[F]

  private def stopActor[F[_]: Async: Logger](state: State[F]): F[(State[F], Response[F])] =
    Logger[F].info(show"Stop body fetcher actor for peer ${state.hostId}") >>
    (state, state).pure[F]

  private def finalizer[F[_]: Async: Logger](state: State[F]): F[Unit] = stopActor(state).void
}
