package xyz.stratalab.networking.fsnetwork

import cats.MonadThrow
import cats.data.EitherT
import cats.effect.Async
import cats.implicits._
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import xyz.stratalab.sdk.syntax.ioTransactionAsTransactionSyntaxOps
import xyz.stratalab.sdk.validation.algebras.TransactionSyntaxVerifier
import org.typelevel.log4cats.Logger
import xyz.stratalab.algebras.Store
import xyz.stratalab.models.p2p._
import xyz.stratalab.networking.blockchain.BlockchainPeerClient
import xyz.stratalab.networking.fsnetwork.BlockDownloadError.BlockBodyOrTransactionError._
import xyz.stratalab.networking.fsnetwork.P2PShowInstances._
import xyz.stratalab.typeclasses.implicits._

class TransactionFetcher[F[_]: Async: Logger](
  hostId:                      HostId,
  transactionSyntaxValidation: TransactionSyntaxVerifier[F],
  transactionStore:            Store[F, TransactionId, IoTransaction],
  client:                      BlockchainPeerClient[F]
) {

  def downloadCheckSaveTransaction(
    transactionId:  TransactionId,
    runSyntaxCheck: Boolean
  ): F[(TransactionId, Option[Long])] =
    for {
      _ <- Logger[F].debug(show"Fetching transaction id=$transactionId from peer $hostId")
      (downloadTime, downloadedTransaction) <- Async[F].timed(downloadTransaction(transactionId))
      _ <- checkTransaction(transactionId, downloadedTransaction, transactionSyntaxValidation, runSyntaxCheck)
      _ <- Logger[F].debug(show"Saving transaction id=$transactionId")
      _ <- transactionStore.put(transactionId, downloadedTransaction)
    } yield (transactionId, Option(downloadTime.toMillis))

  private def checkTransaction(
    transactionId:               TransactionId,
    downloadedTransaction:       IoTransaction,
    transactionSyntaxValidation: TransactionSyntaxVerifier[F],
    runSyntaxCheck:              Boolean
  ): F[IoTransaction] = {
    val downloadedTransactionId = downloadedTransaction.id
    if (downloadedTransactionId =!= transactionId) {
      MonadThrow[F].raiseError(TransactionHaveIncorrectId(transactionId, downloadedTransactionId))
    } else {
      if (runSyntaxCheck) {
        EitherT(transactionSyntaxValidation.validate(downloadedTransaction))
          .leftMap(errors => TransactionHaveIncorrectSyntax(transactionId, errors))
          .rethrowT
      } else {
        downloadedTransaction.pure[F]
      }
    }
  }

  private def downloadTransaction(
    transactionId: TransactionId
  ): F[IoTransaction] =
    client.getRemoteTransactionOrError(transactionId, TransactionNotFoundInPeer(transactionId)).map(_.embedId)

}
