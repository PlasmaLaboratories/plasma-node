package xyz.stratalab.transactiongenerator.interpreters

import cats.effect._
import cats.implicits._
import io.grpc.Metadata
import xyz.stratalab.indexer.services.{QueryByLockAddressRequest, TransactionServiceFs2Grpc, TxoState}
import xyz.stratalab.sdk.models.box.Box
import xyz.stratalab.transactiongenerator.algebras.WalletInitializer
import xyz.stratalab.transactiongenerator.models.Wallet

object IndexerWalletInitializer {

  def make[F[_]: Async](indexerRpc: TransactionServiceFs2Grpc[F, Metadata]): Resource[F, WalletInitializer[F]] =
    Resource.pure[F, WalletInitializer[F]](
      new WalletInitializer[F] {

        def initialize: F[Wallet] =
          emptyWallet.propositions.keys.toList.foldLeftM(emptyWallet)((wallet, address) =>
            indexerRpc
              .getTxosByLockAddress(
                QueryByLockAddressRequest(address, None, TxoState.UNSPENT),
                new Metadata()
              )
              .map(_.txos.filter(_.transactionOutput.value.value.isLvl).foldLeft(wallet) { (wallet, txo) =>
                val newBoxes =
                  wallet.propositions
                    .get(txo.transactionOutput.address)
                    .map(lock =>
                      (
                        txo.outputAddress,
                        Box(lock, txo.transactionOutput.value)
                      )
                    )
                wallet.copy(spendableBoxes = wallet.spendableBoxes ++ newBoxes)
              })
          )

      }
    )

}
