package org.plasmalabs.transactiongenerator.interpreters

import cats.effect.*
import cats.implicits.*
import io.grpc.Metadata
import org.plasmalabs.indexer.services.{QueryByLockAddressRequest, TransactionServiceFs2Grpc, TxoState}
import org.plasmalabs.sdk.models.box.Box
import org.plasmalabs.transactiongenerator.algebras.WalletInitializer
import org.plasmalabs.transactiongenerator.models.Wallet

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
