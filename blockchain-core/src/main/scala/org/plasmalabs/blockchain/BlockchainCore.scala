package org.plasmalabs.blockchain

import fs2.Stream
import org.plasmalabs.algebras.{ClockAlgebra, ProtocolConfigurationAlgebra}
import org.plasmalabs.blockchain.algebras.EpochDataAlgebra
import org.plasmalabs.consensus.Consensus
import org.plasmalabs.consensus.models.BlockId
import org.plasmalabs.eventtree.ParentChildTree
import org.plasmalabs.ledger.Ledger

trait BlockchainCore[F[_]] {
  def clock: ClockAlgebra[F]
  def dataStores: DataStores[F]
  def cryptoResources: CryptoResources[F]
  def blockIdTree: ParentChildTree[F, BlockId]
  def consensus: Consensus[F]
  def ledger: Ledger[F]
  def validators: Validators[F]
  def epochData: EpochDataAlgebra[F]
  def protocolConfiguration: ProtocolConfigurationAlgebra[F, Stream[F, *]]
}

case class BlockchainCoreImpl[F[_]](
  clock:                 ClockAlgebra[F],
  dataStores:            DataStores[F],
  cryptoResources:       CryptoResources[F],
  blockIdTree:           ParentChildTree[F, BlockId],
  consensus:             Consensus[F],
  ledger:                Ledger[F],
  validators:            Validators[F],
  epochData:             EpochDataAlgebra[F],
  protocolConfiguration: ProtocolConfigurationAlgebra[F, Stream[F, *]]
) extends BlockchainCore[F]
