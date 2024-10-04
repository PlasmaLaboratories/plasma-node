package xyz.stratalab.blockchain

import fs2.Stream
import xyz.stratalab.algebras.{ClockAlgebra, ProtocolConfigurationAlgebra}
import xyz.stratalab.blockchain.algebras.EpochDataAlgebra
import xyz.stratalab.consensus.Consensus
import xyz.stratalab.consensus.models.BlockId
import xyz.stratalab.eventtree.ParentChildTree
import xyz.stratalab.ledger.Ledger

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
