package xyz.stratalab.networking.blockchain

import fs2._
import xyz.stratalab.consensus.models.{BlockHeader, BlockId, SlotData}
import xyz.stratalab.node.models._
import xyz.stratalab.sdk.models.TransactionId
import xyz.stratalab.sdk.models.transaction.IoTransaction

/**
 * Serves local blockchain data to a remote peer
 */
trait BlockchainPeerServerAlgebra[F[_]] {
  def peerAsServer: F[Option[KnownHost]]
  def localBlockAdoptions: F[Stream[F, BlockId]]
  def localTransactionNotifications: F[Stream[F, TransactionId]]
  def getLocalSlotData(id:              BlockId): F[Option[SlotData]]
  def requestSlotDataAndParents(from:   BlockId, to: BlockId): F[Option[List[SlotData]]]
  def getLocalHeader(id:                BlockId): F[Option[BlockHeader]]
  def getLocalBody(id:                  BlockId): F[Option[BlockBody]]
  def getLocalTransaction(id:           TransactionId): F[Option[IoTransaction]]
  def getLocalBlockAtHeight(height:     Long): F[Option[BlockId]]
  def getLocalBlockAtDepth(depth:       Long): F[Option[BlockId]]
  def getKnownHosts(req:                CurrentKnownHostsReq): F[Option[CurrentKnownHostsRes]]
  def getPong(req:                      PingMessage): F[Option[PongMessage]]
  def notifyApplicationLevel(isEnabled: Boolean): F[Option[Unit]]
}
