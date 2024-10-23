package org.plasmalabs.blockchain.algebras

import org.plasmalabs.consensus.models.BlockId
import org.plasmalabs.models.Bytes
import org.plasmalabs.sdk.models.TransactionId

trait NodeMetadataAlgebra[F[_]] {

  def readAppVersion: F[Option[String]]
  def setAppVersion(version: String): F[Unit]

  def readInitTime: F[Option[Long]]
  def setInitTime(timestamp: Long): F[Unit]

  def readStakingRegistrationTransactionId: F[Option[TransactionId]]
  def setStakingRegistrationTransactionId(id: TransactionId): F[Unit]

  def readStakingRegistrationBlockId: F[Option[BlockId]]
  def setStakingRegistrationBlockId(id: BlockId): F[Unit]

  def readP2PSK: F[Option[Bytes]]
  def setP2PSK(id: Bytes): F[Unit]

}
