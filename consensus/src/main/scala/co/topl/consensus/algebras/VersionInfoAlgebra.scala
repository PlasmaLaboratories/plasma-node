package co.topl.consensus.algebras

import co.topl.models._

trait VersionInfoAlgebra[F[_]] {
  def addVersionStartEpoch(epoch:    Epoch, version: VersionId): F[Unit]
  def removeVersionStartEpoch(epoch: Epoch): F[Unit]
  def getVersionForEpoch(epoch:      Epoch): F[VersionId]
}
