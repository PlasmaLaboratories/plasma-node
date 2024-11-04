package org.plasmalabs.algebras

import org.plasmalabs.models._

trait NodeRegTestRpc[F[_], S[_]] {
  def setVoting(votingVersion: VersionId, votingProposal: ProposalId): F[Unit]
}
