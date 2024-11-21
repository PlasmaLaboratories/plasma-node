package org.plasmalabs.algebras

import org.plasmalabs.models.*

trait NodeRegTestRpc[F[_], S[_]] {
  def setVoting(votingVersion: VersionId, votingProposal: ProposalId): F[Unit]
}
