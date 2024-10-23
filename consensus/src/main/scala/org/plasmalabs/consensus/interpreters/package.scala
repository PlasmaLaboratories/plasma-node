package org.plasmalabs.consensus

import org.plasmalabs.consensus.models.{BlockHeader, ProtocolVersion}
import org.plasmalabs.models._

package object interpreters {

  implicit class BlockHeaderVersionsOps(header: BlockHeader) {

    def getProposalVote: Option[ProposalId] = header.version.getProposalVote

    def getVersionVote: Option[VersionId] = header.version.getVersionVote

    def versionId: VersionId = header.version.versionId
  }

  implicit class ProtocolVersionOps(protocolVersion: ProtocolVersion) {

    def getProposalVote: Option[ProposalId] =
      Option.when(protocolVersion.thirdDigit != emptyProposal)(protocolVersion.thirdDigit)

    def setProposalVote(proposalId: ProposalId): ProtocolVersion = protocolVersion.copy(thirdDigit = proposalId)

    def getVersionVote: Option[VersionId] =
      Option.when(protocolVersion.secondDigit != emptyVersion)(protocolVersion.secondDigit)

    def setVersionVote(versionVote: VersionId): ProtocolVersion = protocolVersion.copy(secondDigit = versionVote)

    def setVersionId(version: VersionId): ProtocolVersion = protocolVersion.copy(firstDigit = version)
    def versionId: VersionId = protocolVersion.firstDigit
  }

}
