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
      Option.when(protocolVersion.votedProposalId != emptyProposalId)(protocolVersion.votedProposalId)

    def getVersionVote: Option[VersionId] =
      Option.when(protocolVersion.votedVersionId != emptyVersionId)(protocolVersion.votedVersionId)

    def versionId: VersionId = protocolVersion.versionId
  }

}
