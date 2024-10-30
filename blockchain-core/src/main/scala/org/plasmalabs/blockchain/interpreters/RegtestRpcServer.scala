package org.plasmalabs.blockchain.interpreters

import cats.effect.{Async, Resource, Sync}
import cats.implicits._
import io.grpc.{Metadata, ServerServiceDefinition}
import org.plasmalabs.models.{ProposalId, VersionId}
import org.plasmalabs.node.services._
import org.typelevel.log4cats.Logger

/**
 * Serves the RPC(s) needed to operate the node in "regtest" mode
 */
object RegtestRpcServer {

  def service[F[_]: Async: Logger](
    instructMakeBlock:   F[Unit],
    updateVotedVersion:  VersionId => F[Unit],
    updateVotedProposal: ProposalId => F[Unit]
  ): Resource[F, ServerServiceDefinition] =
    RegtestRpcFs2Grpc.bindServiceResource(
      new RegtestRpcFs2Grpc[F, Metadata] {

        override def makeBlocks(request: MakeBlocksReq, ctx: Metadata): F[MakeBlocksRes] =
          Sync[F].defer(instructMakeBlock.replicateA(request.quantity)).as(MakeBlocksRes())

        override def setVoting(request: SetVotingReq, ctx: Metadata): F[SetVotingRes] =
          Logger[F].info(s"Set version voting as ${request.versionVoting}") >>
          Logger[F].info(s"Set proposal voting as ${request.proposalVoting}") >>
          updateVotedVersion(request.versionVoting) >>
          updateVotedProposal(request.proposalVoting).as(SetVotingRes())
      }
    )

}
