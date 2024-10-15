package xyz.stratalab.blockchain.interpreters

import cats.effect.{Async, Resource, Sync}
import cats.implicits._
import io.grpc.{Metadata, ServerServiceDefinition}
import xyz.stratalab.models.{ProposalId, VersionId}
import xyz.stratalab.node.services._

/**
 * Serves the RPC(s) needed to operate the node in "regtest" mode
 */
object RegtestRpcServer {

  def service[F[_]: Async](
    instructMakeBlock:   F[Unit],
    updateVotedVersion:  VersionId => F[Unit],
    updateVotedProposal: ProposalId => F[Unit]
  ): Resource[F, ServerServiceDefinition] =
    RegtestRpcFs2Grpc.bindServiceResource(
      new RegtestRpcFs2Grpc[F, Metadata] {

        override def makeBlocks(request: MakeBlocksReq, ctx: Metadata): F[MakeBlocksRes] =
          Sync[F].defer(instructMakeBlock.replicateA(request.quantity)).as(MakeBlocksRes())

        override def setVoting(request: SetVotingReq, ctx: Metadata): F[SetVotingRes] =
          updateVotedVersion(request.versionVoting) >>
          updateVotedProposal(request.proposalVoting).as(SetVotingRes())
      }
    )

}
