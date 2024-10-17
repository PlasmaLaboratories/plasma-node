package org.plasmalabs.blockchain.interpreters

import cats.Show
import cats.effect.{Async, Resource}
import cats.implicits._
import fs2.concurrent.Topic
import io.grpc.{Metadata, ServerServiceDefinition}
import org.plasmalabs.models.p2p._
import org.plasmalabs.models.utility.{NetworkCommands, byteStringToByteVector}
import org.plasmalabs.node.services._
import org.typelevel.log4cats.Logger

object NetworkControlRpcServer {
  implicit val showHostId: Show[HostId] = id => show"${id.id.toBase58}"

  def service[F[_]: Async: Logger](
    thisHostId:         HostId,
    networkCommandsOpt: Option[Topic[F, NetworkCommands]]
  ): Resource[F, ServerServiceDefinition] =
    NetworkControlRpcFs2Grpc.bindServiceResource(
      new NetworkControlRpcFs2Grpc[F, Metadata] {

        override def getHostId(request: GetHostIdReq, ctx: Metadata): F[GetHostIdRes] =
          Logger[F].info(show"Network control: Requested current host id") >>
          GetHostIdRes(RpcHostId(thisHostId.id)).pure[F]

        override def forgetPeer(request: ForgetPeerReq, ctx: Metadata): F[ForgetPeerRes] = {
          val command = for {
            topic <- networkCommandsOpt
            hostIdToCold = HostId(request.id.id)
          } yield Logger[F].info(show"Network control: Requested to forget remote peer $hostIdToCold") >>
          topic.publish1(NetworkCommands.ForgetPeer(hostIdToCold)).as(ForgetPeerRes())

          command.getOrElse(ForgetPeerRes().pure[F])
        }

        override def addPeer(request: AddPeerReq, ctx: Metadata): F[AddPeerRes] = {
          val command = for {
            topic <- networkCommandsOpt
            hostIdToAdd = request.id.map(rpcHostId => HostId(rpcHostId.id))
            remoteAddress = RemoteAddress(request.ip, request.port)
          } yield Logger[F].info(show"Network control: Requested to add remote peer $remoteAddress as known") >>
          topic.publish1(NetworkCommands.AddPeer(remoteAddress, hostIdToAdd)).as(AddPeerRes())

          command.getOrElse(AddPeerRes().pure[F])
        }
      }
    )
}
