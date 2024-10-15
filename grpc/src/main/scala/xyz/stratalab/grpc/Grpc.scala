package xyz.stratalab.grpc

import cats.effect.kernel.{Async, Resource}
import fs2.grpc.syntax.all._
import io.grpc.netty.shaded.io.grpc.netty.NettyServerBuilder
import io.grpc.protobuf.services.ProtoReflectionService
import io.grpc.{Server, ServerServiceDefinition}

import java.net.InetSocketAddress

object Grpc {

  object Server {

    /**
     * Serves the given gRPC Services
     * @param host The host to bind
     * @param port The port to bind
     * @param services The gRPC services to launch
     */
    def serve[F[_]: Async](host: String, port: Int)(services: List[ServerServiceDefinition]): Resource[F, Server] =
      services
        .foldLeft(
          NettyServerBuilder
            .forAddress(new InetSocketAddress(host, port))
        )(_.addService(_))
        .addService(ProtoReflectionService.newInstance())
        .resource[F]
        .evalMap(server => Async[F].delay(server.start()))
  }
}
