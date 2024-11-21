package org.plasmalabs.grpc

import cats.effect.kernel.{Async, Resource}
import cats.implicits.*
import fs2.Stream
import grpc.health.v1.*
import io.grpc.ServerServiceDefinition
import org.plasmalabs.algebras.HealthCheckAlgebra
import org.plasmalabs.grpc.services.HealthCheckService

object HealthCheckGrpc {

  object Server {

    def services[F[_]: Async](
      healthCheck: HealthCheckAlgebra[F, Stream[F, *]]
    ): Resource[F, List[ServerServiceDefinition]] =
      List(HealthFs2Grpc.bindServiceResource(new HealthCheckService(healthCheck))).sequence
  }
}
