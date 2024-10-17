package org.plasmalabs.grpc.services

import cats.effect.Async
import fs2.Stream
import grpc.health.v1.{HealthCheckRequest, HealthCheckResponse, HealthFs2Grpc}
import io.grpc.Metadata
import org.plasmalabs.algebras.HealthCheckAlgebra
import org.plasmalabs.grpc.FApplicativeErrorAdapter

class HealthCheckService[F[_]: Async](healthCheck: HealthCheckAlgebra[F, Stream[F, *]])
    extends HealthFs2Grpc[F, Metadata] {

  def check(request: HealthCheckRequest, ctx: Metadata): F[HealthCheckResponse] =
    healthCheck
      .check(request)
      .adaptErrorsToGrpc

  def watch(request: HealthCheckRequest, ctx: Metadata): Stream[F, HealthCheckResponse] =
    Stream
      .force(healthCheck.watch(request))
      .adaptErrorsToGrpc
}
