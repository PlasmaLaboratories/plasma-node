package xyz.stratalab.healthcheck

import cats.effect._
import fs2.Stream
import fs2.concurrent.SignallingRef
import grpc.health.v1.ServingStatus
import xyz.stratalab.algebras._
import xyz.stratalab.models.ServiceStatus

/**
 * HealthCheck
 * Captures the interpreters needed to query the health check service.
 * @param healthChecker
 * @tparam F Effect type
 * @tparam S Health check response container, Ex: Stream, Seq, etc.
 */
case class HealthCheck[F[_], S[_]](
  healthChecker: HealthCheckAlgebra[F, Stream[F, *]]
)

object HealthCheck {

  def make[F[_]: Async](): Resource[F, HealthCheck[F, fs2.Stream[F, *]]] = {
    val checkRef: F[Ref[F, Map[String, ServingStatus]]] =
      // empty string indicates the health of the server in general,
      // rather than any particular gRPC service running on the server
      Ref.of[F, Map[String, ServingStatus]](
        Map(
          ""        -> ServingStatus.SERVING,
          "Node"    -> ServingStatus.SERVING,
          "Indexer" -> ServingStatus.SERVING
        )
      )

    val watchSignal: F[SignallingRef[F, Option[ServiceStatus]]] =
      SignallingRef.of[F, Option[ServiceStatus]](None)

    for {
      ref    <- Resource.liftK(checkRef)
      signal <- Resource.liftK(watchSignal)

      healthChecker <- HealthChecker.make[F](ref, signal)
    } yield HealthCheck(healthChecker)
  }
}
