package xyz.stratalab.node

import cats.effect.{IO, Resource}
import cats.implicits._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import xyz.stratalab.config.ApplicationConfig
import xyz.stratalab.grpc.{HealthCheckGrpc, NodeGrpc}
import xyz.stratalab.healthcheck.HealthCheck
import xyz.stratalab.node.ApplicationConfigOps._

class IdleApp(appConfig: ApplicationConfig) {

  type F[+A] = IO[A]

  implicit private val logger: Logger[F] =
    Slf4jLogger.getLoggerFromName[F]("Node.Idle")

  def run: F[Unit] =
    (
      for {
        _ <- Resource.make(Logger[F].info("Launching node in idle operation mode"))(_ => Logger[F].info("Done"))
        _ <- Logger[F].info(show"Node configuration=$appConfig").toResource
        _ <- Resource.make(Logger[F].info("Launching healthcheck"))(_ => Logger[F].info("Healthcheck terminated"))
        healthCheck    <- HealthCheck.make[F]()
        healthServices <- HealthCheckGrpc.Server.services(healthCheck.healthChecker)
        _ <- NodeGrpc.Server.serve[F](appConfig.node.rpc.bindHost, appConfig.node.rpc.bindPort)(healthServices)
        _ <- Logger[F].info(show"Waiting forever...").toResource
        _ <- Resource.never[F, Unit]
      } yield ()
    ).use_

}
