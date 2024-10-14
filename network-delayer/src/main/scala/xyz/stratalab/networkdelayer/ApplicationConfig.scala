package xyz.stratalab.networkdelayer

import cats.Show
import com.typesafe.config.Config
import pureconfig.generic.derivation.default._
import pureconfig.{ConfigSource, _}

import scala.concurrent.duration.FiniteDuration

case class ApplicationConfig(
  routes: List[ApplicationConfig.Route]
) derives ConfigReader

object ApplicationConfig {

  case class Route(
    bindHost:        String,
    bindPort:        Int,
    destinationHost: String,
    destinationPort: Int,
    throttle:        Option[Route.Throttle]
  ) derives ConfigReader

  object Route {

    case class Throttle(latency: FiniteDuration, downloadBytesPerSecond: Long, uploadBytesPerSecond: Long)
        derives ConfigReader
  }

  def unsafe(config: Config): ApplicationConfig =
    ConfigSource.fromConfig(config).loadOrThrow[ApplicationConfig]

  implicit val showApplicationConfig: Show[ApplicationConfig] =
    Show.fromToString
}
