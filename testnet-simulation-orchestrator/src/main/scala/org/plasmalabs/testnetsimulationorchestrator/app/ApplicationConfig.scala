package org.plasmalabs.testnetsimulationorchestrator.app

import cats.Show
import com.typesafe.config.Config
import pureconfig.generic.derivation.default._
import pureconfig.{ConfigSource, _}

import scala.concurrent.duration.FiniteDuration

case class ApplicationConfig(
  simulationOrchestrator: ApplicationConfig.SimulationOrchestrator
) derives ConfigReader

object ApplicationConfig {

  case class SimulationOrchestrator(
    kubernetes: SimulationOrchestrator.Kubernetes,
    scenario:   SimulationOrchestrator.Scenario,
    nodes:      List[SimulationOrchestrator.Node],
    publish:    SimulationOrchestrator.Publish
  ) derives ConfigReader

  object SimulationOrchestrator {

    case class Kubernetes(namespace: String) derives ConfigReader

    case class Scenario(targetHeight: Long, transactionsPerSecond: Double, timeout: FiniteDuration) derives ConfigReader

    case class Node(name: String, host: String, port: Int) derives ConfigReader

    case class Publish(bucket: String, filePrefix: String) derives ConfigReader
  }

  def unsafe(config: Config): ApplicationConfig =
    ConfigSource.fromConfig(config).loadOrThrow[ApplicationConfig]

  implicit val showApplicationConfig: Show[ApplicationConfig] =
    Show.fromToString
}
