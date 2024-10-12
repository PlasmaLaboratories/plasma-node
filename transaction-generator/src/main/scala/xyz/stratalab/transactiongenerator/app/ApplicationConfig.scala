package xyz.stratalab.transactiongenerator.app

import cats.Show
import com.typesafe.config.Config
import pureconfig.ConfigSource
import pureconfig._
import pureconfig.generic.derivation.default._

import scala.concurrent.duration.FiniteDuration

case class ApplicationConfig(
  transactionGenerator: ApplicationConfig.TransactionGenerator
) derives ConfigReader

object ApplicationConfig {

  case class TransactionGenerator(
    rpc:         TransactionGenerator.Rpc,
    generator:   TransactionGenerator.Generator,
    broadcaster: TransactionGenerator.Broadcaster,
    mempool:     TransactionGenerator.Mempool
  ) derives ConfigReader

  object TransactionGenerator {

    case class Rpc(client: String) derives ConfigReader

    case class Generator(insertMetadata: Boolean) derives ConfigReader

    case class Broadcaster(tps: Double) derives ConfigReader

    case class Mempool(period: FiniteDuration) derives ConfigReader
  }

  def unsafe(config: Config): ApplicationConfig =
    ConfigSource.fromConfig(config).loadOrThrow[ApplicationConfig]

  implicit val showApplicationConfig: Show[ApplicationConfig] =
    Show.fromToString
}
