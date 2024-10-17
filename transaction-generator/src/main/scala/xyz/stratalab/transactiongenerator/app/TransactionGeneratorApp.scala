package xyz.stratalab.transactiongenerator.app

import cats.Show
import cats.effect._
import cats.effect.std.{Random, SecureRandom}
import cats.implicits._
import com.typesafe.config.Config
import fs2._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import xyz.stratalab.algebras.NodeRpc
import xyz.stratalab.common.application._
import xyz.stratalab.grpc.NodeGrpc
import xyz.stratalab.indexer.services.TransactionServiceFs2Grpc
import xyz.stratalab.sdk.models.TransactionId
import xyz.stratalab.sdk.models.transaction.IoTransaction
import xyz.stratalab.sdk.syntax._
import xyz.stratalab.sdk.validation.algebras.TransactionCostCalculator
import xyz.stratalab.sdk.validation.{TransactionCostCalculatorInterpreter, TransactionCostConfig}
import xyz.stratalab.transactiongenerator.interpreters._
import xyz.stratalab.typeclasses.implicits._

import scala.concurrent.duration._

object TransactionGeneratorApp
    extends IOBaseApp[Args, ApplicationConfig](
      createArgs = args => IO.delay(Args.parserArgs.constructOrThrow(args)),
      createConfig = IOBaseApp.createTypesafeConfig(_),
      parseConfig = (_, conf) => IO.delay(ApplicationConfig.unsafe(conf))
    ) {

  implicit private val logger: Logger[F] =
    Slf4jLogger.getLoggerFromClass[F](this.getClass)

  override def run(args: Args, config: Config, appConfig: ApplicationConfig): IO[Unit] =
    for {
      _               <- Logger[F].info(show"Launching Transaction Generator with appConfig=$appConfig")
      given Random[F] <- SecureRandom.javaSecuritySecureRandom[F]
      // Initialize gRPC Clients
      clientAddress <- parseClientAddress(appConfig)
      _             <- Logger[F].info(show"Initializing client=$clientAddress")
      indexerClientResource = xyz.stratalab.grpc
        .makeChannel[F](clientAddress._1, clientAddress._2, clientAddress._3)
        .flatMap(TransactionServiceFs2Grpc.stubResource[F])
      _      <- Logger[F].info(show"Initializing wallet")
      wallet <- indexerClientResource.flatMap(IndexerWalletInitializer.make[F]).use(_.initialize)
      _      <- Logger[F].info(show"Initialized wallet with spendableBoxes=${wallet.spendableBoxes}")
      // Produce a stream of Transactions from the base wallet
      targetTps = appConfig.transactionGenerator.broadcaster.tps
      _              <- Logger[F].info(show"Generating and broadcasting transactions at tps=$targetTps")
      costCalculator <- TransactionCostCalculatorInterpreter.make[F](TransactionCostConfig()).pure[F]
      metadataF =
        if (appConfig.transactionGenerator.generator.insertMetadata) Fs2TransactionGenerator.randomMetadata[F]
        else Fs2TransactionGenerator.emptyMetadata[F]
      transactionStream <- Fs2TransactionGenerator
        .make[F](wallet, costCalculator, metadataF)
        .flatMap(_.generateTransactions)
      _ <- NodeGrpc.Client
        .make[F](clientAddress._1, clientAddress._2, clientAddress._3)
        .use(client =>
          // Broadcast the transactions and run the background mempool stream
          (
            runBroadcastStream(transactionStream, client, targetTps, costCalculator),
            runMempoolStream(client, appConfig.transactionGenerator.mempool.period)
          ).parTupled
        )
    } yield ()

  /**
   * Parse the RPC addresses from configuration.
   * If the address starts with "https://", TLS will be enabled on the connection.
   * If the address starts with "http://", TLS will be disabled on the connection.
   * If the address starts with neither, TLS will be enabled on the connection.
   */
  private def parseClientAddress(appConfig: ApplicationConfig): F[(String, Int, Boolean)] =
    IO.fromEither {
      val string = appConfig.transactionGenerator.rpc.client
      val (withoutProtocol: String, useTls: Boolean) =
        if (string.startsWith("http://")) (string.drop(7), false)
        else if (string.startsWith("https://")) (string.drop(8), true)
        else (string, true)

      withoutProtocol.split(':').toList match {
        case host :: port :: Nil =>
          port.toIntOption.toRight(new IllegalArgumentException("Invalid RPC port provided")).map((host, _, useTls))
        case _ => Left(new IllegalArgumentException("Invalid RPC config provided"))
      }
    }

  /**
   * Broadcasts each transaction from the input stream
   */
  private def runBroadcastStream(
    transactionStream: Stream[F, IoTransaction],
    client:            NodeRpc[F, Stream[F, *]],
    targetTps:         Double,
    costCalculator:    TransactionCostCalculator
  ) =
    transactionStream
      // Send 1 transaction per _this_ duration
      .meteredStartImmediately((1000000000d / targetTps).nanos)
      // Broadcast+log the transaction
      .evalTap(transaction =>
        Logger[F].debug(show"Broadcasting transaction id=${transaction.id}") >>
        client.broadcastTransaction(transaction) >>
        costCalculator
          .costOf(transaction)
          .pure[F]
          .flatTap(cost => Logger[F].info(show"Broadcasted transaction id=${transaction.id} cost=$cost"))
      )
      .onError { case e =>
        Stream.eval(Logger[F].error(e)("Stream failed"))
      }
      .compile
      .drain

  implicit private val showMempool: Show[Set[TransactionId]] =
    catsStdShowForSet(using showIoTransactionId)

  /**
   * Periodically poll and log the state of the mempool.
   */
  private def runMempoolStream(client: NodeRpc[F, Stream[F, *]], period: FiniteDuration) =
    Stream
      .fixedRateStartImmediately[F](period)
      .evalMap(_ => client.currentMempool())
      .evalTap(transactionIds => Logger[F].info(show"Current mempool=$transactionIds"))
      .compile
      .drain

}
