package org.plasmalabs.consensus.interpreters

import cats.effect.IO
import cats.implicits._
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.plasmalabs.algebras.testInterpreters.TestStore
import org.plasmalabs.models.{Epoch, _}
import org.plasmalabs.sdk.generators.TransactionGenerator

class VersionInfoSpec
    extends CatsEffectSuite
    with ScalaCheckEffectSuite
    with AsyncMockFactory
    with TransactionGenerator {
  type F[A] = IO[A]
  implicit val logger: Logger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)

  test("Add, remove, get epoch version") {
    withMock {
      for {
        storage   <- TestStore.make[F, Epoch, VersionId]
        _         <- Seq(0 -> 1, 10 -> 2).traverse { case (key, value) => storage.put(key, value) }
        underTest <- VersionInfo.make(storage)

        epoch0Version  <- underTest.getVersionForEpoch(0)
        _              <- assert(epoch0Version == 1).pure[F]
        epoch5Version  <- underTest.getVersionForEpoch(5)
        _              <- assert(epoch5Version == 1).pure[F]
        epoch10Version <- underTest.getVersionForEpoch(10)
        _              <- assert(epoch10Version == 2).pure[F]
        epoch20Version <- underTest.getVersionForEpoch(11)
        _              <- assert(epoch20Version == 2).pure[F]

        _               <- underTest.removeVersionStartEpoch(10)
        epoch10Version2 <- underTest.getVersionForEpoch(10)
        _               <- assert(epoch10Version2 == 1).pure[F]

        _               <- underTest.addVersionStartEpoch(10, 3)
        epoch10Version3 <- underTest.getVersionForEpoch(10)
        _               <- assert(epoch10Version3 == 3).pure[F]
        epoch20Version3 <- underTest.getVersionForEpoch(11)
        _               <- assert(epoch20Version3 == 3).pure[F]

        storageData <- storage.getAll()
        _           <- assert(storageData == Seq(0 -> 1, 10 -> 3)).pure[F]
      } yield ()

    }
  }
}
