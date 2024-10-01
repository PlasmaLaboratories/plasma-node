package xyz.stratalab.consensus.interpreters

import cats.effect.Async
import cats.implicits._
import org.typelevel.log4cats.Logger
import xyz.stratalab.algebras._
import xyz.stratalab.consensus.algebras.VersionInfoAlgebra
import xyz.stratalab.models._

import scala.collection.mutable

object VersionInfo {

  def make[F[_]: Async: Logger](epochToActiveVersionStorage: Store[F, Epoch, VersionId]): F[VersionInfoAlgebra[F]] =
    for {
      cache <- epochToActiveVersionStorage.getAll().map(mutable.TreeMap.from[Epoch, VersionId])
      versionInfo = new VersionInfoAlgebra[F] {

        override def addVersionStartEpoch(epoch: Epoch, version: VersionId): F[Unit] =
          epochToActiveVersionStorage.put(epoch, version) >>
          cache.put(epoch, version).pure[F].void

        override def removeVersionStartEpoch(epoch: Epoch): F[Unit] =
          epochToActiveVersionStorage.remove(epoch) >>
          cache.remove(epoch).pure[F].void

        override def getVersionForEpoch(epoch: Epoch): F[VersionId] =
          cache.getOrElse(epoch, cache.maxBefore(epoch).get._2).pure[F]
      }
    } yield versionInfo
}
