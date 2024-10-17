package org.plasmalabs.consensus.interpreters

import cats.effect.Async
import cats.implicits._
import org.plasmalabs.algebras._
import org.plasmalabs.consensus.algebras.VersionInfoAlgebra
import org.plasmalabs.models._
import org.typelevel.log4cats.Logger

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
