package org.plasmalabs.algebras.testInterpreters

import cats.effect.kernel.{Async, Ref}
import cats.implicits.*
import org.plasmalabs.algebras.Store

object TestStore {

  def make[F[_]: Async, Key, T]: F[TestStore[F, Key, T]] =
    Ref.of[F, Map[Key, T]](Map.empty[Key, T]).map(new TestStore[F, Key, T](_))

}

class TestStore[F[_]: Async, Key, T] private (ref: Ref[F, Map[Key, T]]) extends Store[F, Key, T] {

  def get(id: Key): F[Option[T]] =
    ref.get.map(_.get(id))

  def put(id: Key, t: T): F[Unit] =
    ref.update(_.updated(id, t))

  def remove(id: Key): F[Unit] =
    ref.update(_ - id)

  def contains(id: Key): F[Boolean] =
    ref.get.map(_.contains(id))

  def copyData: F[Map[Key, T]] = ref.get.map(_.toMap)

  def getAll(): F[Seq[(Key, T)]] = ref.get.map(_.toSeq)
}
