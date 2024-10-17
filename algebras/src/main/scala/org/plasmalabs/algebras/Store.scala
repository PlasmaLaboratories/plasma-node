package org.plasmalabs.algebras

import cats.data.OptionT
import cats.implicits._
import cats.{Functor, MonadThrow, Show}

trait StoreReader[F[_], Key, T] {
  outer =>
  def get(id: Key): F[Option[T]]
  def getAll(): F[Seq[(Key, T)]]

  def contains(id: Key): F[Boolean]

  def getOrRaise(id: Key)(implicit monadThrow: MonadThrow[F], showKey: Show[Key]): F[T] =
    OptionT(get(id)).getOrElseF(monadThrow.raiseError(new NoSuchElementException(show"Element not found. id=$id")))

  def mapRead[KU, TU](fKey: KU => Key, rKey: Key => KU, rValue: T => TU)(implicit
    functor: Functor[F]
  ): StoreReader[F, KU, TU] =
    new StoreReader[F, KU, TU] {
      def get(id: KU): F[Option[TU]] = OptionT(outer.get(fKey(id))).map(rValue).value

      def contains(id: KU): F[Boolean] = outer.contains(fKey(id))

      def getAll(): F[Seq[(KU, TU)]] = outer.getAll().map(_.map { case (key, value) => (rKey(key), rValue(value)) })
    }
}

trait StoreWriter[F[_], Key, T] {
  def put(id:    Key, t: T): F[Unit]
  def remove(id: Key): F[Unit]
}

trait Store[F[_], Key, T] extends StoreReader[F, Key, T] with StoreWriter[F, Key, T]
