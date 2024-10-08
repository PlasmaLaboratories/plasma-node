package xyz.stratalab.consensus

import cats.data.Chain
import cats.implicits._
import cats.{Defer, Monad}
import xyz.stratalab.algebras.SecureStore
import xyz.stratalab.codecs.bytes.typeclasses.Persistable
import xyz.stratalab.codecs.bytes.typeclasses.implicits._
import xyz.stratalab.models.Bytes

/**
 * A simple, in-memory, non-thread-safe implementation of a SecureStore
 *
 * (For testing purposes only)
 */
class EphemeralSecureStore[F[_]: Monad: Defer] extends SecureStore[F] {

  private var entries: Map[String, Bytes] = Map.empty

  def list: F[Chain[String]] =
    Defer[F].defer(Chain.fromSeq(entries.keys.toSeq).pure[F])

  def erase(name: String): F[Unit] =
    Defer[F].defer(
      entries
        .get(name)
        .foreach { _ =>
          entries -= name
        }
        .pure[F]
    )

  def write[A: Persistable](name: String, data: A): F[Unit] =
    Defer[F].defer((entries += (name -> data.persistedBytes)).pure[F])

  def consume[A: Persistable](name: String): F[Option[A]] =
    Defer[F].defer {
      {
        val entry = entries.get(name).flatMap(b => Persistable[A].fromPersistedBytes(b).toOption)
        entries -= name
        entry
      }.pure[F]
    }
}