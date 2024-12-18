package org.plasmalabs.interpreters

import cats.effect.kernel.Sync
import cats.implicits.*
import com.github.benmanes.caffeine.cache.Caffeine
import org.plasmalabs.algebras.Store
import scalacache.Entry
import scalacache.caffeine.CaffeineCache

object ContainsCacheStore {

  /**
   * Wrap existing store with added cache for "contains" operation
   * @tparam F parameter type
   * @tparam Key the underlying Store Key type
   * @tparam Value the underlying Store Value type
   * @return a new Store which wraps the underlying Store with Caffeine Cache for contains operations
   */
  def make[F[_]: Sync, Key, Value](
    underlying:        F[Store[F, Key, Value]],
    containsCacheSize: Long
  ): F[Store[F, Key, Value]] =
    underlying.flatMap(underlying =>
      Sync[F]
        .delay[CaffeineCache[F, Key, Boolean]](
          CaffeineCache[F, Key, Boolean](
            Caffeine.newBuilder.maximumSize(containsCacheSize).build[Key, Entry[Boolean]]
          )
        )
        .map(containsCache =>
          new Store[F, Key, Value] {

            def put(id: Key, t: Value): F[Unit] =
              underlying.put(id, t) >> underlying.contains(id).flatMap(containsCache.put(id)(_))

            def remove(id: Key): F[Unit] =
              underlying.remove(id) >> underlying.contains(id).flatMap(containsCache.put(id)(_))

            def get(id: Key): F[Option[Value]] = underlying.get(id)

            def contains(id: Key): F[Boolean] =
              containsCache.cachingF(id)(ttl = None)(Sync[F].defer(underlying.contains(id)))

            def getAll(): F[Seq[(Key, Value)]] = underlying.getAll()
          }
        )
    )

  implicit class ContainsCacheStoreSyntax[F[_]: Sync, Key, Value](store: Store[F, Key, Value]) {
    def withCachedContains(containsCacheSize: Long): F[Store[F, Key, Value]] = make(store.pure[F], containsCacheSize)
  }
}
