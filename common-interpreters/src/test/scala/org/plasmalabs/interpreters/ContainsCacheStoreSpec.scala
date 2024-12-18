package org.plasmalabs.interpreters

import cats.effect.{IO, Resource}
import cats.implicits.*
import fs2.io.file.{Files, Path}
import munit.CatsEffectSuite
import org.plasmalabs.algebras.Store
import org.plasmalabs.algebras.testInterpreters.NoOpLogger
import org.plasmalabs.db.leveldb.{LevelDbStore, SpecKey, SpecValue}
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.Logger

class ContainsCacheStoreSpec extends CatsEffectSuite with AsyncMockFactory {
  type F[A] = IO[A]

  test("Read, store, and delete cache values") {
    withMock {
      inSequence {
        for {
          underlying <- mock[Store[F, Long, String]].pure[F]
          underTest  <- ContainsCacheStore.make[F, Long, String](underlying.pure[F], 5)

          // check using underlying cache
          _ = (underlying.get).expects(5L).once().returning(none[String].pure[F])
          _ <- underTest.get(5L).assertEquals(None)

          // put information shall be cached
          _ = (underlying.put).expects(6L, "Test").once().returning(().pure[F])
          _ = (underlying.contains).expects(6L).once().returning(true.pure[F])
          _ <- underTest.put(6L, "Test")
          _ <- underTest.contains(6L).assertEquals(true)
          _ <- underTest.contains(6L).assertEquals(true)

          // remove information shall be cached
          _ = (underlying.remove).expects(6L).once().returning(().pure[F])
          _ = (underlying.contains).expects(6L).once().returning(false.pure[F])
          _ <- underTest.remove(6L)
          _ <- underTest.contains(6L).assertEquals(false)
          _ <- underTest.contains(6L).assertEquals(false)

          // contains information shall be cached
          _ = (underlying.contains).expects(7L).once().returning(true.pure[F])
          _ <- underTest.contains(7L).assertEquals(true)
          _ <- underTest.contains(7L).assertEquals(true)
        } yield ()
      }
    }
  }

  implicit val logger: Logger[F] = new NoOpLogger[F]

  ResourceFunFixture[Path](Resource.make(Files[F].createTempDirectory)(Files[F].deleteRecursively))
    .test("Check multi thread") { testPath =>
      def parWriting(store: Store[F, SpecKey, SpecValue], key: SpecKey, thr: Int): F[Unit] =
        for {
          values <- (1 to thr).toList.map(t => SpecValue(key.id, t)).pure[F]
          _ <- values.map { value =>
            store
              .contains(key)
              .ifM(
                ifTrue = store.get(key),
                ifFalse = store.put(key, value)
              )
          }.parUnorderedSequence
        } yield ()

      LevelDbStore.makeFactory[F]().flatMap(LevelDbStore.makeDb[F](testPath, _)).use { dbUnderTest =>
        for {
          leveldb        <- LevelDbStore.make[F, SpecKey, SpecValue](dbUnderTest)
          cacheStore     <- CacheStore.make[F, SpecKey, SpecKey, SpecValue](leveldb.pure[F], identity)
          threadsN       <- 10.pure[F]
          elements       <- 100000.pure[F]
          containsCached <- ContainsCacheStore.make[F, SpecKey, SpecValue](cacheStore.pure[F], elements)
          keys           <- (1 to elements).map(id => SpecKey(id.toString)).toList.pure[F]
          _              <- keys.map(key => parWriting(containsCached, key, threadsN)).parUnorderedSequence
          _              <- keys.map(key => containsCached.get(key).map(_.isDefined).assert).parSequence
        } yield ()
      }

    }
}
