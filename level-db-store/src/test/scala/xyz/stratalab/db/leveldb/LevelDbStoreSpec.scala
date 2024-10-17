package org.plasmalabs.db.leveldb

import cats.effect.{IO, Resource}
import cats.implicits._
import fs2.io.file.{Files, Path}
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.typelevel.log4cats.Logger
import scodec.{Codec, codecs}
import org.plasmalabs.algebras.testInterpreters.NoOpLogger
import org.plasmalabs.codecs.bytes.typeclasses.Persistable
import org.plasmalabs.codecs.bytes.typeclasses.implicits._

import java.util.InputMismatchException

//noinspection ScalaStyle
class LevelDbStoreSpec extends CatsEffectSuite with ScalaCheckEffectSuite {

  type F[A] = IO[A]

  import SpecKey._
  import SpecValue._

  implicit val logger: Logger[F] = new NoOpLogger[F]

  ResourceFixture[Path](Resource.make(Files[F].createTempDirectory)(Files[F].deleteRecursively))
    .test("Save, Contains, Read, Delete, GetAll") { testPath =>
      LevelDbStore.makeFactory[F]().flatMap(LevelDbStore.makeDb[F](testPath, _)).use { dbUnderTest =>
        for {
          underTest <- LevelDbStore.make[F, SpecKey, SpecValue](dbUnderTest)
          key = SpecKey("test1")
          keyArray = key.persistedBytes.toByteArray
          value = SpecValue("foo", 6458)
          valueArray = value.persistedBytes.toByteArray
          // The entry should not yet exist
          _ <- underTest.contains(key).assertEquals(false)
          // The entry should still be null
          _ <- IO.blocking(dbUnderTest.get(keyArray)).assertEquals(null)
          // Now write a value
          _ <- underTest.put(key, value)
          // Verify that the written value's byte array matches what we expect
          _ <- IO.blocking(dbUnderTest.get(keyArray)).map(_ sameElements valueArray).assert
          // The entry should exist
          _ <- underTest.contains(key).assertEquals(true)
          // The entry deserialized value should equal the input value
          _ <- underTest.get(key).assertEquals(Some(value))
          // Now delete the entry
          _ <- underTest.remove(key)
          // Verify that it no longer exists
          _ <- underTest.contains(key).assertEquals(false)
          // Verify directly that it no longer exists
          _ <- IO.blocking(dbUnderTest.get(keyArray)).assertEquals(null)

          // Verify get all
          dataPut =
            Seq(
              SpecKey("10") -> SpecValue("10", 10),
              SpecKey("11") -> SpecValue("11", 11),
              SpecKey("12") -> SpecValue("12", 12),
              SpecKey("13") -> SpecValue("13", 13),
              SpecKey("14") -> SpecValue("14", 14)
            )
          _       <- dataPut.traverse { case (k, v) => underTest.put(k, v) }
          dataGet <- underTest.getAll()
          _       <- assert(dataPut == dataGet).pure[F]
        } yield ()
      }
    }

  ResourceFixture[Path](Resource.make(Files[F].createTempDirectory)(Files[F].deleteRecursively))
    .test("Malformed data throws exceptions") { testPath =>
      LevelDbStore.makeFactory[F]().flatMap(LevelDbStore.makeDb[F](testPath, _)).use { dbUnderTest =>
        for {
          underTest <- LevelDbStore.make[F, SpecKey, SpecValue](dbUnderTest)
          key = SpecKey("test1")
          keyArray = key.persistedBytes.toByteArray
          _ <- IO.blocking(dbUnderTest.put(keyArray, Array[Byte](1, 2, 3, 4)))
          _ <- underTest.contains(key).assertEquals(true)
          _ <- interceptIO[InputMismatchException](underTest.get(key))
        } yield ()
      }
    }

}

case class SpecKey(id: String)

object SpecKey {

  implicit val testKeyCodec: Codec[SpecKey] =
    codecs.utf8_32.as[SpecKey]

  implicit val testKeyPersistable: Persistable[SpecKey] =
    Persistable.instanceFromCodec
}
case class SpecValue(value1: String, value2: Long)

object SpecValue {

  implicit val specValueCodec: Codec[SpecValue] =
    (codecs.utf8_32 :: codecs.vlongL).as[SpecValue]

  implicit val specValuePersistable: Persistable[SpecValue] =
    Persistable.instanceFromCodec
}
