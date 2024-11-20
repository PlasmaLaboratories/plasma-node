package org.plasmalabs.mpt

import cats.effect.IO
import munit.CatsEffectSuite
import org.plasmalabs.algebras.testInterpreters.NoOpLogger
import org.typelevel.log4cats.Logger
import org.web3j.rlp.RlpType
import fs2.io.file.Files
import cats.effect.kernel.Resource
import fs2.io.file.Path
import org.web3j.rlp.RlpString
import munit.ScalaCheckEffectSuite
import org.scalacheck.Gen
import org.scalacheck.Prop
import org.scalacheck.effect.PropF
import cats.effect.kernel.Ref
import org.checkerframework.checker.units.qual.s

/**
 * This is a test suite for the MPTrie class.
 * Examples from https://github.com/gabrocheleau/merkle-patricia-trees-examples
 */
class MPTrieSpec extends CatsEffectSuite with ScalaCheckEffectSuite {

  type F[A] = IO[A]

  type SpecKey = Array[Byte]

  type SpecValue = String

  given RLPPersistable[SpecValue] with {

    override def decode(bytes: RlpType): SpecValue =
      bytes match {
        case rlpString: RlpString => new String(rlpString.getBytes(), "UTF-8")
        case _                    => throw new RuntimeException("Invalid RLP type")
      }

    def encode(t: SpecValue): RlpType = RlpString.create(t.getBytes("UTF-8"))
  }

  given MPTKeyEncoder[SpecKey] with {

    def toNibbles(t: SpecKey): Array[Byte] = t.flatMap { b =>
      Array((b >> 4).toByte, (b & 0xf).toByte)
    }
  }

  implicit val logger: Logger[F] = new NoOpLogger[F]

  val dbResource = for {
    testPath <- Resource.make(Files[IO].createTempDirectory)(Files[IO].deleteRecursively)
    mpt      <- MPTrie.makeContainer[IO, SpecKey, SpecValue](testPath)
  } yield mpt

  val cleanUpDb =
    ResourceFunFixture(dbResource)

  cleanUpDb.test("Get root of empty") { mpt =>
    assertIO(
      for {
        mptRoot       <- mpt.getMPTrie(keccak256(Array.empty))
        keyNotPresent <- mptRoot.get("test".getBytes())
      } yield keyNotPresent,
      None
    )
  }

  val simpleMPTHash = List(
    0x8e.toByte,
    0x81.toByte,
    0x43.toByte,
    0x67.toByte,
    0x21.toByte,
    0x33.toByte,
    0xdd.toByte,
    0x5a.toByte,
    0xb0.toByte,
    0xd.toByte,
    0xfc.toByte,
    0x4b.toByte,
    0x1.toByte,
    0x14.toByte,
    0x60.toByte,
    0xea.toByte,
    0x2a.toByte,
    0x7b.toByte,
    0x0.toByte,
    0xd9.toByte,
    0x10.toByte,
    0xdc.toByte,
    0x42.toByte,
    0x78.toByte,
    0x94.toByte,
    0x2a.toByte,
    0xe9.toByte,
    0x10.toByte,
    0x5c.toByte,
    0xb6.toByte,
    0x20.toByte,
    0x74.toByte
  )

  cleanUpDb.test("Insert creates a valid tree root") { mpt =>
    assertIOBoolean(
      for {
        mptRoot <- mpt.getMPTrie(keccak256(Array.empty))
        // print testKey as hex
        treeRoot       <- mptRoot.put("testKey".getBytes(), "testValue")
        newTree        <- mpt.getMPTrie(treeRoot.toByteArray)
        retrievedValue <- newTree.get("testKey".getBytes())
      } yield ((treeRoot.toByteArray.toList == simpleMPTHash) && (retrievedValue == Some("testValue")))
    )
  }

  // byte array: be ad e9 13 ab 37 dc a0 dc a2 e4 29 24 b9 18 c2 a1 ca c4 57 83 3b d8 2b 9e 32 45 de cb 87 d0 fb
  val hashTree = List(
    0xbe.toByte,
    0xad.toByte,
    0xe9.toByte,
    0x13.toByte,
    0xab.toByte,
    0x37.toByte,
    0xdc.toByte,
    0xa0.toByte,
    0xdc.toByte,
    0xa2.toByte,
    0xe4.toByte,
    0x29.toByte,
    0x24.toByte,
    0xb9.toByte,
    0x18.toByte,
    0xc2.toByte,
    0xa1.toByte,
    0xca.toByte,
    0xc4.toByte,
    0x57.toByte,
    0x83.toByte,
    0x3b.toByte,
    0xd8.toByte,
    0x2b.toByte,
    0x9e.toByte,
    0x32.toByte,
    0x45.toByte,
    0xde.toByte,
    0xcb.toByte,
    0x87.toByte,
    0xd0.toByte,
    0xfb.toByte
  )

  cleanUpDb.test("Insert creates a valid tree root when inserting hash") { mpt =>
    assertIOBoolean(
      for {
        mptRoot <- mpt.getMPTrie(keccak256(Array.empty))
        keyHash = keccak256("testKey".getBytes())
        treeRoot       <- mptRoot.put(keyHash, "testValue")
        newTree        <- mpt.getMPTrie(treeRoot.toByteArray)
        retrievedValue <- newTree.get(keyHash)
      } yield ((treeRoot.toByteArray.toList == hashTree) && (retrievedValue == Some("testValue")))
    )
  }

  // 98 aa bc d8 89 d3 3a f5 97 1f 95 82 31 be 14 6c bd 28 64 d4 48 5a b6 8c f1 fc 8c a3 e8 ce 23 88>
  val branchExample =
    List(
      0x98.toByte,
      0xaa.toByte,
      0xbc.toByte,
      0xd8.toByte,
      0x89.toByte,
      0xd3.toByte,
      0x3a.toByte,
      0xf5.toByte,
      0x97.toByte,
      0x1f.toByte,
      0x95.toByte,
      0x82.toByte,
      0x31.toByte,
      0xbe.toByte,
      0x14.toByte,
      0x6c.toByte,
      0xbd.toByte,
      0x28.toByte,
      0x64.toByte,
      0xd4.toByte,
      0x48.toByte,
      0x5a.toByte,
      0xb6.toByte,
      0x8c.toByte,
      0xf1.toByte,
      0xfc.toByte,
      0x8c.toByte,
      0xa3.toByte,
      0xe8.toByte,
      0xce.toByte,
      0x23.toByte,
      0x88.toByte
    )

  cleanUpDb.test("Insert with branch creation") { mpt =>
    assertIO(
      for {
        mptRoot  <- mpt.getMPTrie(keccak256(Array.empty))
        treeRoot <- mptRoot.put("testKey".getBytes(), "testValue")
        mptRoot  <- mpt.getMPTrie(treeRoot.toByteArray)
        treeRoot <- mptRoot.put("testKey0".getBytes(), "testValue0")
        mptRoot  <- mpt.getMPTrie(treeRoot.toByteArray)
        treeRoot <- mptRoot.put("testKeyA".getBytes(), "testValueA")
      } yield treeRoot.toByteArray.toList,
      branchExample
    )
  }

  // ca 77 01 a9 08 a4 f2 a5 6c 04 49 2f ae 96 2c c1 d0 87 1a 73 4e 1d dd f6 ff 39 3c 5a db 9a 8f 99>
  val extensionExample =
    List(
      0xca.toByte,
      0x77.toByte,
      0x01.toByte,
      0xa9.toByte,
      0x08.toByte,
      0xa4.toByte,
      0xf2.toByte,
      0xa5.toByte,
      0x6c.toByte,
      0x04.toByte,
      0x49.toByte,
      0x2f.toByte,
      0xae.toByte,
      0x96.toByte,
      0x2c.toByte,
      0xc1.toByte,
      0xd0.toByte,
      0x87.toByte,
      0x1a.toByte,
      0x73.toByte,
      0x4e.toByte,
      0x1d.toByte,
      0xdd.toByte,
      0xf6.toByte,
      0xff.toByte,
      0x39.toByte,
      0x3c.toByte,
      0x5a.toByte,
      0xdb.toByte,
      0x9a.toByte,
      0x8f.toByte,
      0x99.toByte
    )

  cleanUpDb.test("Insert with branch extension node") { mpt =>
    assertIO(
      for {
        mptRoot  <- mpt.getMPTrie(keccak256(Array.empty))
        treeRoot <- mptRoot.put("testKey".getBytes(), "testValue")
        mptRoot  <- mpt.getMPTrie(treeRoot.toByteArray)
        treeRoot <- mptRoot.put("testKey0001".getBytes(), "testValue0")
        mptRoot  <- mpt.getMPTrie(treeRoot.toByteArray)
        treeRoot <- mptRoot.put("testKey000A".getBytes(), "testValueA")
        mptRoot  <- mpt.getMPTrie(treeRoot.toByteArray)
      } yield treeRoot.toByteArray.toList,
      extensionExample
    )
  }

  def genKey() = Gen.alphaNumStr

  def genValue() = Gen.alphaNumStr

  def genMap() =
    Gen
      .nonEmptyMap(for {
        key   <- genKey()
        value <- genValue()
      } yield (key, value))

  test("Random key/value test") {
    PropF.forAllF(genMap()) { map =>
      for {
        treeRoot <- dbResource.use(mpt =>
          map.foldLeft(mpt.getMPTrie(keccak256(Array.empty))) { case (mptRoot, (key, value)) =>
            for {

              mptRoot  <- mptRoot
              treeRoot <- mptRoot.put(key.getBytes(), value)
              mptRoot  <- mpt.getMPTrie(treeRoot.toByteArray)
              _ <- assertIOBoolean(
                for {
                  retrievedValue <- mptRoot.get(key.getBytes())
                } yield retrievedValue == Some(value)
              )
            } yield mptRoot
          }
        )
      } yield ()
    }
  }

  cleanUpDb.test("Random key/value test large map") { mpt =>
    PropF.forAllF(for {
      key             <- Gen.stringOfN(4, Gen.alphaNumChar)
      value           <- Gen.alphaNumStr
      someTreeRootRef <- Gen.const(Ref.of[IO, Option[Array[Byte]]](None).unsafeRunSync())
    } yield (key, value, someTreeRootRef)) { entry =>
      val (key, value, someTreeRootRef) = entry
      for {
        someTreeRoot <- someTreeRootRef.get
        mptRoot      <- mpt.getMPTrie(someTreeRoot.getOrElse(keccak256(Array.empty)))
        treeRoot <-
          for {
            treeRoot <- mptRoot.put(key.getBytes(), value)
            mptRoot  <- mpt.getMPTrie(treeRoot.toByteArray)
            _ <- someTreeRootRef.set(
              Some(treeRoot.toByteArray)
            )
            _ <- assertIOBoolean(
              for {
                retrievedValue <- mptRoot.get(key.getBytes())
              } yield retrievedValue == Some(value)
            )
          } yield mptRoot

      } yield ()
    }
  }

}
