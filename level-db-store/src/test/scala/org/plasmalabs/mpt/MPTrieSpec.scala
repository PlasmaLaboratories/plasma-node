package org.plasmalabs.mpt

import cats.effect.IO
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.plasmalabs.algebras.testInterpreters.NoOpLogger
import org.typelevel.log4cats.Logger
import org.web3j.rlp.RlpType
import fs2.io.file.Files
import cats.effect.kernel.Resource
import fs2.io.file.Path
import org.web3j.rlp.RlpString
import org.web3j.rlp.RlpList

/**
 * This is a test suite for the MPTrie class.
 * Examples from https://github.com/gabrocheleau/merkle-patricia-trees-examples
 */
class MPTrieSpec extends CatsEffectSuite {

  type F[A] = IO[A]

  type SpecKey = String

  type SpecValue = String

  given RLPPersistable[SpecKey] with {

    override def decode(bytes: RlpType): SpecKey =
      bytes match {
        case rlpString: RlpString => new String(rlpString.getBytes(), "UTF-8")
        case _                    => throw new RuntimeException("Invalid RLP type")
      }

    def encode(t: SpecKey): RlpType = RlpString.create(t.getBytes("UTF-8"))
  }

  given MPTKeyEncoder[SpecKey] with {

    def toNibbles(t: SpecKey): Array[Byte] = t.getBytes("UTF-8").flatMap { b =>
      Array((b >> 4).toByte, (b & 0xf).toByte)
    }
  }

  implicit val logger: Logger[F] = new NoOpLogger[F]

  val cleanUpDb =
    ResourceFunFixture(
      for {
        testPath <- Resource.make(Files[IO].createTempDirectory)(Files[IO].deleteRecursively)
        mpt      <- MPTrie.makeContainer[IO, SpecKey, SpecValue](testPath)
      } yield mpt
    )

  
  // cleanUpDb.test("Get root of empty") { mpt =>
  //   assertIO(
  //     for {
  //       mptRoot       <- mpt.getMPTrie(keccak256(Array.empty))
  //       keyNotPresent <- mptRoot.get("test")
  //     } yield keyNotPresent,
  //     None
  //   )
  // }


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
        treeRoot       <- mptRoot.put("testKey", "testValue")
        newTree        <- mpt.getMPTrie(treeRoot.toByteArray)
        retrievedValue <- newTree.get("testKey")
      } yield ((treeRoot.toByteArray.toList == simpleMPTHash) && (retrievedValue == Some("testValue")))
    )
  }

}
