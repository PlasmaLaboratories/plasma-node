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
import fs2.text.utf8

/**
 * This is a test suite for the MPTrie class.
 * Examples from https://github.com/gabrocheleau/merkle-patricia-trees-examples
 */
class MPTrieSpec extends CatsEffectSuite {

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

  val cleanUpDb =
    ResourceFunFixture(
      for {
        testPath <- Resource.make(Files[IO].createTempDirectory)(Files[IO].deleteRecursively)
        mpt      <- MPTrie.makeContainer[IO, SpecKey, SpecValue](testPath)
      } yield mpt
    )

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
        mptRoot    <- mpt.getMPTrie(keccak256(Array.empty))
        treeRoot   <- mptRoot.put("testKey".getBytes(), "testValue")
        mptRoot    <- mpt.getMPTrie(treeRoot.toByteArray)
        treeRoot   <- mptRoot.put("testKey0".getBytes(), "testValue0")
        mptRoot    <- mpt.getMPTrie(treeRoot.toByteArray)
        treeRoot   <- mptRoot.put("testKeyA".getBytes(), "testValueA")
        mptRoot    <- mpt.getMPTrie(treeRoot.toByteArray)
      } yield treeRoot.toByteArray.toList,
      branchExample
    )
  }

}
