package org.plasmalabs.mpt

import cats.effect.IO
import cats.effect.kernel.{Ref, Resource}
import fs2.io.file.{Files, Path}
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.plasmalabs.algebras.testInterpreters.NoOpLogger
import org.scalacheck.Gen
import org.scalacheck.effect.PropF
import org.typelevel.log4cats.Logger
import org.web3j.rlp.{RlpString, RlpType}

/**
 * This is a test suite for the MPTrie class.
 * Examples from https://github.com/gabrocheleau/merkle-patricia-trees-examples
 */
class MPTrieSpec extends CatsEffectSuite with ScalaCheckEffectSuite with MPTTestData {

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
      val first = (b >>> 4).toByte
      val correctedFirst = if (first < 0) (16 - first.abs).toByte else first
      Array(correctedFirst, (b & 0xf).toByte)
    }
  }

  implicit val logger: Logger[F] = new NoOpLogger[F]

  val dbResource = for {
    testPath <- Resource.make(Files[IO].createTempDirectory)(Files[IO].deleteRecursively)
    mpt      <- MPTrie.makeContainer[IO, SpecKey, SpecValue](testPath)
  } yield mpt

  val dbResourceAndTreeRootRef = for {
    testPath <- Resource.make(Files[IO].createTempDirectory)(Files[IO].deleteRecursively)
    mpt      <- MPTrie.makeContainer[IO, SpecKey, SpecValue](testPath)
    ref      <- Ref.of[IO, Array[Byte]](keccak256(Array.empty)).toResource
  } yield (mpt, ref)

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
    import cats.implicits._
    PropF.forAllF(genMap()) { generatedMap =>
      for {
        treeRoot <- dbResource.use(mpt =>
          for {
            completeTree <- generatedMap.foldLeft(mpt.getMPTrie(keccak256(Array.empty))) {
              case (mptRoot, (key, value)) =>
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
            // we check that all elements are in the tree
            _ <- generatedMap
              .map { case (key, value) =>
                assertIO(
                  for {
                    obtainedValue <- completeTree.get(key.getBytes())
                  } yield obtainedValue,
                  Some(value)
                )
              }
              .toList
              .sequence
          } yield completeTree
        )
      } yield ()
    }
  }

  ResourceFunFixture(dbResourceAndTreeRootRef).test("Random key/value test large map") { mptAndRef =>
    val (mpt, treeRooRef) = mptAndRef
    PropF.forAllF(for {
      key   <- Gen.stringOfN(4, Gen.alphaNumChar)
      value <- Gen.alphaNumStr
    } yield (key, value)) { entry =>
      val (key, value) = entry
      for {
        treeRoot <- treeRooRef.get
        mptRoot  <- mpt.getMPTrie(treeRoot)
        treeRoot <- mptRoot.put(key.getBytes(), value)
        mptRoot  <- mpt.getMPTrie(treeRoot.toByteArray)
        _ <- treeRooRef.set(
          treeRoot.toByteArray
        )
        _ <- assertIOBoolean(
          for {
            retrievedValue <- mptRoot.get(key.getBytes())
          } yield retrievedValue == Some(value)
        )
      } yield ()
    }
  }

  ResourceFunFixture(dbResourceAndTreeRootRef).test("Random key/value test large map update") { mptAndRef =>
    val (mpt, treeRooRef) = mptAndRef
    PropF.forAllF(for {
      key   <- Gen.stringOfN(4, Gen.alphaNumChar)
      value <- Gen.alphaNumStr
    } yield (key, value)) { entry =>
      val (key, value) = entry
      for {
        treeRoot <- treeRooRef.get
        mptRoot  <- mpt.getMPTrie(treeRoot)
        treeRoot <- mptRoot.put(key.getBytes(), value)
        mptRoot  <- mpt.getMPTrie(treeRoot.toByteArray)
        _ <- assertIOBoolean(
          for {
            retrievedValue <- mptRoot.get(key.getBytes())
          } yield retrievedValue == Some(value)
        )
        treeRoot <- mptRoot.update(key.getBytes(), _ + "1")
        mptRoot  <- mpt.getMPTrie(treeRoot.get.toByteArray)
        _ <- assertIOBoolean(
          for {
            retrievedValue <- mptRoot.get(key.getBytes())
          } yield retrievedValue == Some(value + "1")
        )
        _ <- treeRooRef.set(treeRoot.get.toByteArray)

      } yield ()
    }
  }

}
