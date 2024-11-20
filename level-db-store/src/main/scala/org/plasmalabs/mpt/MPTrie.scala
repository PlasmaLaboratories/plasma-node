package org.plasmalabs.mpt

import cats.data.OptionT
import cats.effect.kernel.{Async, Resource}
import com.google.protobuf.ByteString
import fs2.io.file.Path
import org.plasmalabs.codecs.bytes.typeclasses.Persistable
import org.plasmalabs.db.leveldb.LevelDbStore
import org.typelevel.log4cats.Logger
import org.web3j.rlp.{RlpDecoder, RlpType}

import scala.annotation.{nowarn, targetName}
import org.web3j.rlp.RlpList

opaque type MPTKey = Array[Byte]

opaque type TreeRoot = Array[Byte]

extension (x: MPTKey) {
  def toByteArray: Array[Byte] = x
}

extension (x: TreeRoot) {
  @targetName("treeRootToByteArray") def toByteArray: Array[Byte] = x
}

extension (x: Array[Byte]) {

  def toTreeRoot: TreeRoot = {
    assert(x.length == 32)
    x
  }
}

trait MPTrieContainer[F[_], Key, T] {

  def getMPTrie(trieHash: Array[Byte]): F[MPTrie[F[_], Key, T]]
}

trait MPTrie[F[_], Key, T] {

  /**
   * Puts a value into the trie.
   *
   * @param id The key to store the value under.
   * @param t The value to store.
   * @return The new root hash of the trie. If there is a problem with the
   * database or the trie, this will return None.
   */
  def put(id: Key, t: T): F[TreeRoot]

  /**
   * Updates a value in the trie.
   *
   * @param id The key to update.
   * @param f The function to apply to the value.
   * @return The new root hash of the trie. None if the key does not exist and the trie is unchanged.
   */
  def update(id: Key, f: T => T): F[Option[TreeRoot]]

  /**
   * Gets a value from the trie.
   *
   * @param id The key to get the value for.
   * @return The value, if it exists.
   */
  def get(id: Key): F[Option[T]]
}

object MPTrie {

  import cats.effect.implicits.effectResourceOps
  import cats.implicits._

  given Persistable[Array[Byte]] with {
    def persistedBytes(a: Array[Byte]): ByteString = ByteString.copyFrom(a)

    def fromPersistedBytes(bytes: ByteString): Either[String, Array[Byte]] = Right(bytes.toByteArray)
  }

  def makeContainer[F[_]: Async: Logger, Key: MPTKeyEncoder, T: RLPPersistable](
    baseDirectory: Path
  ): Resource[F, MPTrieContainer[F, Key, T]] = for {
    dbFactory  <- LevelDbStore.makeFactory[F]()
    db         <- LevelDbStore.makeDb[F](baseDirectory, dbFactory)
    levelDbRef <- LevelDbStore.make[F, Array[Byte], Array[Byte]](db).toResource
    rlpDecoder <- Resource.eval(Async[F].delay(new RlpDecoder()))
  } yield new MPTrieContainer[F, Key, T] {

    val kEv = implicitly[MPTKeyEncoder[Key]]

    def getMPTrie(trieHash: Array[Byte]): F[MPTrie[F[_], Key, T]] =
      levelDbRef
        .get(trieHash)
        .map { x =>
          val root = x.getOrElse(Array.emptyByteArray)
          new MPTrie[F, Key, T] with AuxFunctions[F, T] {

            override val levelDb = levelDbRef

            def put(id: Key, t: T): F[TreeRoot] = {

              val list = RlpDecoder.decode(root)
              if (list.getValues().size() == 0) {
                val leaf = LeafNode(hp(kEv.toNibbles(id), true), t)
                val newRootBytes = bytesRlpList.reverseGet(nodeToRlp(leaf))
                val newTreeRootByes = keccak256(newRootBytes)
                levelDb
                  .put(newTreeRootByes, newRootBytes)
                  .as(newTreeRootByes.toTreeRoot)
              } else {
                val rlpType = list.getValues().get(0)
                rlpTypeToNode(rlpType) match {
                  case Some(node) =>
                    (for {
                      updatedNode <- OptionT(auxPut(node, kEv.toNibbles(id), t))
                      newRoot     <- OptionT.liftF(capNode(updatedNode))
                      refRoot     <- OptionT.liftF(nodeToRef(newRoot))
                    } yield refRoot.hash).value.flatMap {
                      case Some(hash) =>
                        Async[F].pure(hash.toByteArray.toTreeRoot)
                      case None => Async[F].raiseError(new RuntimeException("Invalid root node"))
                    }
                  case None => Async[F].raiseError(new RuntimeException("Invalid root node"))
                }
              }
            }

            def get(id: Key): F[Option[T]] = {
              // there is no way that the root node is just an rlp string
              // that would mean it is a reference, so it has to be a list
              @nowarn("msg=.*cannot be checked at runtime because its type arguments can't be determined from.*")
              def auxGet(partialKey: Array[Byte], currentNode: Node): F[Option[T]] =
                (currentNode match {
                  case EmptyNode =>
                    OptionT.none
                  case RefNode(hash) =>
                    (for {
                      refNode <- OptionT(levelDb.get(hash.toByteArray))
                      nextNode <- OptionT.fromOption(
                        (rlpTypeToNode compose ((x: RlpList) => x.getValues().get(0)) compose RlpDecoder.decode)(
                          refNode
                        )
                      )
                      res <- OptionT(auxGet(partialKey, nextNode))
                    } yield res)
                  case LeafNode[T](key, value) =>
                    // key is encoded using the hp function
                    // we encode the key using the hp function
                    OptionT.when(hp(partialKey, flag = true).sameElements(key))(value)
                  case ExtensionNode[T](hpEncodedKey, value) =>
                    OptionT
                      .whenF(hp(partialKey, flag = false).sameElements(hpEncodedKey)) {
                        auxGet(Array.emptyByteArray, value)
                      }
                      .orElse {
                        val previousKeyNibbles = nibblesFromHp(hpEncodedKey)
                        val prefixLength = partialKey.zip(previousKeyNibbles).takeWhile(_ == _).length
                        if (prefixLength == previousKeyNibbles.length) {
                          OptionT.liftF(auxGet(partialKey.drop(prefixLength), value))
                        } else {
                          OptionT.none
                        }
                      }
                      .mapFilter(identity)
                  case BranchNode[T](children, value) =>
                    if (partialKey.isEmpty) {
                      OptionT.fromOption(value)
                    } else {
                      val child = children(partialKey.head)
                      OptionT(auxGet(partialKey.tail, child))
                    }
                }).value

              (for {
                list <- OptionT.fromOption(bytesRlpList.getOption(root))
                node <- OptionT.fromOption(rlpTypeToNode(list))
                res  <- OptionT(auxGet(kEv.toNibbles(id), node))
              } yield res).value
            }

            def update(id: Key, f: T => T): F[Option[TreeRoot]] = {
              val list = RlpDecoder.decode(root)
              if (list.getValues().size() == 0) {
                None.pure[F]
              } else {
                val rlpType = list.getValues().get(0)
                rlpTypeToNode(rlpType) match {
                  case Some(node) =>
                    (for {
                      updatedNode <- OptionT(auxUpdate(node, kEv.toNibbles(id), f))
                      newRoot     <- OptionT.liftF(capNode(updatedNode))
                      refRoot     <- OptionT.liftF(nodeToRef(newRoot))
                    } yield refRoot.hash).value
                  case None => None.pure[F]
                }
              }
            }

          }
        }
  }
}
