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
   * @return The new root hash of the trie.
   */
  def update(id: Key, f: T => T): F[TreeRoot]

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
    levelDb    <- LevelDbStore.make[F, Array[Byte], Array[Byte]](db).toResource
    rlpDecoder <- Resource.eval(Async[F].delay(new RlpDecoder()))
  } yield new MPTrieContainer[F, Key, T] {

    val kEv = implicitly[MPTKeyEncoder[Key]]

    def getMPTrie(trieHash: Array[Byte]): F[MPTrie[F[_], Key, T]] =
      levelDb
        .get(trieHash)
        .map { x =>
          val root = x.getOrElse(Array.emptyByteArray)
          new MPTrie[F, Key, T] {

            val auxFcts = AuxFunctions[F, T](levelDb)
            import auxFcts._

            @nowarn("msg=.*cannot be checked at runtime because its type arguments can't be determined from.*")
            def auxPut(currentNode: Node, currentPartialKeyNibbles: Array[Byte], t: T): F[Option[Node]] =
              (currentNode match
                case EmptyNode =>
                  OptionT(createNewLeaf(currentPartialKeyNibbles, t).map(_.some))
                case RefNode(hash) =>
                  for {
                    refNode <- OptionT(levelDb.get(hash.toByteArray))
                    nextNode <- OptionT.fromOption(
                      (rlpTypeToNode compose ((x: RlpList) => x.getValues().get(0)) compose RlpDecoder.decode)(
                        refNode
                      )
                    )
                    res <- OptionT(auxPut(nextNode, currentPartialKeyNibbles, t))
                  } yield res
                case LeafNode[T](hpEncodedPreviousPartialKey, value) =>
                  val previousPartialKeyNibbles = nibblesFromHp(hpEncodedPreviousPartialKey)
                  if (currentPartialKeyNibbles.sameElements(previousPartialKeyNibbles)) { // replace case
                    OptionT.liftF(createNewLeaf(currentPartialKeyNibbles, t))
                  } else {
                    val prefixLength =
                      currentPartialKeyNibbles.zip(previousPartialKeyNibbles).takeWhile(_ == _).length
                    OptionT
                      .liftF {
                        if (prefixLength == 0) { // no sharing, we create a branch
                          createBranch(currentPartialKeyNibbles, t, previousPartialKeyNibbles, value)
                        } else { // sharing, we create an extension
                          createNewExtension(
                            currentPartialKeyNibbles.take(prefixLength),
                            currentPartialKeyNibbles.drop(prefixLength),
                            t,
                            previousPartialKeyNibbles.drop(prefixLength),
                            value
                          )
                        }
                      }
                  }
                case n @ ExtensionNode[T](hpEncodedPreviousPartialKey, node) =>
                  // this by definition has at least one nibble
                  val previousPartialKeyNibbles = nibblesFromHp(hpEncodedPreviousPartialKey)
                  if (currentPartialKeyNibbles.sameElements(previousPartialKeyNibbles)) {
                    // we are updating the value of the node at the end of the extension
                    // we leave that to the next call to decide how to do that
                    for {
                      newNode <- OptionT(auxPut(node, Array.emptyByteArray, t))
                      result  <- OptionT.liftF(capNode(ExtensionNode[T](hpEncodedPreviousPartialKey, newNode)))
                    } yield result
                  } else if (currentPartialKeyNibbles.isEmpty) {
                    // in this case we are getting here from a branch node that consumed the last nibble
                    // hence, we are replacing the value of the extension node
                    // we have two options
                    // 1. previousPartialKeyNibbles.length == 1
                    // since the whole thing become as a branch, we forget
                    // about the extension and create a branch with the node
                    if (previousPartialKeyNibbles.length == 1) {
                      OptionT.liftF(createBranch(previousPartialKeyNibbles, node, t))
                    } else {
                      // 2. previousPartialKeyNibbles.length > 1
                      // in this case the extension becomes a branch and is pushed down (hence the tail)
                      // we create a branch with the extension as a node and the value as the branch's end value
                      for {
                        cappedExtension <- OptionT
                          .liftF(capNode(ExtensionNode[T](hp(previousPartialKeyNibbles.tail, false), node)))
                        newBranch <- OptionT.liftF(
                          createBranch(
                            previousPartialKeyNibbles,
                            cappedExtension,
                            t
                          )
                        )
                      } yield newBranch
                    }
                  } else {
                    // now, we have have some partial key to match
                    val prefixLength =
                      currentPartialKeyNibbles.zip(previousPartialKeyNibbles).takeWhile(_ == _).length
                    if (prefixLength == 0) {
                      // there is no sharing, we create a branch
                      // 1. previousPartialKeyNibbles.length == 1
                      // this means tha the node occupies the place of the branch corresponding to the shared nibble
                      // and the value is in a leaf with the rest of the key
                      if (previousPartialKeyNibbles.length == 1) {
                        // as before, we create a branch with the node and another with the leaf containing the value
                        OptionT
                          .liftF(createBranch(previousPartialKeyNibbles, node, currentPartialKeyNibbles, t))
                      } else {
                        // previousPartialKeyNibbles.length > 1
                        // this means that the extension becomes a branch and is pushed down (hence the tail)
                        // and the value is in a leaf with the rest of the key
                        // in this case we create a branch with the extension and another with the leaf containing the value
                        for {
                          cappedExtension <- OptionT
                            .liftF(capNode(ExtensionNode[T](hp(previousPartialKeyNibbles.tail, false), node)))
                          newBranch <- OptionT.liftF(
                            createBranch(
                              previousPartialKeyNibbles,
                              cappedExtension,
                              currentPartialKeyNibbles,
                              t
                            )
                          )
                        } yield newBranch
                      }
                    } else { // sharing, we create an extension
                      // it cannot be the whole key, since that would have been
                      // catched by the previous case
                      // three cases:
                      // 1. previousPartialKeyNibbles.drop(prefixLength).length == 0
                      // 2. currentPartialKeyNibbles.drop(prefixLength).length == 0
                      // 3. both are non-empty
                      if (previousPartialKeyNibbles.drop(prefixLength).length == 0) {
                        // here we will replace the whole node at the end of the extension
                        // with a new value
                        // the currentPartialKeyNibbles.drop(prefixLength) might
                        // be empty, but  that is handled in the next call
                        for {
                          updatedChild <- OptionT(
                            auxPut(node, currentPartialKeyNibbles.drop(prefixLength), t)
                          )
                          cappedExtension <- OptionT.liftF(
                            capNode(ExtensionNode[T](hpEncodedPreviousPartialKey, updatedChild))
                          )
                        } yield cappedExtension
                      } else if (currentPartialKeyNibbles.drop(prefixLength).length == 0) {
                        // here, the partial key is consumed by the branch
                        // two options
                        // 1. previousPartialKeyNibbles.drop(prefixLength).length == 1
                        if (previousPartialKeyNibbles.drop(prefixLength).length == 1) {
                          // here the extension becomes a branch and disappears, leaving the node as part of the branch
                          // the value is now part of the branch
                          // in this case we create a branch with the node and another with the leaf containing the value
                          OptionT
                            .liftF(
                              createBranch(
                                previousPartialKeyNibbles.drop(prefixLength),
                                node,
                                t
                              )
                            )
                        } else { // previousPartialKeyNibbles.drop(prefixLength).length > 1
                          // here the extension becomes a branch and is pushed down
                          // the value is now part of the branch as in the other case
                          // in this case we create a branch with the extension and another with the leaf containing the value
                          for {
                            cappedExtension <- OptionT
                              .liftF(
                                capNode(
                                  ExtensionNode[T](hp(previousPartialKeyNibbles.drop(prefixLength).tail, false), node)
                                )
                              )
                            newBranch <- OptionT.liftF(
                              createBranch(
                                previousPartialKeyNibbles.drop(prefixLength),
                                cappedExtension,
                                t
                              )
                            )
                          } yield newBranch
                        }
                      } else {
                        // 3. both are non-empty
                        // here, part of the extension is pushed down
                        // the value and partial key go on to become leaves
                        for {
                          newLeaf <- OptionT.liftF(
                            createNewLeaf(
                              currentPartialKeyNibbles.drop(prefixLength).tail,
                              t
                            )
                          )
                          cappedExtension <-
                            if (previousPartialKeyNibbles.drop(prefixLength).tail.length > 0)
                              OptionT.liftF(
                                capNode(
                                  ExtensionNode[T](hp(previousPartialKeyNibbles.drop(prefixLength).tail, false), node)
                                )
                              )
                            else
                              OptionT.pure(node)
                          newBranch <- OptionT.liftF(
                            createBranch(
                              previousPartialKeyNibbles.drop(prefixLength),
                              cappedExtension,
                              currentPartialKeyNibbles.drop(prefixLength),
                              newLeaf
                            )
                          )
                          cappedExtension <- OptionT.liftF(
                            capNode(
                              ExtensionNode[T](hp(previousPartialKeyNibbles.take(prefixLength), false), newBranch)
                            )
                          )
                        } yield cappedExtension
                      }
                    }
                  }
                case n @ BranchNode[T](children, someValue) =>
                  // two cases
                  // 1. currentPartialKeyNibbles.isEmpty
                  // here we just update the value of the branch
                  if (currentPartialKeyNibbles.isEmpty) {
                    val newBranch = BranchNode[T](
                      children,
                      t.some
                    )
                    OptionT.liftF(capNode(newBranch))
                  } else {
                    // 2. currentPartialKeyNibbles.nonEmpty
                    // here we have to go down the branch and update the child or children
                    val child = children(currentPartialKeyNibbles.head)
                    for {
                      updatedChild <- OptionT(auxPut(child, currentPartialKeyNibbles.tail, t))
                      newBranch = BranchNode[T](
                        children.updated(currentPartialKeyNibbles.head, updatedChild),
                        someValue
                      )
                      result <- OptionT.liftF(capNode(newBranch))
                    } yield result
                  }
              ).value

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

            def update(id: Key, f: T => T): F[TreeRoot] = ???

          }
        }
  }
}
