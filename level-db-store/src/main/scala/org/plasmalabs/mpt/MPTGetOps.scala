package org.plasmalabs.mpt

import cats.data.OptionT
import cats.effect.kernel.Async
import org.web3j.rlp.{RlpDecoder, RlpList, RlpType}

import scala.annotation.nowarn

private[mpt] trait MPTGetOps[F[_]: Async, T: RLPPersistable] extends Optics[T] {

  self: AuxFunctions[F, T] =>

  @nowarn("msg=.*cannot be checked at runtime because its type arguments can't be determined from.*")
  def auxGet(partialKey: Nibbles, currentNode: Node): F[Option[T]] =
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
            auxGet(Nibbles.empty, value)
          }
          .orElse {
            val previousKeyNibbles = nibblesFromHp(hpEncodedKey)
            val prefixLength = partialKey.zipNibbles(previousKeyNibbles).takeWhile(_ == _).length
            if (prefixLength == previousKeyNibbles.lengthNibbles) {
              OptionT.liftF(auxGet(partialKey.dropNibbles(prefixLength), value))
            } else {
              OptionT.none
            }
          }
          .mapFilter(identity)
      case BranchNode[T](children, value) =>
        if (partialKey.isEmptyNibbles) {
          OptionT.fromOption(value)
        } else {
          val child = children(partialKey.headNibbles)
          OptionT(auxGet(partialKey.tailNibbles, child))
        }
    }).value
}
