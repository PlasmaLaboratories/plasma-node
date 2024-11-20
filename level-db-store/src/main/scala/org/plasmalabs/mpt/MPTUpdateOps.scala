package org.plasmalabs.mpt

import cats.data.OptionT
import cats.effect.kernel.Async
import org.web3j.rlp.{RlpDecoder, RlpList, RlpType}

import scala.annotation.nowarn

private[mpt] trait MPTUpdateOps[F[_]: Async, T: RLPPersistable] extends Optics[T] {

  self: AuxFunctions[F, T] =>

  @nowarn("msg=.*cannot be checked at runtime because its type arguments can't be determined from.*")
  def auxUpdate(currentNode: Node, currentPartialKeyNibbles: Array[Byte], f: T => T): F[Option[Node]] =
    (currentNode match
      case EmptyNode =>
        OptionT.none
      case RefNode(hash) =>
        for {
          refNode <- OptionT(levelDb.get(hash.toByteArray))
          nextNode <- OptionT.fromOption(
            (rlpTypeToNode compose ((x: RlpList) => x.getValues().get(0)) compose RlpDecoder.decode)(
              refNode
            )
          )
          res <- OptionT(auxUpdate(nextNode, currentPartialKeyNibbles, f))
        } yield res
      case LeafNode[T](hpEncodedPreviousPartialKey, value) =>
        val previousPartialKeyNibbles = nibblesFromHp(hpEncodedPreviousPartialKey)
        if (currentPartialKeyNibbles.sameElements(previousPartialKeyNibbles)) { // replace case
          OptionT.liftF(createNewLeaf(currentPartialKeyNibbles, f(value)))
        } else {
          OptionT.none
        }
      case n @ ExtensionNode[T](hpEncodedPreviousPartialKey, node) =>
        val previousPartialKeyNibbles = nibblesFromHp(hpEncodedPreviousPartialKey)
        if (currentPartialKeyNibbles.sameElements(previousPartialKeyNibbles)) {
          for {
            newNode <- OptionT(auxUpdate(node, Array.emptyByteArray, f))
            result  <- OptionT.liftF(capNode(ExtensionNode[T](hpEncodedPreviousPartialKey, newNode)))
          } yield result
        } else {
          val prefixLength =
            currentPartialKeyNibbles.zip(previousPartialKeyNibbles).takeWhile(_ == _).length
          if (prefixLength != 0) {
            if (previousPartialKeyNibbles.drop(prefixLength).length == 0) {
              for {
                updatedChild <- OptionT(
                  auxUpdate(node, currentPartialKeyNibbles.drop(prefixLength), f)
                )
                cappedExtension <- OptionT.liftF(
                  capNode(ExtensionNode[T](hpEncodedPreviousPartialKey, updatedChild))
                )
              } yield cappedExtension
            } else OptionT.none
          } else {
            OptionT.none
          }
        }
      case n @ BranchNode[T](children, someValue) =>
        if (currentPartialKeyNibbles.isEmpty) {
          val newBranch = BranchNode[T](
            children,
            someValue.map(f)
          )
          OptionT.liftF(capNode(newBranch))
        } else {
          val child = children(currentPartialKeyNibbles.head)
          for {
            updatedChild <- OptionT(auxUpdate(child, currentPartialKeyNibbles.tail, f))
            newBranch = BranchNode[T](
              children.updated(currentPartialKeyNibbles.head, updatedChild),
              someValue
            )
            result <- OptionT.liftF(capNode(newBranch))
          } yield result
        }
    ).value
}
