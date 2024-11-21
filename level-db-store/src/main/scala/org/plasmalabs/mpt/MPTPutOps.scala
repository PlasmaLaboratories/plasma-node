package org.plasmalabs.mpt

import cats.data.OptionT
import cats.effect.kernel.Async
import org.web3j.rlp.{RlpDecoder, RlpList, RlpType}

import scala.annotation.nowarn

private[mpt] trait MPTPutOps[F[_]: Async, T: RLPPersistable] extends Optics[T] {

  self: AuxFunctions[F, T] =>

  private def goDownExtensionNibbles(
    hpEncodedPreviousPartialKey: Array[Byte],
    node:                        Node,
    partialNibbles:              Nibbles,
    t:                           T
  ) =
    for {
      updatedChild <- OptionT(
        auxPut(node, partialNibbles, t)
      )
      cappedExtension <- OptionT.liftF(
        capNode(ExtensionNode(hpEncodedPreviousPartialKey, updatedChild))
      )
    } yield cappedExtension

  private def replaceExtensionWithBranch(previousPartialKeyNibbles: Nibbles, node: Node, t: T) =
    if (previousPartialKeyNibbles.lengthNibbles == 1) {
      OptionT.liftF(createBranch(previousPartialKeyNibbles, node, t))
    } else {
      for {
        cappedExtension <- OptionT
          .liftF(capNode(ExtensionNode(hp(previousPartialKeyNibbles.tailNibbles, false), node)))
        newBranch <- OptionT.liftF(
          createBranch(
            previousPartialKeyNibbles,
            cappedExtension,
            t
          )
        )
      } yield newBranch
    }

  private def replaceExtensionWithBranch(
    previousPartialKeyNibbles: Nibbles,
    node:                      Node,
    currentPartialKeyNibbles:  Nibbles,
    t:                         T
  ) =
    if (previousPartialKeyNibbles.lengthNibbles == 1) {
      OptionT.liftF(createBranch(previousPartialKeyNibbles, node, currentPartialKeyNibbles, t))
    } else {
      for {
        cappedExtension <- OptionT
          .liftF(capNode(ExtensionNode(hp(previousPartialKeyNibbles.tailNibbles, false), node)))
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

  private def replaceExtensionWithShorterExtension(
    newPrefixNibbles:   Nibbles,
    previousKeyNibbles: Nibbles,
    node:               Node,
    currentKeyNibbles:  Nibbles,
    t:                  T
  ) =
    for {
      newLeaf <- OptionT.liftF(
        createNewLeaf(
          currentKeyNibbles.tailNibbles,
          t
        )
      )
      cappedExtension <-
        if (previousKeyNibbles.tailNibbles.lengthNibbles > 0)
          OptionT.liftF(
            capNode(
              ExtensionNode(hp(previousKeyNibbles.tailNibbles, false), node)
            )
          )
        else
          OptionT.pure(node)
      newBranch <- OptionT.liftF(
        createBranch(
          previousKeyNibbles,
          cappedExtension,
          currentKeyNibbles,
          newLeaf
        )
      )
      cappedExtension <- OptionT.liftF(
        capNode(
          ExtensionNode(hp(newPrefixNibbles, false), newBranch)
        )
      )
    } yield cappedExtension

  @nowarn("msg=.*cannot be checked at runtime because its type arguments can't be determined from.*")
  def auxPut(currentNode: Node, currentPartialKeyNibbles: Nibbles, t: T): F[Option[Node]] =
    (currentNode match
      case EmptyNode =>
        import cats.implicits._
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
        if (currentPartialKeyNibbles.sameElementsNibbles(previousPartialKeyNibbles)) { // replace case
          OptionT.liftF(createNewLeaf(currentPartialKeyNibbles, t))
        } else {
          val prefixLength =
            currentPartialKeyNibbles.zipNibbles(previousPartialKeyNibbles).takeWhile(_ == _).length
          OptionT
            .liftF {
              if (prefixLength == 0) { // no sharing, we create a branch
                createBranch(currentPartialKeyNibbles, t, previousPartialKeyNibbles, value)
              } else { // sharing, we create an extension
                createNewExtension(
                  currentPartialKeyNibbles.takeNibbles(prefixLength),
                  currentPartialKeyNibbles.dropNibbles(prefixLength),
                  t,
                  previousPartialKeyNibbles.dropNibbles(prefixLength),
                  value
                )
              }
            }
        }
      case n @ ExtensionNode(hpEncodedPreviousPartialKey, node) =>
        // this by definition has at least one nibble
        val previousPartialKeyNibbles = nibblesFromHp(hpEncodedPreviousPartialKey)
        if (currentPartialKeyNibbles.sameElementsNibbles(previousPartialKeyNibbles)) {
          goDownExtensionNibbles(hpEncodedPreviousPartialKey, node, Nibbles.empty, t)
        } else if (currentPartialKeyNibbles.isEmptyNibbles) {
          replaceExtensionWithBranch(previousPartialKeyNibbles, node, t)
        } else {
          // now, we have have some partial key to match
          val prefixLength =
            currentPartialKeyNibbles.zipNibbles(previousPartialKeyNibbles).takeWhile(_ == _).length
          if (prefixLength == 0) {
            replaceExtensionWithBranch(previousPartialKeyNibbles, node, currentPartialKeyNibbles, t)
          } else { // sharing, we create an extension
            if (previousPartialKeyNibbles.dropNibbles(prefixLength).lengthNibbles == 0) {
              goDownExtensionNibbles(
                hpEncodedPreviousPartialKey,
                node,
                currentPartialKeyNibbles.dropNibbles(prefixLength),
                t
              )
            } else if (currentPartialKeyNibbles.dropNibbles(prefixLength).lengthNibbles == 0) {
              replaceExtensionWithBranch(previousPartialKeyNibbles.dropNibbles(prefixLength), node, t)
            } else {
              replaceExtensionWithShorterExtension(
                previousPartialKeyNibbles.takeNibbles(prefixLength),
                previousPartialKeyNibbles.dropNibbles(prefixLength),
                node,
                currentPartialKeyNibbles.dropNibbles(prefixLength),
                t
              )
            }
          }
        }
      case n @ BranchNode[T](children, someValue) =>
        // two cases
        // 1. currentPartialKeyNibbles.isEmpty
        // here we just update the value of the branch
        if (currentPartialKeyNibbles.isEmptyNibbles) {
          import cats.implicits._
          val newBranch = BranchNode[T](
            children,
            t.some
          )
          OptionT.liftF(capNode(newBranch))
        } else {
          // 2. currentPartialKeyNibbles.nonEmpty
          // here we have to go down the branch and update the child or children
          val child = children(currentPartialKeyNibbles.headNibbles)
          for {
            updatedChild <- OptionT(auxPut(child, currentPartialKeyNibbles.tailNibbles, t))
            newBranch = BranchNode[T](
              children.updated(currentPartialKeyNibbles.headNibbles, updatedChild),
              someValue
            )
            result <- OptionT.liftF(capNode(newBranch))
          } yield result
        }
    ).value

}
