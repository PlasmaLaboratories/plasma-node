package org.plasmalabs.mpt

import cats.data.OptionT
import cats.effect.kernel.Async
import org.web3j.rlp.{RlpDecoder, RlpList, RlpType}

import scala.annotation.nowarn

private[mpt] trait MPTPutOps[F[_]: Async, T: RLPPersistable] extends Optics[T] {

  self: AuxFunctions[F, T] =>

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
            newNode <- OptionT(auxPut(node, Nibbles.empty, t))
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
                .liftF(capNode(ExtensionNode[T](hp(previousPartialKeyNibbles.tailNibbles, false), node)))
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
                  .liftF(capNode(ExtensionNode[T](hp(previousPartialKeyNibbles.tailNibbles, false), node)))
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
                        ExtensionNode[T](hp(previousPartialKeyNibbles.drop(prefixLength).tailNibbles, false), node)
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
                    currentPartialKeyNibbles.drop(prefixLength).tailNibbles,
                    t
                  )
                )
                cappedExtension <-
                  if (previousPartialKeyNibbles.drop(prefixLength).tailNibbles.length > 0)
                    OptionT.liftF(
                      capNode(
                        ExtensionNode[T](hp(previousPartialKeyNibbles.drop(prefixLength).tailNibbles, false), node)
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
          import cats.implicits._
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
            updatedChild <- OptionT(auxPut(child, currentPartialKeyNibbles.tailNibbles, t))
            newBranch = BranchNode[T](
              children.updated(currentPartialKeyNibbles.head, updatedChild),
              someValue
            )
            result <- OptionT.liftF(capNode(newBranch))
          } yield result
        }
    ).value

}
