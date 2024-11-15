package org.plasmalabs.mpt

import cats.effect.kernel.Async
import org.plasmalabs.algebras.Store
import scala.annotation.nowarn

class AuxFunctions[F[_]: Async, T: RLPPersistable](private val levelDb: Store[F, Array[Byte], Array[Byte]])
    extends Optics[T] {

  import cats.implicits._

  @nowarn("msg=.*cannot be checked at runtime because its type arguments can't be determined from.*")
  def capNode(node: Node): F[Node] = node match {
    case n: RefNode =>
      n.pure[F]
    case n: LeafNode[T] =>
      val encodedNewLeaf = byteToLeafPrism.reverseGet(n)
      if (encodedNewLeaf.size < 32)
        n.pure[F]
      else {
        val keccak = keccak256(encodedNewLeaf)
        for {
          _ <- levelDb.put(keccak, encodedNewLeaf)
        } yield RefNode(keccak256(encodedNewLeaf).toTreeRoot)
      }
    case n: ExtensionNode[T] =>
      val encodedExtension = byteToExtension.reverseGet(n)
      for {
        result <-
          if (encodedExtension.size < 32)
            n.pure[F]
          else {
            val keccak = keccak256(encodedExtension)
            for {
              _ <- levelDb.put(keccak, encodedExtension)
            } yield RefNode(keccak256(encodedExtension).toTreeRoot)
          }
      } yield result
    case n: BranchNode[T] =>
      val encodedBranch = byteToBranch.reverseGet(n)
      for {
        result <-
          if (encodedBranch.size < 32)
            n.pure[F]
          else {
            val keccak = keccak256(encodedBranch)
            for {
              _ <- levelDb.put(keccak, encodedBranch)
            } yield RefNode(keccak256(encodedBranch).toTreeRoot)
          }
      } yield result
    case EmptyNode => EmptyNode.pure[F]
  }

  def createNewLeaf(partialKey: Array[Byte], value: T): F[Node] =
    capNode(LeafNode(hp(partialKey, true), value))

  def createBranch(partialKey1: Array[Byte], v1: T, partialKey2: Array[Byte], v2: T) = {
    val branch = if (partialKey2.isEmpty) {
      for {
        newLeaf <- createNewLeaf(hp(partialKey1.tail, true), v1)
      } yield BranchNode[T](
        Vector
          .fill(16)(EmptyNode)
          .updated(partialKey1.head, newLeaf),
        Some(v2)
      )
    } else if (partialKey1.isEmpty) {
      for {
        newLeaf <- createNewLeaf(hp(partialKey2.tail, true), v2)
      } yield BranchNode[T](
        Vector
          .fill(16)(EmptyNode)
          .updated(partialKey2.head, newLeaf),
        Some(v1)
      )
    } else {
      for {
        newLeaf      <- createNewLeaf(hp(partialKey1.tail, true), v1)
        previousLeaf <- createNewLeaf(hp(partialKey2.tail, true), v2)
      } yield BranchNode[T](
        Vector
          .fill(16)(EmptyNode)
          .updated(partialKey1.head, newLeaf)
          .updated(partialKey2.head, previousLeaf),
        None
      )
    }
    for {
      newBranch <- branch
      result    <- capNode(newBranch)
    } yield result
  }

  def createBranch(partialKey1: Array[Byte], node1: Node, partialKey2: Array[Byte], node2: Node) = {
    assert(partialKey1.length > 0)
    assert(partialKey2.length > 0)
    for {
      cappedNode1 <- capNode(node1)
      cappedNode2 <- capNode(node2)
      result <- capNode(
        BranchNode[T](
          Vector
            .fill(16)(EmptyNode)
            .updated(partialKey1.head, cappedNode1)
            .updated(partialKey2.head, cappedNode2),
          None
        )
      )
    } yield result
  }

  def createBranch(partialKey1: Array[Byte], node: Node, v: T) = {
    assert(partialKey1.length > 0)
    val branch = BranchNode[T](
      Vector
        .fill(16)(EmptyNode)
        .updated(partialKey1.head, node),
      Some(v)
    )
    capNode(branch)
  }

  def createBranch(partialKey1: Array[Byte], node: Node, partialKey2: Array[Byte], v: T) = {
    assert(partialKey1.length > 0)
    assert(partialKey2.length > 0)
    for {
      cappedLeaf <- createNewLeaf(hp(partialKey2.tail, true), v)
      newBranch = BranchNode[T](
        Vector
          .fill(16)(EmptyNode)
          .updated(partialKey1.head, node)
          .updated(partialKey2.head, cappedLeaf),
        None
      )
      result <- capNode(newBranch)
    } yield result
  }

  def createNewExtension(
    prefix:      Array[Byte],
    partialKey1: Array[Byte],
    v1:          T,
    partialKey2: Array[Byte],
    v2:          T
  ): F[Node] =
    for {
      newBranch <- createBranch(
        partialKey1,
        v1,
        partialKey2,
        v2
      )
      result <- capNode(ExtensionNode[T](hp(prefix, false), newBranch))
    } yield result

  def createNewExtension(
    prefix:      Array[Byte],
    partialKey1: Array[Byte],
    node1:       Node,
    partialKey2: Array[Byte],
    node2:       Node
  ): F[Node] =
    for {
      newBranch <- createBranch(
        partialKey1,
        node1,
        partialKey2,
        node2
      )
      newExtension = ExtensionNode[T](hp(prefix, false), newBranch)
      result <- capNode(newExtension)
    } yield result

}
