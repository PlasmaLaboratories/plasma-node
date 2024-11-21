package org.plasmalabs.mpt

import cats.effect.kernel.Async
import org.plasmalabs.algebras.Store

import scala.annotation.nowarn

private[mpt] trait AuxFunctions[F[_]: Async, T: RLPPersistable]
    extends Optics[T]
    with MPTUpdateOps[F, T]
    with MPTPutOps[F, T]
    with MPTGetOps[F, T] {

  val levelDb: Store[F, Array[Byte], Array[Byte]]

  import cats.implicits._

  @nowarn("msg=.*cannot be checked at runtime because its type arguments can't be determined from.*")
  def nodeToRef(node: Node): F[RefNode] = node match {
    case ref: RefNode => ref.pure[F]
    case n: LeafNode[T] =>
      val encodedNewLeaf = byteToLeafPrism.reverseGet(n)
      val keccak = keccak256(encodedNewLeaf)
      for {
        _ <- levelDb.put(keccak, encodedNewLeaf)
      } yield RefNode(keccak.toTreeRoot)

    case n: ExtensionNode[T] =>
      val encodedExtension = byteToExtension.reverseGet(n)
      val keccak = keccak256(encodedExtension)
      for {
        _ <- levelDb.put(keccak, encodedExtension)
      } yield RefNode(keccak.toTreeRoot)
    case n: BranchNode[T] =>
      val encodedBranch = byteToBranch.reverseGet(n)
      val keccak = keccak256(encodedBranch)
      for {
        _ <- levelDb.put(keccak, encodedBranch)
      } yield RefNode(keccak256(encodedBranch).toTreeRoot)
    case EmptyNode => RefNode(keccak256(Array.emptyByteArray).toTreeRoot).pure[F]
  }

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

  def createNewLeaf(partialKey: Nibbles, value: T): F[Node] =
    capNode(LeafNode(hp(partialKey, true), value))

  def createBranch(partialKey1: Nibbles, v1: T, partialKey2: Nibbles, v2: T) = {
    val branch = if (partialKey2.isEmptyNibbles) {
      for {
        newLeaf <- createNewLeaf(partialKey1.tailNibbles, v1)
      } yield BranchNode[T](
        Vector
          .fill(16)(EmptyNode)
          .updated(partialKey1.headNibbles, newLeaf),
        Some(v2)
      )
    } else if (partialKey1.isEmptyNibbles) {
      for {
        newLeaf <- createNewLeaf(partialKey2.tailNibbles, v2)
      } yield BranchNode[T](
        Vector
          .fill(16)(EmptyNode)
          .updated(partialKey2.headNibbles, newLeaf),
        Some(v1)
      )
    } else {
      for {
        newLeaf      <- createNewLeaf(partialKey1.tailNibbles, v1)
        previousLeaf <- createNewLeaf(partialKey2.tailNibbles, v2)
      } yield BranchNode[T](
        Vector
          .fill(16)(EmptyNode)
          .updated(partialKey1.headNibbles, newLeaf)
          .updated(partialKey2.headNibbles, previousLeaf),
        None
      )
    }
    for {
      newBranch <- branch
      result    <- capNode(newBranch)
    } yield result
  }

  def createBranch(partialKey1: Nibbles, node1: Node, partialKey2: Nibbles, node2: Node) = {
    assert(partialKey1.lengthNibbles > 0)
    assert(partialKey2.lengthNibbles > 0)
    for {
      cappedNode1 <- capNode(node1)
      cappedNode2 <- capNode(node2)
      result <- capNode(
        BranchNode[T](
          Vector
            .fill(16)(EmptyNode)
            .updated(partialKey1.headNibbles, cappedNode1)
            .updated(partialKey2.headNibbles, cappedNode2),
          None
        )
      )
    } yield result
  }

  def createBranch(partialKey1: Nibbles, node: Node, v: T) = {
    assert(partialKey1.lengthNibbles > 0)
    val branch = BranchNode[T](
      Vector
        .fill(16)(EmptyNode)
        .updated(partialKey1.headNibbles, node),
      Some(v)
    )
    capNode(branch)
  }

  def createBranch(partialKey1: Nibbles, node: Node, partialKey2: Nibbles, v: T) = {
    assert(partialKey1.lengthNibbles > 0)
    assert(partialKey2.lengthNibbles > 0)
    for {
      cappedLeaf <- createNewLeaf(partialKey2.tailNibbles, v)
      newBranch = BranchNode[T](
        Vector
          .fill(16)(EmptyNode)
          .updated(partialKey1.headNibbles, node)
          .updated(partialKey2.headNibbles, cappedLeaf),
        None
      )
      result <- capNode(newBranch)
    } yield result
  }

  def createNewExtension(
    prefix:      Nibbles,
    partialKey1: Nibbles,
    v1:          T,
    partialKey2: Nibbles,
    v2:          T
  ): F[Node] =
    for {
      newBranch <- createBranch(
        partialKey1,
        v1,
        partialKey2,
        v2
      )
      newExtension = ExtensionNode[T](hp(prefix, false), newBranch)
      result <- capNode(newExtension)
    } yield result

  def createNewExtension(
    prefix:      Nibbles,
    partialKey1: Nibbles,
    node1:       Node,
    partialKey2: Nibbles,
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
