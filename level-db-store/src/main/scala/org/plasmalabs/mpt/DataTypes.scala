package org.plasmalabs.mpt

sealed trait Node

case object EmptyNode extends Node

case class RefNode(hash: TreeRoot) extends Node

case class LeafNode[T](key: Array[Byte], value: T) extends Node {
  override def toString(): String = s"LeafNode(${nibblesFromHp(key).mkString("[", ", ", "]")}, $value)"
}

case class ExtensionNode[T](key: Array[Byte], node: Node) extends Node {

  assert(nibblesFromHp(key).filter(x => x <= 15 && x >= 0).nonEmpty)

  override def toString(): String = s"ExtensionNode(${nibblesFromHp(key).mkString("[", ", ", "]")}, $node)"
}

case class BranchNode[T](children: Vector[Node], value: Option[T]) extends Node
