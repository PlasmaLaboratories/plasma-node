package org.plasmalabs.mpt

sealed trait Node

case object EmptyNode extends Node

case class RefNode(hash: TreeRoot) extends Node

case class LeafNode[T](key: Array[Byte], value: T) extends Node {
  override def toString(): String = s"LeafNode(${nibblesFromHp(key).mkStringNibbles("[", ", ", "]")}, $value)"
}

case class ExtensionNode(key: Array[Byte], node: Node) extends Node {

  override def toString(): String = s"ExtensionNode(${nibblesFromHp(key).mkStringNibbles("[", ", ", "]")}, $node)"
}

case class BranchNode[T](children: Vector[Node], value: Option[T]) extends Node
