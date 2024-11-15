package org.plasmalabs.mpt

sealed trait Node

case object EmptyNode extends Node

case class RefNode(hash: TreeRoot) extends Node

case class LeafNode[T](key: Array[Byte], value: T) extends Node

case class ExtensionNode[T](key: Array[Byte], node: Node) extends Node

case class BranchNode[T](children: Vector[Node], value: Option[T]) extends Node
