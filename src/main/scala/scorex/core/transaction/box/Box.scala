package scorex.core.transaction.box

import scorex.core.serialization.BytesSerializable
import scorex.core.transaction.box.proposition.Proposition

/**
  * Box is a state element locked by some proposition.
  */
trait GenericBox[P <: Proposition, T] extends BytesSerializable {
  val value: T
  val proposition: P

  val id: Array[Byte]
}

trait Box[P <: Proposition] extends GenericBox[P, Long]{
}
