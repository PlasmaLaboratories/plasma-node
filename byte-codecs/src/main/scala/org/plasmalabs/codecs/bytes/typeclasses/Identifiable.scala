package org.plasmalabs.codecs.bytes.typeclasses

import scodec.bits.ByteVector

import scala.language.implicitConversions

/**
 * Satisfies that T can be uniquely identified
 */
trait Identifiable[T] {
  def idOf(t: T): (Byte, ByteVector)
}

object Identifiable {

  def apply[A](implicit instance: Identifiable[A]): Identifiable[A] = instance

  trait Ops[A] {
    def typeClassInstance: Identifiable[A]
    def self: A
    def id: (Byte, ByteVector) = typeClassInstance.idOf(self)
  }

  trait ToIdentifiableOps {

    implicit def toSemigroupOps[A](target: A)(implicit tc: Identifiable[A]): Ops[A] = new Ops[A] {
      val self: A = target
      val typeClassInstance: Identifiable[A] = tc
    }
  }
}
