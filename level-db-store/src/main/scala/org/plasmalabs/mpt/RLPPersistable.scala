package org.plasmalabs.mpt

import org.web3j.rlp.RlpType

trait RLPPersistable[T] {

  def decode(bytes: RlpType): T

  def encode(t: T): RlpType

}

trait MPTKeyEncoder[T] {

  def toNibbles(t: T): Array[Byte]

}
