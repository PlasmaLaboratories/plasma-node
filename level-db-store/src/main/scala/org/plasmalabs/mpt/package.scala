package org.plasmalabs

import org.bouncycastle.jcajce.provider.digest.Keccak

package object mpt {

  def hp(nibbles: Array[Byte], flag: Boolean): Array[Byte] = {
    val f = if (flag) 2 else 0
    if (nibbles.length % 2 == 0)
      (f.toByte << 4).toByte +: nibbles.grouped(2).map { case Array(a, b) => ((a << 4) | b).toByte }.toArray
    else
      (((f + 1) << 4) | nibbles.head).toByte +: nibbles.tail
        .grouped(2)
        .map { case Array(a, b) => ((a << 4) | b).toByte }
        .toArray
  }

  def nibblesFromHp(hp: Array[Byte]): Array[Byte] = {
    val lengthIsPair = ((hp.head >>> 4) & 0x01) == 0
    val headNibble = if (lengthIsPair) Array.emptyByteArray else Array((hp.head & 0x0f).toByte)
    headNibble ++ hp.tail.flatMap { b =>
      val first = (b >>> 4).toByte
      val correctedFirst = if (first < 0) (16 - first.abs).toByte else first
      Array(correctedFirst, (b & 0x0f).toByte)
    }
  }

  inline def hpFlag(hp: Array[Byte]): Boolean = ((hp.head >>> 4) & 0x2) == 2

  def keccak256(input: Array[Byte]): Array[Byte] = {
    val kecc = new Keccak.Digest256()
    kecc.update(input, 0, input.length)
    kecc.digest()
  }

}
