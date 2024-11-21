package org.plasmalabs

import org.bouncycastle.jcajce.provider.digest.Keccak

package object mpt {

  /**
   * A nibble is a byte whose values is between 0 and 15, so it could
   * be represented by a single hexadecimal digit.
   */
  private[mpt] opaque type Nibbles = Array[Byte]

  object Nibbles {

    val empty: Nibbles = Array.emptyByteArray

    def apply(bytes: Array[Byte]): Nibbles = {
      assert(bytes.forall(b => b >= 0 && b <= 15))
      bytes
    }
  }

  extension (x: Nibbles) {
    inline def tailNibbles: Nibbles = x.tail
    inline def groupedNibbles(n: Int) = x.grouped(n)
    inline def isEmptyNibbles: Boolean = x.isEmpty
    inline def headNibbles: Byte = x.head
    inline def lengthNibbles: Int = x.length
    inline def mkStringNibbles(begin: String, sep: String, end: String): String = x.mkString(begin, sep, end)
    inline def zipNibbles(y:          Nibbles): Array[(Byte, Byte)] = x.zip(y)
    inline def dropNibbles(n:         Int): Nibbles = x.drop(n)
    inline def takeNibbles(n:         Int): Nibbles = x.take(n)
    inline def sameElementsNibbles(y: Nibbles): Boolean = x.sameElements(y)
  }

  /**
   * Uses the hex prefix encoding from the Ethereum Yellow Paper.
   *
   * @param nibbles The nibble array. An array of nibbles, each represented by
   * a byte. A nibbles is a byte whose values is between 0 and 15.
   * @param flag The flag to use. This flag is used in the encoding for the
   * MP Trie.
   * @return The encoded array.
   */
  def hp(nibbles: Nibbles, flag: Boolean): Array[Byte] = {
    val f = if (flag) 2 else 0
    if (nibbles.length % 2 == 0)
      (f.toByte << 4).toByte +: nibbles.grouped(2).map { case Array(a, b) => ((a << 4) | b).toByte }.toArray
    else
      (((f + 1) << 4) | nibbles.head).toByte +: nibbles.tail
        .grouped(2)
        .map { case Array(a, b) => ((a << 4) | b).toByte }
        .toArray
  }

  def nibblesFromHp(hp: Array[Byte]): Nibbles = {
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
