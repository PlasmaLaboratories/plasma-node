package co.topl.attestation.keyManagement

import cats.implicits._
import co.topl.crypto.Pbkdf2Sha512
import co.topl.crypto.hash.sha256

import scala.math.BigInt
import scala.util.Try

/**
 * AMS 2020:
 * Phrase translator class for translating given seed phrases
 * and UUID strings generated by java.util.UUID.randomUUID.toString
 *
 * Mnemonic seed phrase standard is given by the
 * Bitcoin Improvement Project 39 (BIP39) specification:
 * https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki
 *
 * Expected input phrase length is 12, 15, 18, 21, or 24 words
 * corresponding to 128, 160, 192, 224, and 256 bits of entropy respectively
 *
 * Output is 12 word seed phrase produced by 128 bit entropy of a random UUID string
 *
 * Phrase list directory given by:
 * https://github.com/bitcoin/bips/tree/master/bip-0039
 */

case class Bip39(words: List[String], hash: String) {

  /**
   * Verifies the wordlist for the given language by calculating the SHA2 hash
   * @return true if hash matches precalculated hash
   */
  def verifyPhraseList: Boolean =
    sha256
      .hash(words.mkString.getBytes("UTF-8"))
      .value
      .map(byte => "%02x" format byte)
      .mkString == hash

  def hexToUuid(s: String): String =
    s.slice(0, 8) + "-" + s.slice(8, 12) + "-" + s.slice(12, 16) + "-" + s.slice(16, 20) + "-" + s.substring(20)

  /**
   * Checks if user input seedphrase is valid
   * @param phrase user input seedphrase
   * @return true if seedphrase is valid, false if seedphrase invalid
   */
  def validateInputPhrase(phrase: String): Boolean = {
    val phraseWords: List[String] = phrase.split(" ").toList
    val pl = phraseWords.length

    if (phraseWords.forall(word => words.contains(word)) && Bip39.validPhraseLengths.contains(pl)) {
      val phraseBin = phraseWords.map(words.indexOf(_)).map(Bip39.toBinaryIndex).mkString
      val phraseHashBin: List[String] =
        sha256
          .hash(
            phraseBin.slice(0, Bip39.entMap(pl)).grouped(Bip39.byteLen).toArray map {
              Integer.parseInt(_, 2).toByte
            }
          )
          .value
          .map(Bip39.toBinaryByte)
          .toList
      phraseBin.substring(Bip39.entMap(pl)) == phraseHashBin.head.slice(0, Bip39.chkMap(pl))
    } else {
      false
    }
  }

  def phraseToBytes(phrase: String): Array[Byte] = {
    val phraseWords: List[String] = phrase.split(" ").toList
    phraseWords
      .map(words.indexOf(_))
      .map(Bip39.toBinaryIndex)
      .mkString
      .slice(0, Bip39.entMap(phraseWords.length))
      .grouped(Bip39.byteLen)
      .toArray
      .map(Integer.parseInt(_, 2).toByte)
  }

  /**
   * Translates valid seed phrase to a hex string
   * @param phrase user input seed phrase
   * @return hex string
   */
  def phraseToHex(phrase: String): String =
    phraseToBytes(phrase).map("%02x" format _).mkString

  def phraseToSeed(phrase: String, password: Option[String]): Array[Byte] =
    Pbkdf2Sha512.generateKey(
      phraseToBytes(phrase),
      ("mnemonic" + password.getOrElse("")).getBytes("UTF-8"),
      64,
      2048)

  /**
   * Produces a seed phrase from a UUID string
   * @param inputUuid generated by KeyFile.uuid
   * @return hex seed string, mnemonic seed phrase
   */
  def uuidSeedPhrase(inputUuid: String): (String, String) = {
    val seed = inputUuid.filterNot("-".toSet)
    val seedBytes: Array[Byte] = seed.grouped(2).toArray.map(Integer.parseInt(_, 16).toByte)
    val seedBin: Array[String] = seedBytes.map(Bip39.toBinaryByte)
    val seedHashBin: Array[String] = sha256.hash(seedBytes).value.map(Bip39.toBinaryByte)
    val phrase = (seedBin.mkString("") + seedHashBin(0).slice(0, Bip39.endCSMap(seedBin.mkString("").length)))
      .grouped(Bip39.indexLen)
      .toArray
      .map(Integer.parseInt(_, 2))
      .map(words(_))
      .mkString(" ")
    (seed, phrase)
  }
}

object Bip39 {

  val wordlistDirectory = "bip-0039"

  /*
   *  CS = ENT / 32
   *  MS = (ENT + CS) / 11
   *
   * |  ENT  | CS | ENT+CS |  MS  |
   * +-------+----+--------+------+
   * |  128  |  4 |   132  |  12  |
   * |  160  |  5 |   165  |  15  |
   * |  192  |  6 |   198  |  18  |
   * |  224  |  7 |   231  |  21  |
   * |  256  |  8 |   264  |  24  |
   *
   */

  val validPhraseLengths: List[Int] = List(12, 15, 18, 21, 24)

  val entMap: Map[Int, Int] = Map(12 -> 128, 15 -> 160, 18 -> 192, 21 -> 224, 24 -> 256)
  val chkMap: Map[Int, Int] = Map(12 -> 4, 15 -> 5, 18 -> 6, 21 -> 7, 24 -> 8)
  val endCSMap: Map[Int, Int] = Map(128 -> 4, 160 -> 5, 192 -> 6, 224 -> 7, 256 -> 8)
  val byteLen = 8
  val indexLen = 11

  case class ReadWordListFailure(exception: Throwable)

  sealed abstract class Language(val file: String, val hash: String) {

    def getWords: Either[ReadWordListFailure, List[String]] =
      Try(scala.io.Source.fromResource(s"${wordlistDirectory}/$file").getLines.toList).toEither
        .leftMap(ReadWordListFailure)
  }

  def toBinaryIndex(i: Int): String = String.format("%11s", BigInt(i).toString(2)).replace(' ', '0')

  def toBinaryByte(b: Byte): String = String.format("%8s", BigInt(b & 0xff).toString(2)).replace(' ', '0')

  def withLanguage(phraseLanguage: Language): Either[ReadWordListFailure, Bip39] =
    phraseLanguage.getWords.map(Bip39(_, phraseLanguage.hash))

  case object ChineseSimplified
      extends Language("chinese_simplified.txt", "bfd683b91db88609fabad8968c7efe4bf69606bf5a49ac4a4ba5e355955670cb")

  case object ChineseTraditional
      extends Language("chinese_traditional.txt", "85b285c4e0e3eb1e52038e2cf4b4f8bba69fd814e1a09e063ce3609a1f67ad62")

  case object English
      extends Language("english.txt", "ad90bf3beb7b0eb7e5acd74727dc0da96e0a280a258354e7293fb7e211ac03db")

  case object French extends Language("french.txt", "9cbdaadbd3ce9cbaee1b360fce45e935b21e3e2c56d9fcd56b3398ced2371866")

  case object Italian
      extends Language("italian.txt", "80d2e90d7436603fd6e57cd9af6f839391e64beac1a3e015804f094fcc5ab24c")

  case object Japanese
      extends Language("japanese.txt", "d9d1fde478cbeb45c06b93632a487eefa24f6533970f866ae81f136fbf810160")

  case object Korean extends Language("korean.txt", "f04f70b26cfef84474ff56582e798bcbc1a5572877d14c88ec66551272688c73")

  case object Spanish
      extends Language("spanish.txt", "a556a26c6a5bb36db0fb7d8bf579cb7465fcaeec03957c0dda61b569962d9da5")
}
