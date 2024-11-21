package org.plasmalabs.models.protocol

import munit.FunSuite
import org.plasmalabs.models.protocol.RatioCodec.*
import org.plasmalabs.models.utility.Ratio
import org.plasmalabs.quivr.models.Ratio as QuivrRatio
import org.plasmalabs.sdk.models.box.Value.ConfigProposal

case class SimpleTest1(stringPar: String, intPar: Int, longPar: Long)
case class SimpleTest2(stringPar: String, intPar: Int, longPar: Long, stringList: List[String], listInt: List[Int])
case class SimpleTest3(stringParOpt: Option[String])
case class RatioTest(ratio: QuivrRatio)

class ConfigConverterTest extends FunSuite {

  test("Extraction only") {
    val simpleTestString1 = "{\"stringPar\" : \"string\",  \"intPar\" : \"1\", \"longPar\" : \"-23\"}"
    val res1 = ConfigConverter.extractOrRaise[SimpleTest1](ConfigProposal(Map("jsonConfig" -> simpleTestString1)))
    assert(res1 == SimpleTest1("string", 1, -23))

    val simpleTestString2 =
      "{\"stringPar\" : \"string\",  \"intPar\" : \"1\", \"longPar\" : \"-23\", \"stringList\" : [\"a\", \"bb\", \"c\"], \"listInt\" : [1, 3, -8]}"
    val res2 = ConfigConverter.extractOrRaise[SimpleTest2](ConfigProposal(Map("jsonConfig" -> simpleTestString2)))
    assert(res2 == SimpleTest2("string", 1, -23, List("a", "bb", "c"), List(1, 3, -8)))

    val simpleTestString3 = "{\"stringParOpt\" : null}"
    val res3 = ConfigConverter.extractOrRaise[SimpleTest3](ConfigProposal(Map("jsonConfig" -> simpleTestString3)))
    assert(res3 == SimpleTest3(None))
  }

  test("Full conversion check") {
    val test10 = SimpleTest1("string", 95, -7)
    val test20 = SimpleTest2("string", 95, -7, List("der"), List(4, 0))
    val test21 = SimpleTest2("string", 95, -7, List("a"), List())
    val test30 = SimpleTest3(Option("string"))
    val test31 = SimpleTest3(None)
    assert(test10 == ConfigConverter.extractOrRaise[SimpleTest1](ConfigConverter.pack(test10)))
    assert(test20 == ConfigConverter.extractOrRaise[SimpleTest2](ConfigConverter.pack(test20)))
    assert(test21 == ConfigConverter.extractOrRaise[SimpleTest2](ConfigConverter.pack(test21)))
    assert(test30 == ConfigConverter.extractOrRaise[SimpleTest3](ConfigConverter.pack(test30)))
    assert(test31 == ConfigConverter.extractOrRaise[SimpleTest3](ConfigConverter.pack(test31)))
  }

  test("Error processing") {
    // missed map
    assert(ConfigConverter.extract[SimpleTest1](ConfigProposal()).isLeft)

    // malformed json
    val simpleTestString1 = "{\"stringPar\" : \"string\",  \"intPar\" : \"1\", \"longPar\" : \"-23\""
    assert(
      ConfigConverter
        .extract[SimpleTest1](ConfigProposal(Map(ConfigConverter.jsonConfigKey -> simpleTestString1)))
        .isLeft
    )
  }

  test("Ratio custom serialization / deserialization") {
    val ratioTest = RatioTest(Ratio(4, 5))
    assert(ratioTest == ConfigConverter.extractOrRaise[RatioTest](ConfigConverter.pack(ratioTest)))
  }

  test("Real Config") {
    val config = ConfigGenesis(
      "label",
      Ratio(56, 89),
      45,
      99,
      Ratio(99, 56),
      Ratio(66, 7),
      100,
      com.google.protobuf.duration.Duration(56, 9),
      55,
      9,
      13,
      4,
      1000
    )
    assert(config == ConfigConverter.extractOrRaise[ConfigGenesis](ConfigConverter.pack(config)))
  }

}
