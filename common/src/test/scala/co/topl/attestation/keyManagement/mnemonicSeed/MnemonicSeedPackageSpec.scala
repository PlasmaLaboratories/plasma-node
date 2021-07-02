package co.topl.attestation.keyManagement.mnemonicSeed

import cats.implicits._
import co.topl.attestation.keyManagement.mnemonicSeed.Mnemonic._
import co.topl.utils.CommonGenerators
import co.topl.utils.IdiomaticScalaTransition.implicits._
import co.topl.utils.encode.Base16
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

class MnemonicSeedPackageSpec
    extends AnyPropSpec
    with CommonGenerators
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks {

  property("12 phrase mnemonic with valid words should be valid") {
    val phrase = "cat swing flag economy stadium alone churn speed unique patch report train"
    val mnemonic = Mnemonic.fromPhrase(phrase, Mnemonic12, English)

    mnemonic.isRight shouldBe true
  }

  property("12 phrase mnemonic with invalid word length should be invalid") {
    val phrase = "result fresh margin life life filter vapor trim"

    val mnemonic = Mnemonic.fromPhrase(phrase, Mnemonic12, English)

    mnemonic shouldBe Left(InvalidWordLength())
  }

  property("12 phrase mnemonic with invalid words should be invalid") {
    val phrase = "amber glue hallway can truth drawer wave flex cousin grace close compose"

    val mnemonic = Mnemonic.fromPhrase(phrase, Mnemonic12, English)

    mnemonic shouldBe Left(InvalidWords())
  }

  property("12 phrase mnemonic with valid words and invalid checksum should be invalid") {
    val phrase = "ugly wire busy skate slice kidney razor eager bicycle struggle aerobic picnic"
    val mnemonic = Mnemonic.fromPhrase(phrase, Mnemonic12, English)

    mnemonic shouldBe Left(InvalidChecksum())
  }

  property("phrase should output the same seed if same password") {
    forAll(stringGen) { s =>
      val phrase = "ozone drill grab fiber curtain grace pudding thank cruise elder eight picnic"
      val mnemonic = Mnemonic.fromPhrase(phrase, Mnemonic12, English).getOrElse(throw new Error("Invalid mnemonic!"))
      val password = Some(s)

      val firstAttempt = mnemonic(password)
      val secondAttempt = mnemonic(password)

      firstAttempt shouldBe secondAttempt
    }
  }

  property("phrase should output a different if different password") {
    forAll(stringGen, stringGen) { (s1, s2) =>
      val phrase = "ozone drill grab fiber curtain grace pudding thank cruise elder eight picnic"
      val mnemonic = Mnemonic.fromPhrase(phrase, Mnemonic12, English).getOrElse(throw new Error("Invalid mnemonic!"))
      val password1 = Some(s1)
      val password2 = Some(s2)

      val firstAttempt = mnemonic(password1)
      val secondAttempt = mnemonic(password2)

      if (s1 != s2) firstAttempt sameElements secondAttempt shouldBe false
    }
  }

  property("from UUID should return valid mnemonic with size 12") {
    forAll(Gen.uuid) { uuid =>
      val mnemonic = Mnemonic.fromUuid(uuid, English)

      mnemonic.isRight shouldBe true
    }
  }

  def entropyLengthTest(bytes: Int, expected: MnemonicSize): Unit =
    property(s"from entropy of length $bytes should be valid") {
      forAll(specificLengthBytesGen(bytes)) { entropy =>
        if (entropy.length == bytes) {
          val mnemonic = Mnemonic.fromEntropy(entropy, expected, English)

          mnemonic.isRight shouldBe true
        }
      }
    }

  entropyLengthTest(16, Mnemonic12)
  entropyLengthTest(20, Mnemonic15)
  entropyLengthTest(24, Mnemonic18)
  entropyLengthTest(28, Mnemonic21)
  entropyLengthTest(32, Mnemonic24)

  case class Bip39TestVector(mnemonic: Mnemonic, seed: String)

  // expected password for all test vectors is TREZOR
  val testVectors: Seq[Bip39TestVector] = Seq(
    mnemonic12TestVector(
      "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about",
      "c55257c360c07c72029aebc1b53c05ed0362ada38ead3e3e9efa3708e53495531f09a6987599d18264c1e1c92f2cf141630c7a3c4" +
      "ab7c81b2f001698e7463b04"
    ),
    mnemonic12TestVector(
      "legal winner thank year wave sausage worth useful legal winner thank yellow",
      "2e8905819b8723fe2c1d161860e5ee1830318dbf49a83bd451cfb8440c28bd6fa457fe1296106559a3c80937a1c1069be3a3a5bd3" +
      "81ee6260e8d9739fce1f607"
    ),
    mnemonic12TestVector(
      "letter advice cage absurd amount doctor acoustic avoid letter advice cage above",
      "d71de856f81a8acc65e6fc851a38d4d7ec216fd0796d0a6827a3ad6ed5511a30fa280f12eb2e47ed2ac03b5c462a0358d18d69fe4" +
      "f985ec81778c1b370b652a8"
    ),
    mnemonic12TestVector(
      "zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo wrong",
      "ac27495480225222079d7be181583751e86f571027b0497b5b5d11218e0a8a13332572917f0f8e5a589620c6f15b11c61dee32765" +
      "1a14c34e18231052e48c069"
    ),
    mnemonic18TestVector(
      "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon " +
      "abandon abandon abandon abandon agent",
      "035895f2f481b1b0f01fcf8c289c794660b289981a78f8106447707fdd9666ca06da5a9a565181599b79f53b844d8a71dd9f439c5" +
      "2a3d7b3e8a79c906ac845fa"
    ),
    mnemonic18TestVector(
      "legal winner thank year wave sausage worth useful legal winner thank year wave sausage worth useful legal " +
      "will",
      "f2b94508732bcbacbcc020faefecfc89feafa6649a5491b8c952cede496c214a0c7b3c392d168748f2d4a612bada0753b52a1c7ac" +
      "53c1e93abd5c6320b9e95dd"
    ),
    mnemonic18TestVector(
      "letter advice cage absurd amount doctor acoustic avoid letter advice cage absurd amount doctor acoustic " +
      "avoid letter always",
      "107d7c02a5aa6f38c58083ff74f04c607c2d2c0ecc55501dadd72d025b751bc27fe913ffb796f841c49b1d33b610cf0e91d3aa239" +
      "027f5e99fe4ce9e5088cd65"
    ),
    mnemonic18TestVector(
      "zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo when",
      "0cd6e5d827bb62eb8fc1e262254223817fd068a74b5b449cc2f667c3f1f985a76379b43348d952e2265b4cd129090758b3e3c2c49" +
      "103b5051aac2eaeb890a528"
    ),
    mnemonic24TestVector(
      "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon " +
      "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon art",
      "bda85446c68413707090a52022edd26a1c9462295029f2e60cd7c4f2bbd3097170af7a4d73245cafa9c3cca8d561a7c3de6f5d4a1" +
      "0be8ed2a5e608d68f92fcc8"
    ),
    mnemonic24TestVector(
      "legal winner thank year wave sausage worth useful legal winner thank year wave sausage worth useful legal " +
      "winner thank year wave sausage worth title",
      "bc09fca1804f7e69da93c2f2028eb238c227f2e9dda30cd63699232578480a4021b146ad717fbb7e451ce9eb835f43620bf5c514d" +
      "b0f8add49f5d121449d3e87"
    ),
    mnemonic24TestVector(
      "letter advice cage absurd amount doctor acoustic avoid letter advice cage absurd amount doctor acoustic " +
      "avoid letter advice cage absurd amount doctor acoustic bless",
      "c0c519bd0e91a2ed54357d9d1ebef6f5af218a153624cf4f2da911a0ed8f7a09e2ef61af0aca007096df430022f7a2b6fb91661a9" +
      "589097069720d015e4e982f"
    ),
    mnemonic24TestVector(
      "zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo vote",
      "dd48c104698c30cfe2b6142103248622fb7bb0ff692eebb00089b32d22484e1613912f0a5b694407be899ffd31ed3992c456cdf60" +
      "f5d4564b8ba3f05a69890ad"
    ),
    mnemonic12TestVector(
      "ozone drill grab fiber curtain grace pudding thank cruise elder eight picnic",
      "274ddc525802f7c828d8ef7ddbcdc5304e87ac3535913611fbbfa986d0c9e5476c91689f9c8a54fd55bd38606aa6a8595ad213d4c" +
      "9c9f9aca3fb217069a41028"
    ),
    mnemonic18TestVector(
      "gravity machine north sort system female filter attitude volume fold club stay feature office ecology " +
      "stable narrow fog",
      "628c3827a8823298ee685db84f55caa34b5cc195a778e52d45f59bcf75aba68e4d7590e101dc414bc1bbd5737666fbbef35d1f190" +
      "3953b66624f910feef245ac"
    ),
    mnemonic24TestVector(
      "hamster diagram private dutch cause delay private meat slide toddler razor book happy fancy gospel " +
      "tennis maple dilemma loan word shrug inflict delay length",
      "64c87cde7e12ecf6704ab95bb1408bef047c22db4cc7491c4271d170a1b213d20b385bc1588d9c7b38f1b39d415665b8a9030c9ec" +
      "653d75e65f847d8fc1fc440"
    ),
    mnemonic12TestVector(
      "scheme spot photo card baby mountain device kick cradle pact join borrow",
      "ea725895aaae8d4c1cf682c1bfd2d358d52ed9f0f0591131b559e2724bb234fca05aa9c02c57407e04ee9dc3b454aa63fbff483a8" +
      "b11de949624b9f1831a9612"
    ),
    mnemonic18TestVector(
      "horn tenant knee talent sponsor spell gate clip pulse soap slush warm silver nephew swap uncle crack brave",
      "fd579828af3da1d32544ce4db5c73d53fc8acc4ddb1e3b251a31179cdb71e853c56d2fcb11aed39898ce6c34b10b5382772db8796" +
      "e52837b54468aeb312cfc3d"
    ),
    mnemonic24TestVector(
      "panda eyebrow bullet gorilla call smoke muffin taste mesh discover soft ostrich alcohol speed nation flash" +
      " devote level hobby quick inner drive ghost inside",
      "72be8e052fc4919d2adf28d5306b5474b0069df35b02303de8c1729c9538dbb6fc2d731d5f832193cd9fb6aeecbc469594a70e3dd" +
      "50811b5067f3b88b28c3e8d"
    ),
    mnemonic12TestVector(
      "cat swing flag economy stadium alone churn speed unique patch report train",
      "deb5f45449e615feff5640f2e49f933ff51895de3b4381832b3139941c57b59205a42480c52175b6efcffaa58a2503887c1e8b36" +
      "3a707256bdd2b587b46541f5"
    ),
    mnemonic18TestVector(
      "light rule cinnamon wrap drastic word pride squirrel upgrade then income fatal apart sustain crack supply" +
      " proud access",
      "4cbdff1ca2db800fd61cae72a57475fdc6bab03e441fd63f96dabd1f183ef5b782925f00105f318309a7e9c3ea6967c7801e46c8" +
      "a58082674c860a37b93eda02"
    ),
    mnemonic24TestVector(
      "all hour make first leader extend hole alien behind guard gospel lava path output census museum junior " +
      "mass reopen famous sing advance salt reform",
      "26e975ec644423f4a4c4f4215ef09b4bd7ef924e85d1d17c4cf3f136c2863cf6df0a475045652c57eb5fb41513ca2a2d67722b77e" +
      "954b4b3fc11f7590449191d"
    ),
    mnemonic12TestVector(
      "vessel ladder alter error federal sibling chat ability sun glass valve picture",
      "2aaa9242daafcee6aa9d7269f17d4efe271e1b9a529178d7dc139cd18747090bf9d60295d0ce74309a78852a9caadf0af48aae1c6" +
      "253839624076224374bc63f"
    ),
    mnemonic18TestVector(
      "scissors invite lock maple supreme raw rapid void congress muscle digital elegant little brisk hair " +
      "mango congress clump",
      "7b4a10be9d98e6cba265566db7f136718e1398c71cb581e1b2f464cac1ceedf4f3e274dc270003c670ad8d02c4558b2f8e39edea2" +
      "775c9e232c7cb798b069e88"
    ),
    mnemonic24TestVector(
      "void come effort suffer camp survey warrior heavy shoot primary clutch crush open amazing screen " +
      "patrol group space point ten exist slush involve unfold",
      "01f5bced59dec48e362f2c45b5de68b9fd6c92c6634f44d6d40aab69056506f0e35524a518034ddc1192e1dacd32c1ed3eaa3c3b1" +
      "31c88ed8e7e54c49a5d0998"
    )
  )

  property("test vectors should pass") {
    testVectors.foreach { vector =>
      val result = vector.mnemonic(Some("TREZOR"))
      val hexResult = Base16.encode(result)

      hexResult shouldBe vector.seed
    }
  }

  property("mnemonic with extra whitespace is valid") {
    val mnemonic = Mnemonic.fromPhrase(
      "vessel ladder alter error  federal sibling chat   ability sun glass valve picture",
      Mnemonic12,
      English
    )

    mnemonic.isRight shouldBe true
  }

  property("mnemonic with extra whitespace has same seed as single spaced") {
    val password: Option[String] = None

    val expectedSeed = Mnemonic
      .fromPhrase(
        "vessel ladder alter error federal sibling chat ability sun glass valve picture",
        Mnemonic12,
        English
      )
      .getOrThrow()(password)

    val seed = Mnemonic
      .fromPhrase(
        "vessel ladder alter error  federal sibling chat   ability sun glass valve picture",
        Mnemonic12,
        English
      )
      .getOrThrow()(password)

    seed shouldBe expectedSeed
  }

  property("mnemonic with capital letters is valid") {
    val mnemonic = Mnemonic.fromPhrase(
      "Legal Winner Thank Year Wave Sausage Worth Useful Legal " +
      "Winner Thank Year Wave Sausage Worth Useful Legal Will",
      Mnemonic18,
      English
    )

    mnemonic.isRight shouldBe true
  }

  property("mnemonic with capital letters has same seed as lowercase") {
    val password: Option[String] = None

    val expectedSeed = Mnemonic
      .fromPhrase(
        "legal winner thank year wave sausage worth useful legal " +
        "winner thank year wave sausage worth useful legal will",
        Mnemonic18,
        English
      )
      .getOrThrow()(password)

    val seed = Mnemonic
      .fromPhrase(
        "Legal Winner Thank Year Wave Sausage Worth Useful Legal " +
        "Winner Thank Year Wave Sausage Worth Useful Legal Will",
        Mnemonic18,
        English
      )
      .getOrThrow()(password)

    seed shouldBe expectedSeed
  }

  property("mnemonic with unusual characters is invalid") {
    val mnemonic = Mnemonic.fromPhrase(
      "voi\uD83D\uDD25d come effort suffer camp su\uD83D\uDD25rvey warrior heavy shoot primary" +
      " clutch c\uD83D\uDD25rush" +
      " open amazing screen " +
      "patrol group space point ten exist slush inv\uD83D\uDD25olve unfold",
      Mnemonic24,
      English
    )

    mnemonic shouldBe Left(InvalidWords())
  }

  def mnemonic12TestVector(phrase: String, seed: String): Bip39TestVector =
    Bip39TestVector(
      Mnemonic
        .fromPhrase(phrase, Mnemonic12, English)
        .valueOr(err => throw new Error(s"Invalid length 12 mnemonic: $phrase, $err")),
      seed
    )

  def mnemonic18TestVector(phrase: String, seed: String): Bip39TestVector =
    Bip39TestVector(
      Mnemonic
        .fromPhrase(phrase, Mnemonic18, English)
        .valueOr(err => throw new Error(s"Invalid length 12 mnemonic: $phrase, $err")),
      seed
    )

  def mnemonic24TestVector(phrase: String, seed: String): Bip39TestVector =
    Bip39TestVector(
      Mnemonic
        .fromPhrase(phrase, Mnemonic24, English)
        .valueOr(err => throw new Error(s"Invalid length 12 mnemonic: $phrase, $err")),
      seed
    )
}
