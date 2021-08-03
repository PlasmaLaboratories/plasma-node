package co.topl.attestation.keyManagement.derivedKeys

import cats.implicits._
import co.topl.attestation.SignatureEd25519
import co.topl.attestation.keyManagement.derivedKeys.ExtendedPrivateKeyEd25519.InvalidDerivedKey
import co.topl.crypto.signatures.Ed25519
import co.topl.utils.SizedByteCollection
import co.topl.utils.SizedByteCollection.Types.ByteVector32
import co.topl.utils.SizedByteCollection.implicits._
import co.topl.utils.StringDataTypes.Base16Data
import co.topl.utils.codecs.implicits._
import co.topl.utils.encode.Base16
import org.scalacheck.Gen
import org.scalacheck.Gen.asciiStr
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.forAll
import scodec.bits.ByteOrdering

// Test Vectors:
// https://github.com/satoshilabs/slips/blob/master/slip-0023.md and
// https://github.com/input-output-hk/rust-cardano/blob/master/cardano/src/hdwallet.rs
class ExtendedPrivateKeyEd25519Spec extends AnyFlatSpec {

  private val positiveIntListGen: Gen[List[Int]] = Gen.listOf(Gen.chooseNum(0, Int.MaxValue))

  private val testVector1Seed =
    Base16Data.unsafe("578d685d20b602683dc5171df411d3e2")

  private val testVector2Seed =
    Base16Data.unsafe("a055b781aac0c9dc1bfb7d803bc8ffd5d4392e506db2e4a5a93f0aba958c5be7")

  "ExtendedPrivateKeyEd25519.fromSeed" should "create correct root left key with test vector #1 seed" in {
    ExtendedPrivateKeyEd25519
      .fromSeed(testVector1Seed.value)
      .leftNumber
      .toString
      .shouldBe("38096432269777187972282727382530464140043628323029465813805073381215192153792")
  }

  it should "create correct root left key with test vector #2 seed" in {
    ExtendedPrivateKeyEd25519
      .fromSeed(testVector2Seed.value)
      .leftNumber
      .toString
      .shouldBe("35870817594148037193235249761081259065186522922583196642112477624627719791504")
  }

  it should "create correct root right key with test vector #1 seed" in {
    ExtendedPrivateKeyEd25519
      .fromSeed(testVector1Seed.value)
      .rightKey
      .encodeAsBase16
      .shouldBe(Base16Data.unsafe("4064253ffefc4127489bce1b825a47329010c5afb4d21154ef949ef786204405"))
  }

  it should "create correct root right key with test vector #2 seed" in {
    ExtendedPrivateKeyEd25519
      .fromSeed(testVector2Seed.value)
      .rightKey
      .encodeAsBase16
      .shouldBe(Base16Data.unsafe("f9d99bf3cd9c7e12663e8646afa40cb3aecf15d91f2abc15d21056c6bccb3414"))
  }

  it should "create correct root public key with test vector #1 seed" in {
    ExtendedPrivateKeyEd25519
      .fromSeed(testVector1Seed.value)
      .public
      .bytes
      .encodeAsBase16
      .shouldBe(Base16Data.unsafe("83e3ecaf57f90f022c45e10d1b8cb78499c30819515ad9a81ad82139fdb12a90"))
  }

  it should "create correct root public key with test vector #2 seed" in {
    ExtendedPrivateKeyEd25519
      .fromSeed(testVector2Seed.value)
      .public
      .bytes
      .encodeAsBase16
      .shouldBe(Base16Data.unsafe("eea170f0ef97b59d22907cb429888029721ed67d3e7a1b56b81731086ab7db64"))
  }

  it should "create correct root chain code with test vector #1 seed" in {
    ExtendedPrivateKeyEd25519
      .fromSeed(testVector1Seed.value)
      .chainCode
      .encodeAsBase16
      .shouldBe(Base16Data.unsafe("22c12755afdd192742613b3062069390743ea232bc1b366c8f41e37292af9305"))
  }

  it should "create correct root chain code with test vector #2 seed" in {
    ExtendedPrivateKeyEd25519
      .fromSeed(testVector2Seed.value)
      .chainCode
      .encodeAsBase16
      .shouldBe(Base16Data.unsafe("04f1de750b62725fcc1ae1b93ca4063acb53c486b959cadaa100ebd7828e5460"))
  }

  it should "create a valid signing root key with test vector #1" in {
    val root = ExtendedPrivateKeyEd25519.fromSeed(testVector1Seed.value)

    val publicKey = root.public

    val message = "test".getBytes

    val signature = root.sign(message)

    val ec = new Ed25519

    val isValid = ec.verify(signature.sigBytes, message, publicKey.toPublicKey)

    isValid shouldBe true
  }

  it should "create a valid signing root key with test vector #2" in {
    val root = ExtendedPrivateKeyEd25519.fromSeed(testVector2Seed.value)

    val publicKey = root.public

    val message = "test".getBytes

    val signature = root.sign(message)

    val ec = new Ed25519

    val isValid = ec.verify(signature.sigBytes, message, publicKey.toPublicKey)

    isValid shouldBe true
  }

  "ExtendedPrivateKeyEd25519.deriveChildKey" should "generate valid signing keys for a hardened path" in {
    forAll(asciiStr, asciiStr, positiveIntListGen) { (seed, message, path) =>
      val root = ExtendedPrivateKeyEd25519.fromSeed(seed.getBytes)

      val derivedKey = path.foldLeft(root.asRight[InvalidDerivedKey]) {
        case (Right(key), step) => key.derive(DerivedKeyIndex.hardened(step))
        case (error, _)         => error
      }

      // do not test invalid keys
      derivedKey.foreach { privateKey =>
        val publicKey = privateKey.public

        val messageToSign = message.getBytes

        val ed25519 = new Ed25519

        val signature = privateKey.sign(messageToSign)

        val isValidSignature = ed25519.verify(signature.sigBytes, messageToSign, publicKey.toPublicKey)

        isValidSignature shouldBe true
      }
    }
  }

  "ExtendedPrivateKeyEd25519" should "pass child key derivation and signing test vector from rust-cardano" in {
    val rootKeyBytes: Array[Byte] =
      Base16Data.unsafe(
        "f8a29231ee38d6c5bf715d5bac21c750577aa3798b22d79d65bf97d6fadea15adcd1ee1abdf78bd4be64731a12deb94" +
          "d3671784112eb6f364b871851fd1c9a247384db9ad6003bbd08b3b1ddc0d07a597293ff85e961bf252b331262eddfad0d"
      ).value

    import co.topl.utils.StringDataTypes.implicits._
    println(rootKeyBytes.encodeAsBase16.show)

    val rootKey =
      ExtendedPrivateKeyEd25519(
        SizedByteCollection[ByteVector32].fit(rootKeyBytes.slice(0, 32), ByteOrdering.LittleEndian),
        SizedByteCollection[ByteVector32].fit(rootKeyBytes.slice(32, 64), ByteOrdering.LittleEndian),
        SizedByteCollection[ByteVector32].fit(rootKeyBytes.slice(64, 96), ByteOrdering.LittleEndian),
        Seq()
      )

    val expectedChildKey: Array[Byte] =
      Base16Data.unsafe(
        "60d399da83ef80d8d4f8d223239efdc2b8fef387e1b5219137ffb4e8fbdea15adc9366b7d003af37c11396de9a837" +
          "34e30e05e851efa32745c9cd7b42712c890608763770eddf77248ab652984b21b849760d1da74a6f5bd633ce41adceef07a"
      ).value

    val expectedSignature: Array[Byte] =
      Base16Data.unsafe(
        "90194d57cde4fdadd01eb7cf161780c277e129fc7135b97779a3268837e4cd2e9444b9bb91c0e84d23bba870df3c4bda" +
          "91a110ef735638fa7a34ea2046d4be04"
      ).value

    println(expectedSignature.encodeAsBase16.show)

    val childKey =
      rootKey
        .derive(DerivedKeyIndex.hardened(0))
        .getOrElse(throw new Exception("invalid child key"))

    val childKeyBytes =
      childKey.leftKey.value.toArray ++ childKey.rightKey.value.toArray ++ childKey.chainCode.value.toArray

    val signatureResult: SignatureEd25519 = childKey.sign("Hello World".getBytes("UTF-8"))

    childKeyBytes shouldBe expectedChildKey
    signatureResult.sigBytes shouldBe expectedSignature
  }

  it should "pass generated test vector #1" in {
    val `root private key` =
      Base16Data.unsafe(
        "f0d0f18e6ab029166fe4e89519ab64f42aa870fc2791fc472840c3a1ba507347fee30dcae1ae3941bde71e9ddd19eef33d0a7b91" +
          "aaa4137cea6ef4ea3c27f96a1189e5ec0628974ed7846b594ed0ee2d3ef2d8f5b91d1860ffb0a065159df8be"
      )

    val `expected m/'0 private key` =
      Base16Data.unsafe(
        "b859fdcdafa6a4552e5d4a18c44b79daf1d40f1600f6745768ddcbd9bc507347b7b1cdaf0d837051ed203813f7f3c518ae8046fbd4" +
          "de106bf1cde33496825a390f2f8270d4724314a2a4f7175cd5765c35dffbf5ccbbfc4f8497297e9e68510f"
      )

    val `expected m/'0 public key` =
      Base16Data.unsafe(
        "b983b958d41fbdfecf6c0010ac667efa3cecb02ba27099afd13bc0ef0f82e60c0f2f8270d4724314a2a4f7175cd5765c35df" +
          "fbf5ccbbfc4f8497297e9e68510f"
      )

    val `expected m/'0/'100 private key` =
      Base16Data.unsafe(
        "30c9ae886a00e5524223d96824b28b1aff0419c6026dd07509e5b5a4c15073473890a9decc12d0400869d6daf095092863bba4" +
          "5363b8e33c257e70bf7d3548aacce7b986e25839573c044c389cf8f76d8adcc6f723df9f98bfa1308f0c35282c"
      )

    val `expected m/'0/'100 public key` =
      Base16Data.unsafe(
        "4b95248060cc3bd0fee38cddf2c54b5e155a38de5cfe1846873355b35cc07566cce7b986e25839573c044c389cf8f76d8adcc6f723" +
          "df9f98bfa1308f0c35282c"
      )

    val `expected m/'0/'100/55 public key` =
      Base16Data.unsafe(
        "8e59beac508fcd431c0b7b2dae81686adf45c76c0e32af7af779ecdf78adb8fb3a5c3099aeffe333f39d4107b1f59227a7e5713b9451" +
          "8033a763a542ea289ee8"
      )

    val rootKeyBytes = `root private key`.value

    val rootKey =
      ExtendedPrivateKeyEd25519(
        SizedByteCollection[ByteVector32].fit(rootKeyBytes.slice(0, 32), ByteOrdering.LittleEndian),
        SizedByteCollection[ByteVector32].fit(rootKeyBytes.slice(32, 64), ByteOrdering.LittleEndian),
        SizedByteCollection[ByteVector32].fit(rootKeyBytes.slice(64, 96), ByteOrdering.LittleEndian),
        Seq()
      )

    val `actual m/'0 private key` =
      rootKey.derive(DerivedKeyIndex.hardened(0)).getOrElse(throw new Error())

    (
      `actual m/'0 private key`.leftKey.toVector ++
      `actual m/'0 private key`.rightKey.toVector ++
      `actual m/'0 private key`.chainCode.toVector
    ).encodeAsBase16 shouldBe `expected m/'0 private key`

    val `actual m/'0 public key` =
      `actual m/'0 private key`.public

    (
      `actual m/'0 public key`.bytes.toVector ++
      `actual m/'0 public key`.chainCode.toVector
    ).encodeAsBase16 shouldBe `expected m/'0 public key`

    val `actual m/'0/'100 private key` =
      `actual m/'0 private key`
        .derive(DerivedKeyIndex.hardened(100))
        .getOrElse(throw new Error("invalid child key"))

    (
      `actual m/'0/'100 private key`.leftKey.toVector ++
      `actual m/'0/'100 private key`.rightKey.toVector ++
      `actual m/'0/'100 private key`.chainCode.toVector
    ).encodeAsBase16 shouldBe `expected m/'0/'100 private key`

    val `actual m/'0/'100 public key` =
      `actual m/'0/'100 private key`.public

    (
      `actual m/'0/'100 public key`.bytes.toVector ++
      `actual m/'0/'100 public key`.chainCode.toVector
    ).encodeAsBase16 shouldBe `expected m/'0/'100 public key`

    val `actual m/'0/'100/55 public key` =
      `actual m/'0/'100 public key`.derive(DerivedKeyIndex.soft(55))

    (
      `actual m/'0/'100/55 public key`.bytes.toVector ++
      `actual m/'0/'100/55 public key`.chainCode.toVector
    ).encodeAsBase16 shouldBe `expected m/'0/'100/55 public key`
  }

  it should "pass generated test vector #2" in {
    val `root private key` =
      Base16Data.unsafe(
        "2090d5cdd6bdc4537ed44f109c261f3f8dbe9c17a843a77c035f55c78a723a481c285eee9cf920be4a1e1e3564763ad100fe203b5fd7" +
          "9f6535943170e53597add20dd0bcf02446e2f607419163f9dbf572393b9c2258d33df59fb0e06112d285"
      )

    val `expected m/'1852/'7091/'0/'0 private key` =
      Base16Data.unsafe(
        "60befd4438750e301c86713f2c1a5178d419ff9434d9d3dcf44b9ea5a1723a48a14867f43dc37a11f4b82c10b5c1e7c6b5cc91bcd8c0" +
          "29d180f0aca62dee72f92f5d057d61cce1664344538c61c12d99f74a8a6c331a811d8ecb468b36168ef0"
      )

    val `expected m/'1852/'7091/'0/'0/0 public key` =
      Base16Data.unsafe(
        "f119694710657f95edf110002ad3974db4c22f330b6b091355cd0b5784f04ba8b415521a3550f1e59fad614aa249aa" +
          "3245c93005efd63faf8a02ba7787176782"
      )

    val rootKeyBytes = `root private key`.value

    val rootKey =
      ExtendedPrivateKeyEd25519(
        SizedByteCollection[ByteVector32].fit(rootKeyBytes.slice(0, 32), ByteOrdering.LittleEndian),
        SizedByteCollection[ByteVector32].fit(rootKeyBytes.slice(32, 64), ByteOrdering.LittleEndian),
        SizedByteCollection[ByteVector32].fit(rootKeyBytes.slice(64, 96), ByteOrdering.LittleEndian),
        Seq()
      )

    val `actual m/'1852/'7091/'0/'0 private key` =
      rootKey
        .derive(DerivedKeyIndex.hardened(1852))
        .getOrElse(throw new Error("invalid child key"))
        .derive(DerivedKeyIndex.hardened(7091))
        .getOrElse(throw new Error("invalid child key"))
        .derive(DerivedKeyIndex.hardened(0))
        .getOrElse(throw new Error("invalid child key"))
        .derive(DerivedKeyIndex.hardened(0))
        .getOrElse(throw new Error("invalid child key"))

    (
      `actual m/'1852/'7091/'0/'0 private key`.leftKey.toVector ++
      `actual m/'1852/'7091/'0/'0 private key`.rightKey.toVector ++
      `actual m/'1852/'7091/'0/'0 private key`.chainCode.toVector
    ).encodeAsBase16 shouldBe `expected m/'1852/'7091/'0/'0 private key`

    val `actual m/'1852/'7091/'0/'0/0 public key` =
      `actual m/'1852/'7091/'0/'0 private key`.public.derive(DerivedKeyIndex.soft(0))

    (
      `actual m/'1852/'7091/'0/'0/0 public key`.bytes.toVector ++
      `actual m/'1852/'7091/'0/'0/0 public key`.chainCode.toVector
      ).encodeAsBase16 shouldBe `expected m/'1852/'7091/'0/'0/0 public key`
  }

  it should "pass generated test vector #3" in {
    val rootPrivateKeyBytes =
      Base16Data.unsafe(
          "c05377ef282279549898c5a15fe202bc9416c8a26fe81ffe1e19c147c2493549d61547691b72d73947e588ded4967" +
          "688f82db9628be9bb00c5ad16b5dfaf602ac5f419bd575f8ea23fa1a599b103f85e6325bf2d34b018ff6f2b8cf3f915e19c"
        ).value

    val rootPublicKeyBytes =
      Base16Data.unsafe(
          "2b1b2c00e35c9f9c2dec26ce3ba597504d2fc86862b6035b05340aff8a7ebc4bc5f419bd575f8ea23fa1a599b103f85e6" +
          "325bf2d34b018ff6f2b8cf3f915e19c"
        ).value

    val expectedKeyPairs = Seq(
      (
        "08d0759cf6f08105738945ea2cd4067f173945173b5fe36a0b5d68c8c84935494585bf3e7b11d687c4d64c73dded58915900dc9bb1" +
        "3f062a9532a8366dfa971adcd9ae5c4ef31efedef6eedad9698a15f811d1004036b66241385081d41643cf",
        "7110b5e86240e51b40faaac78a0b92615fe96aed376cdd07255f08ae7ae9ce62dcd9ae5c4ef31efedef6eedad9698a15f811" +
        "d1004036b66241385081d41643cf",
        DerivedKeyIndex.soft(0)
      ),
      (
        "888ba4d32953090155cbcbd26bbe6c6d65e7463eb21a3ec95f6b1af4c74935496b723c972aa1de225b9e8c8f3746a034f3cf6" +
        "7c51e45c4983968b166764cf26c9216b865f39b127515db9ad5591e7fcb908604b9d5056b8b7ac98cf9bd3058c6",
        "393e6946e843dd3ab9ac314524dec7f822e7776cbe2e084918e71003d0baffbc9216b865f39b127515db9ad5591e7fc" +
        "b908604b9d5056b8b7ac98cf9bd3058c6",
        DerivedKeyIndex.soft(1)
      ),
      (
        "c0b712f4c0e2df68d0054112efb081a7fdf8a3ca920994bf555c40e4c249354993f774ae91005da8c69b2c4c59fa80d741ecea" +
        "6722262a6b4576d259cf60ef30c05763f0b510942627d0c8b414358841a19748ec43e1135d2f0c4d81583188e1",
        "906d68169c8bbfc3f0cd901461c4c824e9ab7cdbaf38b7b6bd66e54da0411109c05763f0b510942627d0c8b414358841a19748" +
        "ec43e1135d2f0c4d81583188e1",
        DerivedKeyIndex.soft(2)
      )
    )

    val rootPrivateKey =
      ExtendedPrivateKeyEd25519(
        SizedByteCollection[ByteVector32].fit(rootPrivateKeyBytes.slice(0, 32), ByteOrdering.LittleEndian),
        SizedByteCollection[ByteVector32].fit(rootPrivateKeyBytes.slice(32, 64), ByteOrdering.LittleEndian),
        SizedByteCollection[ByteVector32].fit(rootPrivateKeyBytes.slice(64, 96), ByteOrdering.LittleEndian),
        Seq()
      )

    val rootPublicKey =
      ExtendedPublicKeyEd25519(
        SizedByteCollection[ByteVector32].fit(rootPublicKeyBytes.slice(0, 32), ByteOrdering.LittleEndian),
        SizedByteCollection[ByteVector32].fit(rootPublicKeyBytes.slice(32, 64), ByteOrdering.LittleEndian)
      )

    expectedKeyPairs.foreach { case (expectedXprv, expectedXpub, idx) =>
      val xprvResult = rootPrivateKey.derive(idx).getOrElse(throw new Error("invalid child key"))

      (xprvResult.leftKey.toVector ++ xprvResult.rightKey.toVector ++ xprvResult.chainCode.toVector).encodeAsBase16
        .shouldBe(Base16Data.unsafe(expectedXprv))

      val xpubResult = rootPublicKey.derive(idx)

      (xpubResult.bytes.toVector ++ xpubResult.chainCode.toVector).encodeAsBase16
        .shouldBe(Base16Data.unsafe(expectedXpub))
    }

  }
}
