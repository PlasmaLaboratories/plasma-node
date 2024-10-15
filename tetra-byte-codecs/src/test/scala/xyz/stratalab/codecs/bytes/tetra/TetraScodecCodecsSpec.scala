package xyz.stratalab.codecs.bytes.tetra

import cats.{Eq, Show}
import org.scalacheck.Gen
import xyz.stratalab.codecs.bytes.CodecSpec
import xyz.stratalab.consensus.models.{BlockId, EligibilityCertificate}
import xyz.stratalab.crypto.models.KesBinaryTree
import xyz.stratalab.models._
import xyz.stratalab.models.utility.Ratio

class TetraScodecCodecsSpec extends CodecSpec {

  import TetraScodecCodecs._

  implicit def defaultShow[T]: Show[T] = Show.fromToString
  implicit def defaultEq[T]: Eq[T] = Eq.fromUniversalEquals

  codecBehavior[BigInt](
    "BigInt",
    TetraScodecCodecs.bigIntCodec,
    Gen.long.map(BigInt(_))
  )

  codecBehavior[Ratio](
    "Ratio",
    TetraScodecCodecs.ratioCodec,
    ModelGenerators.ratioGen
  )

  codecBehavior[KesBinaryTree](
    "KesBinaryTree",
    TetraScodecCodecs.nodeCryptoKesBinaryTreeCodec,
    xyz.stratalab.crypto.utils.NodeCryptoGenerators.kesBinaryTreeGen
  )

  codecBehavior[xyz.stratalab.crypto.models.SignatureKesSum](
    "xyz.stratalab.crypto.models.SignatureKesSum",
    nodeCryptoSignatureKesSumCodec,
    xyz.stratalab.crypto.utils.NodeCryptoGenerators.signatureKesSumArbitrary.arbitrary
  )

  codecBehavior[xyz.stratalab.crypto.models.SecretKeyKesSum](
    "xyz.stratalab.crypto.models.SecretKeyKesSum",
    nodeCryptoSecretKeyKesSumCodec,
    xyz.stratalab.crypto.utils.NodeCryptoGenerators.kesSumSKGen
  )

  codecBehavior[xyz.stratalab.crypto.models.SecretKeyKesProduct](
    "xyz.stratalab.crypto.models.SecretKeyKesProduct",
    nodeCryptoSecretKeyKesProductCodec,
    xyz.stratalab.crypto.utils.NodeCryptoGenerators.kesProductSKGen
  )

  codecBehavior[BlockId](
    "xyz.stratalab.consensus.models.BlockId",
    TetraScodecCodecs.blockIdCodec,
    xyz.stratalab.models.generators.consensus.ModelGenerators.arbitraryBlockId.arbitrary
  )

  codecBehavior[EligibilityCertificate](
    "xyz.stratalab.consensus.models.EligibilityCertificate",
    TetraScodecCodecs.consensusEligibilityCertificateCodec,
    xyz.stratalab.models.generators.consensus.ModelGenerators.arbitraryEligibilityCertificate.arbitrary
  )

  codecBehavior[xyz.stratalab.consensus.models.VerificationKeyKesProduct](
    "xyz.stratalab.consensus.models.VerificationKeyKesProduct",
    TetraScodecCodecs.vkKesProductCodec,
    xyz.stratalab.models.generators.consensus.ModelGenerators.arbitraryVerificationKeyKesProduct.arbitrary
  )

  codecBehavior[xyz.stratalab.consensus.models.SignatureKesSum](
    "xyz.stratalab.consensus.models.SignatureKesSum",
    TetraScodecCodecs.signatureKesSumCodec,
    xyz.stratalab.models.generators.consensus.ModelGenerators.signatureKesSumArbitrary.arbitrary
  )

  codecBehavior[xyz.stratalab.consensus.models.SignatureKesProduct](
    "xyz.stratalab.consensus.models.SignatureKesProduct",
    TetraScodecCodecs.signatureKesProductCodec,
    xyz.stratalab.models.generators.consensus.ModelGenerators.signatureKesProductArbitrary.arbitrary
  )

  codecBehavior[xyz.stratalab.consensus.models.OperationalCertificate](
    "xyz.stratalab.consensus.models.OperationalCertificate",
    TetraScodecCodecs.operationalCertificateCodec,
    xyz.stratalab.models.generators.consensus.ModelGenerators.arbitraryOperationalCertificate.arbitrary
  )

  codecBehavior[xyz.stratalab.models.UnsignedBlockHeader.PartialOperationalCertificate](
    "xyz.stratalab.models.UnsignedBlockHeader.PartialOperationalCertificate",
    TetraScodecCodecs.partialOperationalCertificateCodec,
    ModelGenerators.partialOperationalCertificateGen
  )

  codecBehavior[xyz.stratalab.consensus.models.BlockHeader](
    "xyz.stratalab.consensus.models.BlockHeader",
    TetraScodecCodecs.consensusBlockHeaderCodec,
    xyz.stratalab.models.generators.consensus.ModelGenerators.headerGen()
  )

  codecBehavior[xyz.stratalab.models.UnsignedBlockHeader](
    "xyz.stratalab.models.UnsignedBlockHeader",
    TetraScodecCodecs.unsignedBlockHeaderCodec,
    ModelGenerators.unsignedHeaderGen()
  )

}
