package org.plasmalabs.codecs.bytes.tetra

import cats.{Eq, Show}
import org.plasmalabs.codecs.bytes.CodecSpec
import org.plasmalabs.consensus.models.{BlockId, EligibilityCertificate}
import org.plasmalabs.crypto.models.KesBinaryTree
import org.plasmalabs.models._
import org.plasmalabs.models.utility.Ratio
import org.scalacheck.Gen

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
    org.plasmalabs.crypto.utils.NodeCryptoGenerators.kesBinaryTreeGen
  )

  codecBehavior[org.plasmalabs.crypto.models.SignatureKesSum](
    "org.plasmalabs.crypto.models.SignatureKesSum",
    nodeCryptoSignatureKesSumCodec,
    org.plasmalabs.crypto.utils.NodeCryptoGenerators.signatureKesSumArbitrary.arbitrary
  )

  codecBehavior[org.plasmalabs.crypto.models.SecretKeyKesSum](
    "org.plasmalabs.crypto.models.SecretKeyKesSum",
    nodeCryptoSecretKeyKesSumCodec,
    org.plasmalabs.crypto.utils.NodeCryptoGenerators.kesSumSKGen
  )

  codecBehavior[org.plasmalabs.crypto.models.SecretKeyKesProduct](
    "org.plasmalabs.crypto.models.SecretKeyKesProduct",
    nodeCryptoSecretKeyKesProductCodec,
    org.plasmalabs.crypto.utils.NodeCryptoGenerators.kesProductSKGen
  )

  codecBehavior[BlockId](
    "org.plasmalabs.consensus.models.BlockId",
    TetraScodecCodecs.blockIdCodec,
    org.plasmalabs.models.generators.consensus.ModelGenerators.arbitraryBlockId.arbitrary
  )

  codecBehavior[EligibilityCertificate](
    "org.plasmalabs.consensus.models.EligibilityCertificate",
    TetraScodecCodecs.consensusEligibilityCertificateCodec,
    org.plasmalabs.models.generators.consensus.ModelGenerators.arbitraryEligibilityCertificate.arbitrary
  )

  codecBehavior[org.plasmalabs.consensus.models.VerificationKeyKesProduct](
    "org.plasmalabs.consensus.models.VerificationKeyKesProduct",
    TetraScodecCodecs.vkKesProductCodec,
    org.plasmalabs.models.generators.consensus.ModelGenerators.arbitraryVerificationKeyKesProduct.arbitrary
  )

  codecBehavior[org.plasmalabs.consensus.models.SignatureKesSum](
    "org.plasmalabs.consensus.models.SignatureKesSum",
    TetraScodecCodecs.signatureKesSumCodec,
    org.plasmalabs.models.generators.consensus.ModelGenerators.signatureKesSumArbitrary.arbitrary
  )

  codecBehavior[org.plasmalabs.consensus.models.SignatureKesProduct](
    "org.plasmalabs.consensus.models.SignatureKesProduct",
    TetraScodecCodecs.signatureKesProductCodec,
    org.plasmalabs.models.generators.consensus.ModelGenerators.signatureKesProductArbitrary.arbitrary
  )

  codecBehavior[org.plasmalabs.consensus.models.OperationalCertificate](
    "org.plasmalabs.consensus.models.OperationalCertificate",
    TetraScodecCodecs.operationalCertificateCodec,
    org.plasmalabs.models.generators.consensus.ModelGenerators.arbitraryOperationalCertificate.arbitrary
  )

  codecBehavior[org.plasmalabs.models.UnsignedBlockHeader.PartialOperationalCertificate](
    "org.plasmalabs.models.UnsignedBlockHeader.PartialOperationalCertificate",
    TetraScodecCodecs.partialOperationalCertificateCodec,
    ModelGenerators.partialOperationalCertificateGen
  )

  codecBehavior[org.plasmalabs.consensus.models.BlockHeader](
    "org.plasmalabs.consensus.models.BlockHeader",
    TetraScodecCodecs.consensusBlockHeaderCodec,
    org.plasmalabs.models.generators.consensus.ModelGenerators.headerGen()
  )

  codecBehavior[org.plasmalabs.models.UnsignedBlockHeader](
    "org.plasmalabs.models.UnsignedBlockHeader",
    TetraScodecCodecs.unsignedBlockHeaderCodec,
    ModelGenerators.unsignedHeaderGen()
  )

}
