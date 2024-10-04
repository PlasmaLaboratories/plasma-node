package xyz.stratalab

import com.google.protobuf.ByteString
import io.estatico.newtype.macros.{newsubtype, newtype}
import xyz.stratalab.models.utility.{Lengths, Sized}

import scala.language.implicitConversions

package object models {

  type Bytes = ByteString
  type Eta = Sized.Strict[Bytes, Eta.Length]

  object Eta {
    type Length = Lengths.`32`.type
  }

  type TypePrefix = Byte
  type Timestamp = Long
  type Slot = Long
  type Epoch = Long

  @newsubtype case class NetworkPrefix(value: Byte)

  type TxRoot = Sized.Strict[Bytes, Lengths.`32`.type]
  type BloomFilter = Sized.Strict[Bytes, Lengths.`256`.type]
  @newtype case class Rho(sizedBytes: Sized.Strict[Bytes, Lengths.`64`.type])
  @newtype case class RhoTestHash(sizedBytes: Sized.Strict[Bytes, Lengths.`64`.type])
  @newtype case class RhoNonceHash(sizedBytes: Sized.Strict[Bytes, Lengths.`64`.type])
  type Digest32 = Sized.Strict[Bytes, Lengths.`32`.type]

  type ProposalId = Int
  val emptyProposal = 0

  type VersionId = Int
  val emptyVersion = 0

  // how many epoch shall pass before proposal became active
  val proposalDelta = 2

  case class ProposalConfig(
    proposalVotingMaxWindow: Int = 5,
    proposalVotingWindow:    Int = 2,
    // How many epochs shall pass before we could reuse proposal id
    proposalInactiveVotingWindow: Int = 1,
    updateProposalPercentage:     Double = 0.1,
    versionVotingWindow:          Int = 2,
    versionSwitchWindow:          Int = 2,
    updateVersionPercentage:      Double = 0.9 // Shall be more than 50%
  )
}
