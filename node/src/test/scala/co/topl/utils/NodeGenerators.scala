package co.topl.utils

import co.topl.attestation.keyManagement._
import co.topl.attestation.{Address, Proposition, PublicKeyPropositionCurve25519, PublicKeyPropositionEd25519}
import co.topl.consensus.genesis.TestGenesis
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.box.Box.identifier
import co.topl.modifier.box._
import co.topl.modifier.transaction.Transaction.TX
import co.topl.modifier.transaction.{ArbitTransfer, AssetTransfer, PolyTransfer, Transaction}
import co.topl.nodeView.history.{BlockProcessor, History, Storage}
import co.topl.nodeView.state.State
import co.topl.settings.{AppSettings, StartupOpts, Version}
import io.iohk.iodb.LSMStore
import org.scalacheck.Gen
import org.scalatest.Suite

import java.io.File
import java.nio.file.Files
import scala.util.Random

trait NodeGenerators extends CommonGenerators with KeyFileTestHelper {
  self: Suite =>

  private val settingsFilename = "node/src/test/resources/test.conf"

  lazy val settings: AppSettings = {
    val s = AppSettings.read(StartupOpts(Some(settingsFilename)))._1
    s.copy(
      application = s.application.copy(
        dataDir = Some(Files.createTempDirectory("bifrost-test-data").toString)
      )
    )
  }

  lazy val versionGen: Gen[Version] = for {
    first  <- Gen.choose(0: Byte, Byte.MaxValue)
    second <- Gen.choose(0: Byte, Byte.MaxValue)
    third  <- Gen.choose(0: Byte, Byte.MaxValue)
  } yield new Version(first, second, third)

  lazy val genesisBlock: Block =
    TestGenesis(keyRingCurve25519.addresses, keyRingEd25519.addresses, settings).getGenesisBlock.get._1

  def genesisBlockId: ModifierId = genesisBlock.id

  def generateHistory(genesisBlock: Block = genesisBlock): History = {
    val dataDir = s"/tmp/bifrost/test-data/test-${Random.nextInt(10000000)}"

    val iFile = new File(s"$dataDir/blocks")
    iFile.mkdirs()
    val blockStorage = new LSMStore(iFile)

    val storage = new Storage(blockStorage, settings.application.cacheExpire, settings.application.cacheSize)
    //we don't care about validation here
    val validators = Seq()

    var history = new History(storage, BlockProcessor(1024), validators)

    history = history.append(genesisBlock).get._1
    assert(history.modifierById(genesisBlock.id).isDefined)
    history
  }

  def genesisState(settings: AppSettings, genesisBlockWithVersion: Block = genesisBlock): State = {
    History.readOrGenerate(settings).append(genesisBlock)
    State.genesisState(settings, Seq(genesisBlockWithVersion))
  }

  lazy val genesisState: State = genesisState(settings)

  lazy val validBifrostTransactionSeqGen: Gen[Seq[TX]] = for {
    seqLen <- positiveMediumIntGen
  } yield 0 until seqLen map { _ =>
    val g: Gen[TX] = sampleUntilNonEmpty(Gen.oneOf(???))
    sampleUntilNonEmpty(g)
  }

  //TODO: Jing - these three are not used anywhere
//  lazy val validPolyTransferGen: Gen[PolyTransfer[_]] = for {
//    from        <- fromSeqGen
//    to          <- toSeqGen
//    attestation <- attestationCurve25519Gen
//    key         <- publicKeyPropositionCurve25519Gen
//    fee         <- positiveLongGen
//    timestamp   <- positiveLongGen
//    data        <- stringGen
//  } yield {
//
//    val tx = PolyTransfer(from, to, attestation, fee, timestamp, Some(data), minting = false)
//    val sig = key._1.sign(tx.messageToSign)
//    tx.copy(attestation = Map(key._2 -> sig))
//  }
//
//  lazy val validArbitTransferGen: Gen[ArbitTransfer[_]] = for {
//    from        <- fromSeqGen
//    to          <- toSeqGen
//    attestation <- attestationCurve25519Gen
//    fee         <- positiveLongGen
//    timestamp   <- positiveLongGen
//    data        <- stringGen
//  } yield ArbitTransfer(from, to, attestation, fee, timestamp, Some(data), minting = false)
//
//  lazy val validAssetTransferGen: Gen[AssetTransfer[_]] = for {
//    from        <- fromSeqGen
//    to          <- assetToSeqGen
//    attestation <- attestationCurve25519Gen
//    fee         <- positiveLongGen
//    timestamp   <- positiveLongGen
//    data        <- stringGen
//  } yield AssetTransfer(from, to, attestation, fee, timestamp, Some(data), minting = true)

//  def genesisState(settings: AppSettings, genesisBlockWithVersion: Block = genesisBlock): State = {
//    History.readOrGenerate(settings).append(genesisBlock)
//    State.genesisState(settings, Seq(genesisBlockWithVersion))
//  }

  def validPolyTransferCurve25519Gen(
    keyRing: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519],
    state:   State,
    fee:     Long = 1L
  ): Gen[PolyTransfer[PublicKeyPropositionCurve25519]] = {

    val availablePolys = sumBoxes(collectBoxes(keyRing.addresses, state), "PolyBox")
    val (sender, poly) = availablePolys(Random.nextInt(availablePolys.length))
    val polyAmount = SimpleValue(Int128(sampleUntilNonEmpty(Gen.chooseNum(1L + fee, poly.longValue() - 1))) - fee)

    val recipients = {
      val address: Address = keyRing.addresses.filterNot(_ == sender).toSeq(Random.nextInt(keyRing.addresses.size - 1))
      IndexedSeq((address, polyAmount))
    }
    val rawTx = PolyTransfer
      .createRaw[PublicKeyPropositionCurve25519](
        state,
        recipients,
        IndexedSeq(sender),
        changeAddress = sender,
        fee,
        data = None
      )
      .get

    rawTx.copy(attestation = Transaction.updateAttestation(rawTx)(keyRing.generateAttestation(sender)))
  }

  def validPolyTransferEd25519Gen(
    keyRing: KeyRing[PrivateKeyEd25519, KeyfileEd25519],
    state:   State,
    fee:     Long = 1L
  ): Gen[PolyTransfer[PublicKeyPropositionEd25519]] = {

    val availablePolys = sumBoxes(collectBoxes(keyRing.addresses, state), "PolyBox")
    val (sender, poly) = availablePolys(Random.nextInt(availablePolys.length))
    val polyAmount = SimpleValue(Int128(sampleUntilNonEmpty(Gen.chooseNum(1L + fee, poly.longValue() - 1))) - fee)

    val recipients = {
      val address: Address = keyRing.addresses.filterNot(_ == sender).toSeq(Random.nextInt(keyRing.addresses.size - 1))
      IndexedSeq((address, polyAmount))
    }
    val rawTx = PolyTransfer
      .createRaw[PublicKeyPropositionEd25519](
        state,
        recipients,
        IndexedSeq(sender),
        changeAddress = sender,
        fee,
        data = None
      )
      .get

    rawTx.copy(attestation = Transaction.updateAttestation(rawTx)(keyRing.generateAttestation(sender)))
  }

  def validPolyTransferGen(
    keyRingCurve25519: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519],
    keyRingEd25519:    KeyRing[PrivateKeyEd25519, KeyfileEd25519],
    state:             State,
    fee:               Long = 1L
  ): Gen[PolyTransfer[_ <: Proposition]] =
    Gen.oneOf(
      validPolyTransferCurve25519Gen(keyRingCurve25519, state, fee),
      validPolyTransferEd25519Gen(keyRingEd25519, state, fee)
    )

  def validArbitTransferCurve25519Gen(
    keyRing: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519],
    state:   State,
    fee:     Long = 1L
  ): Gen[ArbitTransfer[PublicKeyPropositionCurve25519]] = {

    val availableArbits = sumBoxes(collectBoxes(keyRing.addresses, state), "ArbitBox")
    val (sender, arbit) = availableArbits(Random.nextInt(availableArbits.length))
    val arbitAmount = SimpleValue(Int128(sampleUntilNonEmpty(Gen.chooseNum(1L + fee, arbit.longValue() - 1))) - fee)

    val recipients = {
      val address = keyRing.addresses.filterNot(_ == sender).toSeq(Random.nextInt(keyRing.addresses.size - 1))
      IndexedSeq((address, arbitAmount))
    }
    val rawTx = ArbitTransfer
      .createRaw[PublicKeyPropositionCurve25519](
        state,
        recipients,
        IndexedSeq(sender),
        changeAddress = sender,
        consolidationAddress = sender,
        fee,
        data = None
      )
      .get

    rawTx.copy(attestation = Transaction.updateAttestation(rawTx)(keyRing.generateAttestation(sender)))
  }

  def validArbitTransferEd25519Gen(
    keyRing: KeyRing[PrivateKeyEd25519, KeyfileEd25519],
    state:   State,
    fee:     Long = 1L
  ): Gen[ArbitTransfer[PublicKeyPropositionEd25519]] = {

    val availableArbits = sumBoxes(collectBoxes(keyRing.addresses, state), "ArbitBox")
    val (sender, arbit) = availableArbits(Random.nextInt(availableArbits.length))
    val arbitAmount = SimpleValue(Int128(sampleUntilNonEmpty(Gen.chooseNum(1L + fee, arbit.longValue() - 1))) - fee)

    val recipients = {
      val address = keyRing.addresses.filterNot(_ == sender).toSeq(Random.nextInt(keyRing.addresses.size - 1))
      IndexedSeq((address, arbitAmount))
    }
    val rawTx = ArbitTransfer
      .createRaw[PublicKeyPropositionEd25519](
        state,
        recipients,
        IndexedSeq(sender),
        changeAddress = sender,
        consolidationAddress = sender,
        fee,
        data = None
      )
      .get

    rawTx.copy(attestation = Transaction.updateAttestation(rawTx)(keyRing.generateAttestation(sender)))
  }

  def validArbitTransferGen(
    keyRingCurve25519: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519],
    keyRingEd25519:    KeyRing[PrivateKeyEd25519, KeyfileEd25519],
    state:             State,
    fee:               Long = 1L
  ): Gen[ArbitTransfer[_ <: Proposition]] =
    Gen.oneOf(
      validArbitTransferCurve25519Gen(keyRingCurve25519, state, fee),
      validArbitTransferEd25519Gen(keyRingEd25519, state, fee)
    )

  def validAssetTransferCurve25519Gen(
    keyRing: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519],
    state:   State,
    fee:     Long = 1L,
    minting: Boolean = false
  ): Gen[AssetTransfer[PublicKeyPropositionCurve25519]] = {
    val sender = keyRing.addresses.head
    val asset = AssetValue(1, AssetCode(1: Byte, sender, "test"), SecurityRoot.empty)
    val recipients = IndexedSeq((sender, asset))

    // todo: This should not be using the create raw function because we are testing too many things then!
    val rawTx = AssetTransfer
      .createRaw[PublicKeyPropositionCurve25519](
        state,
        recipients,
        IndexedSeq(sender),
        changeAddress = sender,
        consolidationAddress = sender,
        fee,
        data = None,
        minting
      )
      .get

    rawTx.copy(attestation = Transaction.updateAttestation(rawTx)(keyRing.generateAttestation(sender)))
  }

  def validAssetTransferEd25519Gen(
    keyRing: KeyRing[PrivateKeyEd25519, KeyfileEd25519],
    state:   State,
    fee:     Long = 1L,
    minting: Boolean = false
  ): Gen[AssetTransfer[PublicKeyPropositionEd25519]] = {
    val sender = keyRing.addresses.head
    val asset = AssetValue(1, AssetCode(1: Byte, sender, "test"), SecurityRoot.empty)
    val recipients = IndexedSeq((sender, asset))

    // todo: This should not be using the create raw function because we are testing too many things then!
    val rawTx = AssetTransfer
      .createRaw[PublicKeyPropositionEd25519](
        state,
        recipients,
        IndexedSeq(sender),
        changeAddress = sender,
        consolidationAddress = sender,
        fee,
        data = None,
        minting
      )
      .get

    rawTx.copy(attestation = Transaction.updateAttestation(rawTx)(keyRing.generateAttestation(sender)))
  }

  def validAssetTransferGen(
    keyRingCurve25519: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519],
    keyRingEd25519:    KeyRing[PrivateKeyEd25519, KeyfileEd25519],
    state:             State,
    fee:               Long = 1L,
    minting: Boolean = false
  ): Gen[AssetTransfer[_ <: Proposition]] =
    Gen.oneOf(
      validAssetTransferCurve25519Gen(keyRingCurve25519, state, fee, minting),
      validAssetTransferEd25519Gen(keyRingEd25519, state, fee, minting)
    )

  def collectBoxes(addresses: Set[Address], state: State): Seq[TokenBox[TokenValueHolder]] =
    addresses.flatMap(address => state.getTokenBoxes(address)).flatten.toSeq

  def sumBoxes(boxes: Seq[TokenBox[TokenValueHolder]], tokenType: String): Seq[(Address, Int128)] = {
    val boxesByOwner = boxes.groupBy(_.evidence)
    val ownerQuantities = boxesByOwner.map { case (evidence, boxes) =>
      Address(evidence) -> boxes
        .filter(identifier(_).typeString == tokenType)
        .map(_.value.quantity)
        .sum
    }.toSeq
    ownerQuantities.filter(_._2 > 0)
  }

}
