package co.topl.utils

import co.topl.attestation.keyManagement.{KeyRing, KeyfileCurve25519, KeyfileCurve25519Companion, PrivateKeyCurve25519}
import co.topl.consensus.genesis.PrivateGenesis
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.nodeView.history.History
import co.topl.nodeView.state.State
import co.topl.settings.AppSettings

import scala.util.{Failure, Success}

trait GenesisGenerators extends CoreGenerators with FileUtils {

  val keyRing: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519] =
    KeyRing.empty(settings.application.keyFileDir)

  keyRing.generateNewKeyPairs(num = 3) match {
    case Success(_)     => ()
    case Failure(error) => throw error
  }

  val genesisBlock: Block = PrivateGenesis(keyRing.addresses, settings).getGenesisBlock.get._1

  val genesisBlockId: ModifierId = genesisBlock.id

  def genesisState(settings: AppSettings, genesisBlockWithVersion: Block = genesisBlock): State = {
    History.readOrGenerate(settings).append(genesisBlock)
    State.genesisState(settings, Seq(genesisBlockWithVersion))
  }
}
