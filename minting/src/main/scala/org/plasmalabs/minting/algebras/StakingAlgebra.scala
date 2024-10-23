package org.plasmalabs.minting.algebras

import org.plasmalabs.consensus.models.{BlockHeader, SlotId, StakingAddress}
import org.plasmalabs.minting.models.VrfHit
import org.plasmalabs.models._
import org.plasmalabs.sdk.models.LockAddress

/**
 * Staking means participating in the blockchain network.  A staker uses their funds to provide elgibility certificates.
 * A staker also certifies/signs newly minted blocks.
 */
trait StakingAlgebra[F[_]] {

  /**
   * The staker's staking (account) address, usually a verification key
   */
  def address: F[StakingAddress]

  /**
   * A Lock Address, owned by the Staker, to which rewarded funds should be sent
   */
  def rewardAddress: F[LockAddress]

  /**
   * Attempt to form a VrfHit for the given parent+slot combination
   * @param parentSlotId The slotId of the parent block
   * @param slot The current/test/global slot
   * @return Some(hit) if eligible, None if ineligible
   */
  def elect(parentSlotId: SlotId, slot: Slot): F[Option[VrfHit]]

  /**
   * Constructs a (partial) operational certificate, applies it to the given block builder function, and forms
   * the final proof/signature of the BlockHeader
   * @param parentSlotId The parent of the new block
   * @param slot The slot of the new block
   * @param unsignedBlockBuilder a function that constructs an unsigned BlockHeader from the given
   *                             partial operational certificate
   * @return Some(BlockHeader) in most cases.  But in certain edge cases such as a node restart, the staker may be
   *         temporarily unable to certify blocks.
   */
  def certifyBlock(
    parentSlotId:         SlotId,
    slot:                 Slot,
    unsignedBlockBuilder: UnsignedBlockHeader.PartialOperationalCertificate => UnsignedBlockHeader,
    eta:                  Eta
  ): F[Option[BlockHeader]]
}
