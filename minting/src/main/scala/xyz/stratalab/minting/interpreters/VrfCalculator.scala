package xyz.stratalab.minting.interpreters

import cats.effect._
import cats.effect.implicits.effectResourceOps
import cats.implicits._
import co.topl.crypto.signing.Ed25519VRF
import com.github.benmanes.caffeine.cache.Caffeine
import com.google.protobuf.ByteString
import scalacache.Entry
import scalacache.caffeine.CaffeineCache
import xyz.stratalab.codecs.bytes.typeclasses.implicits._
import xyz.stratalab.consensus.models.VrfArgument
import xyz.stratalab.minting.algebras.VrfCalculatorAlgebra
import xyz.stratalab.models._
import xyz.stratalab.models.utility.HasLength.instances._
import xyz.stratalab.models.utility._

object VrfCalculator {

  private def caffeineCacheBuilder(vrfCacheSize: Long) = Caffeine.newBuilder.maximumSize(vrfCacheSize)

  def make[F[_]: Sync](
    skVrf:              ByteString,
    ed25519VRFResource: Resource[F, Ed25519VRF],
    vrfCacheSize:       Long
  ): Resource[F, VrfCalculatorAlgebra[F]] =
    for {
      vrfProofsCache <- Sync[F]
        .delay(
          CaffeineCache(caffeineCacheBuilder(vrfCacheSize).build[(Bytes, Long), Entry[ByteString]]())
        )
        .toResource
      rhosCache <-
        Sync[F]
          .delay(
            CaffeineCache(caffeineCacheBuilder(vrfCacheSize).build[(Bytes, Long), Entry[Rho]]())
          )
          .toResource
      impl <- Resource.pure(
        new Impl[F](
          skVrf,
          ed25519VRFResource,
          vrfProofsCache,
          rhosCache
        )
      )
    } yield impl

  private class Impl[F[_]: Sync](
    skVrf:              ByteString,
    ed25519VRFResource: Resource[F, Ed25519VRF],
    vrfProofsCache:     CaffeineCache[F, (Bytes, Long), ByteString],
    rhosCache:          CaffeineCache[F, (Bytes, Long), Rho]
  ) extends VrfCalculatorAlgebra[F] {

    def proofForSlot(slot: Slot, eta: Eta): F[ByteString] =
      vrfProofsCache.cachingF((eta.data, slot))(ttl = None)(
        ed25519VRFResource.use(compute(VrfArgument(eta, slot), _))
      )

    def rhoForSlot(slot: Slot, eta: Eta): F[Rho] =
      rhosCache.cachingF((eta.data, slot))(ttl = None)(
        for {
          proof <- proofForSlot(slot, eta)
          proofHashBytes <- ed25519VRFResource.use(ed25519VRF =>
            Sync[F].delay(ed25519VRF.proofToHash(proof.toByteArray))
          )
          rho = Rho(Sized.strictUnsafe(ByteString.copyFrom(proofHashBytes)))
        } yield rho
      )

    private def compute(
      arg:        VrfArgument,
      ed25519VRF: Ed25519VRF
    ): F[ByteString] =
      Sync[F].delay(
        ByteString.copyFrom(
          ed25519VRF
            .sign(
              skVrf.toByteArray,
              arg.signableBytes.toByteArray
            )
        )
      )
  }
}
