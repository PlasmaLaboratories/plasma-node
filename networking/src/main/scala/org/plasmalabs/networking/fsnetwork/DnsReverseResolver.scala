package org.plasmalabs.networking.fsnetwork

import cats.data.OptionT
import cats.effect.Sync
import cats.implicits.*
import cats.{Applicative, Monad}
import com.comcast.ip4s.{Dns, IpAddress}
import com.github.benmanes.caffeine.cache.Caffeine
import org.plasmalabs.models.p2p.*
import scalacache.Entry
import scalacache.caffeine.CaffeineCache

import scala.concurrent.duration.{DurationInt, FiniteDuration}

trait ReverseDnsResolver[F[_]] {
  def reverseResolving(host: String): F[String]
}

object ReverseDnsResolverInstances {

  class NoOpReverseResolver[F[_]: Applicative] extends ReverseDnsResolver[F] {
    override def reverseResolving(address: String): F[String] = address.pure[F]
  }

  class DefaultReverseDnsResolver[F[_]: Dns: Sync] extends ReverseDnsResolver[F] {
    private val reverseDnsCacheSize: Int = 1000
    val expireAfterWriteDuration: FiniteDuration = 30.minutes

    val cache: CaffeineCache[F, String, String] =
      CaffeineCache[F, String, String](
        Caffeine.newBuilder
          .maximumSize(reverseDnsCacheSize)
          .build[String, Entry[String]]()
      )

    override def reverseResolving(ip: String): F[String] =
      cache.cachingF(ip)(expireAfterWriteDuration.some)(doResolve(ip))

    private def doResolve(ip: String): F[String] = {
      val resolver: Dns[F] = implicitly[Dns[F]]
      val res =
        for {
          ip       <- OptionT.fromOption[F](IpAddress.fromString(ip))
          resolved <- OptionT(resolver.reverseOption(ip))
        } yield resolved.normalized.toString
      // if we failed to get hostname then still use ip
      res.value.map(_.getOrElse(ip))
    }
  }

  implicit class ReverseDnsResolverSyntax[F[_]](ip: String)(implicit val resolver: ReverseDnsResolver[F]) {
    def reverseResolving(): F[String] = resolver.reverseResolving(ip)
  }
}

trait ReverseDnsResolverHT[T, F[_]] {
  def reverseResolving(host: T): F[T]
}

object ReverseDnsResolverHTInstances {

  implicit def reverseRemoteAddressResolver[F[_]: Monad: ReverseDnsResolver]: ReverseDnsResolverHT[RemoteAddress, F] =
    (resolvedHost: RemoteAddress) => {
      val resolver = implicitly[ReverseDnsResolver[F]]
      resolver.reverseResolving(resolvedHost.host).map(resolved => resolvedHost.copy(host = resolved))
    }

  implicit def reversePeerToAddResolver[F[_]: Monad: ReverseDnsResolver]: ReverseDnsResolverHT[KnownRemotePeer, F] =
    (resolvedHost: KnownRemotePeer) => {
      val resolver = implicitly[ReverseDnsResolverHT[RemoteAddress, F]]
      resolver.reverseResolving(resolvedHost.address).map(resolved => resolvedHost.copy(address = resolved))
    }

  implicit def reverseRemotePeerToAddResolver[F[_]: Monad: ReverseDnsResolver]: ReverseDnsResolverHT[RemotePeer, F] =
    (resolvedHost: RemotePeer) => {
      val resolver = implicitly[ReverseDnsResolverHT[RemoteAddress, F]]
      resolver.reverseResolving(resolvedHost.address).map(resolved => resolvedHost.copy(address = resolved))
    }

  implicit class ReverseDnsResolverHTSyntax[F[_], T](host: T)(implicit res: ReverseDnsResolverHT[T, F]) {

    def reverseResolving(): F[T] = {
      val resolver: ReverseDnsResolverHT[T, F] = implicitly[ReverseDnsResolverHT[T, F]]
      resolver.reverseResolving(host)
    }
  }
}
