package org.plasmalabs.mpt

import cats.effect.IO
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.plasmalabs.algebras.testInterpreters.NoOpLogger
import org.typelevel.log4cats.Logger
import org.web3j.rlp.RlpType

class MPTrieSpec extends CatsEffectSuite with ScalaCheckEffectSuite {

  type F[A] = IO[A]

  type SpecKey = Array[Byte]

  type SpecValue = Array[Byte]

  given RLPPersistable[SpecKey] with {

    override def decode(bytes: RlpType): SpecKey = ???

    def encode(t: SpecKey): RlpType = ???
  }

  implicit val logger: Logger[F] = new NoOpLogger[F]

  // ResourceFunFixture[Path](Resource.make(Files[F].createTempDirectory)(Files[F].deleteRecursively))
  //   .test("Get root of empty") { testPath =>
  //     for {
  //       underTest <- MPTrie.makeContainer[F, SpecKey, SpecValue](testPath)
  //     } yield ()
  //   }

}
