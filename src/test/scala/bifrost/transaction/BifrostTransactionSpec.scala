package bifrost.transaction

import bifrost.{BifrostGenerators, ValidGenerators}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}

class BifrostTransactionSpec extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators {

  property("Transaction boxes are deterministic") {
    /*val GenesisAccountsNum = 10
    val GenesisBalance = 100000L

    val icoMembers = (1 to 10) map (i => PublicKey25519Proposition(Sha256(i.toString)))
    icoMembers.map(_.address).mkString(",") shouldBe "3m6nhP4AZjFn5pgMd3PvH6PwHx23AG4tvpLCuu7Wt3hhAPssKc,4ZJwiEzpTHhvT6BMYZg1FUXysHkuBLRHb7FvXhZGx6HtsWZCeG,3Y7Ji8wrYZ12EPup6ky2mWEaNo1wTgUKVPJ84xaHwHqTC6LXoh,3WqPcQ1w1HEaEDvHpnnqqYxJBzQGcf5gT5G5CrsXFL7UX4SA2N,4m5cG82kztD9bZVf1Tc1Ni1uvHobpKYuAUyxNSnDm7WLGCZvZh,4huPANjYcqcdRm99tsCw29JqFnHMTJZsQjoufRQTEDPPoWmPSt,3s3CauhVba81UefEuuaNqRqGLEV9jCZJpvLFg5dJdu29TivRZk,3HHuHxBf2eXmbUcGuFCx3dU6Wp7imeRiN5uz4rYDdQwsLwnwW4,38uZVfModMnCg5FSECtFiBE7Dbjmh7Tt1SgBD8gFTA1XDHxiqQ,3WTH7tB28nkbC9KFJTy8EBn1bWkxryiLKDnngeP9BYyuCik3aP"

    val genesisAccount = PrivateKey25519Companion.generateKeys("genesis".getBytes)._1
    val tx = SimpleBoxTransaction(IndexedSeq(genesisAccount -> 0), icoMembers.map(_ -> GenesisBalance), 0L, 0L)
    tx.newBoxes.toString() shouldBe "Vector(PublicKey25519NoncedBox(3m6nhP4AZjFn5pgMd3PvH6PwHx23AG4tvpLCuu7Wt3hhAPssKc,-6219502975712200872,100000), PublicKey25519NoncedBox(4ZJwiEzpTHhvT6BMYZg1FUXysHkuBLRHb7FvXhZGx6HtsWZCeG,2326174055960855030,100000), PublicKey25519NoncedBox(3Y7Ji8wrYZ12EPup6ky2mWEaNo1wTgUKVPJ84xaHwHqTC6LXoh,-2090466149357841238,100000), PublicKey25519NoncedBox(3WqPcQ1w1HEaEDvHpnnqqYxJBzQGcf5gT5G5CrsXFL7UX4SA2N,-4786344880748433993,100000), PublicKey25519NoncedBox(4m5cG82kztD9bZVf1Tc1Ni1uvHobpKYuAUyxNSnDm7WLGCZvZh,2879476891976400353,100000), PublicKey25519NoncedBox(4huPANjYcqcdRm99tsCw29JqFnHMTJZsQjoufRQTEDPPoWmPSt,4610029492489107892,100000), PublicKey25519NoncedBox(3s3CauhVba81UefEuuaNqRqGLEV9jCZJpvLFg5dJdu29TivRZk,416797087985622128,100000), PublicKey25519NoncedBox(3HHuHxBf2eXmbUcGuFCx3dU6Wp7imeRiN5uz4rYDdQwsLwnwW4,-8485818448745401936,100000), PublicKey25519NoncedBox(38uZVfModMnCg5FSECtFiBE7Dbjmh7Tt1SgBD8gFTA1XDHxiqQ,-4750873086163930339,100000), PublicKey25519NoncedBox(3WTH7tB28nkbC9KFJTy8EBn1bWkxryiLKDnngeP9BYyuCik3aP,1904873933279744536,100000))"
    */
  }
}
