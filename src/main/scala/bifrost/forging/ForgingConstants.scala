package bifrost.forging

trait ForgingConstants {

  lazy val MaxTarget = BigInt(1, Array.fill(32)(Byte.MinValue))
  lazy val Difficulty = BigInt("50")

  lazy val GenesisParentId: Array[Byte] = Array.fill(32)(1: Byte)
}
