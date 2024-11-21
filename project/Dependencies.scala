import sbt._

object Dependencies {

  val circeVersion = "0.14.10"
  val kamonVersion = "2.7.5"
  val catsCoreVersion = "2.12.0"
  val catsEffectVersion = "3.5.5"
  val fs2Version = "3.11.0"
  val logback = "1.5.12"
  val orientDbVersion = "3.2.35"
  val ioGrpcVersion = "1.68.1"
  val http4sVersion = "0.23.29"
  val protobufSpecsVersion = "0.1.4"
  val plasmaSdkVersion = "0.2.1"

  val catsSlf4j =
    "org.typelevel" %% "log4cats-slf4j" % "2.7.0"

  val logging: Seq[ModuleID] = Seq(
    "com.typesafe.scala-logging" %% "scala-logging"   % "3.9.5",
    "ch.qos.logback"              % "logback-classic" % logback,
    "ch.qos.logback"              % "logback-core"    % logback,
    "org.slf4j"                   % "slf4j-api"       % "2.0.16",
    catsSlf4j
  )

  val scalamockBase = "org.scalamock" %% "scalamock" % "6.0.0"
  val scalamock = scalamockBase        % Test

  private val mUnitTestBase: Seq[ModuleID] = Seq(
    "org.scalameta" %% "munit"                   % "1.0.2",
    "org.scalameta" %% "munit-scalacheck"        % "1.0.0",
    "org.typelevel" %% "munit-cats-effect"       % "2.0.0",
    "org.typelevel" %% "scalacheck-effect-munit" % "2.0-9366e44",
    scalamockBase
  )

  val mUnitTest: Seq[ModuleID] = mUnitTestBase.map(_ % Test)

  val dockerClient = "com.spotify" % "docker-client" % "8.16.0"

  val circe: Seq[ModuleID] = Seq(
    "io.circe" %% "circe-core"    % circeVersion,
    "io.circe" %% "circe-parser"  % circeVersion,
    "io.circe" %% "circe-generic" % circeVersion
  )

  val monitoring: Seq[ModuleID] = Seq(
    "io.kamon" %% "kamon-system-metrics" % kamonVersion,
    "io.kamon" %% "kamon-cats-io-3"      % kamonVersion,
    "io.kamon" %% "kamon-prometheus"     % kamonVersion
  )

  val cats: Seq[ModuleID] = Seq(
    "org.typelevel" %% "cats-core" % catsCoreVersion
  )

  val catsEffect: Seq[ModuleID] = Seq(
    "org.typelevel" %% "cats-effect" % catsEffectVersion
  )

  val scalacache: Seq[ModuleID] = Seq(
    "com.github.cb372" %% "scalacache-caffeine" % "1.0.0-M6"
  )

  val externalCrypto: Seq[ModuleID] = Seq(
    "org.bouncycastle" % "bcprov-jdk18on" % "1.79"
  )

  val levelDb: Seq[ModuleID] = Seq(
    "io.github.tronprotocol" % "leveldbjni-all" % "1.23.2",
    "org.iq80.leveldb"       % "leveldb"        % "0.12"
  )

  val scodec = Seq(
    "org.scodec" %% "scodec-core" % "2.3.2",
    "org.scodec" %% "scodec-bits" % "1.2.1",
    "org.scodec" %% "scodec-cats" % "1.2.0"
  )

  val geny213ExlusionRule = ExclusionRule("com.lihaoyi", "geny_2.13")

  val mainargs = Seq(
    "com.lihaoyi" %% "mainargs" % "0.7.6"
  )

  val fastparse = "com.lihaoyi" %% "fastparse" % "3.1.0"

  val monocle: Seq[ModuleID] = Seq(
    "dev.optics" %% "monocle-core"  % "3.3.0",
    "dev.optics" %% "monocle-macro" % "3.3.0"
  )

  val fs2Core = "co.fs2"                          %% "fs2-core"                  % fs2Version
  val fs2IO = "co.fs2"                            %% "fs2-io"                    % fs2Version
  val fs2ReactiveStreams = "co.fs2"               %% "fs2-reactive-streams"      % fs2Version
  val pureConfig = "com.github.pureconfig"        %% "pureconfig-core"           % "0.17.8"
  val pureConfigGeneric = "com.github.pureconfig" %% "pureconfig-generic-scala3" % "0.17.8"
  val circeYaml = "io.circe"                      %% "circe-yaml"                % "1.15.0"
  val kubernetes = "io.kubernetes"                 % "client-java"               % "22.0.0"

  val http4s = Seq(
    "org.http4s" %% "http4s-ember-client" % http4sVersion,
    "org.http4s" %% "http4s-dsl"          % http4sVersion,
    "org.http4s" %% "http4s-circe"        % http4sVersion
  )

  val http4sServer = http4s ++ Seq(
    "org.http4s" %% "http4s-ember-server" % http4sVersion
  )

  val plasmaScCrypto = "org.plasmalabs" %% "crypto"     % plasmaSdkVersion
  val plasmaScSdk = "org.plasmalabs"    %% "plasma-sdk" % plasmaSdkVersion
  val quivr4s = "org.plasmalabs"        %% "quivr4s"    % plasmaSdkVersion

  val protobufSpecs: Seq[ModuleID] = Seq(
    "org.plasmalabs" %% "protobuf-fs2" % protobufSpecsVersion
  )

  val ipaddress = "com.github.seancfoley" % "ipaddress" % "5.5.1"

  val apacheCommonLang = "org.apache.commons" % "commons-lang3" % "3.17.0"

  // For NTP-UDP
  val commonsNet = "commons-net" % "commons-net" % "3.11.1"

  val catsAll: Seq[ModuleID] = cats ++ catsEffect ++ Seq(catsSlf4j)
  val fs2All: Seq[ModuleID] = catsAll ++ Seq(fs2Core, fs2IO)

  val grpcServices = "io.grpc" % "grpc-services" % ioGrpcVersion

  val jacksonScala = "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.18.1"

  val node: Seq[ModuleID] =
    Seq(
      catsSlf4j,
      fs2Core,
      fs2IO
    ) ++
    cats ++
    catsEffect ++
    mainargs ++
    logging ++
    monocle ++
    monitoring ++
    mUnitTestBase ++
    Seq(grpcServices) ++
    http4s

  val nodeIt =
    http4sServer.map(_ % Test)

  val networkDelayer: Seq[ModuleID] =
    cats ++ catsEffect ++ mainargs ++ logging ++ Seq(
      catsSlf4j,
      fs2Core,
      fs2IO,
      pureConfig,
      pureConfigGeneric
    )

  val testnetSimulationOrchestator: Seq[ModuleID] =
    cats ++ catsEffect ++ mainargs ++ logging ++ Seq(
      catsSlf4j,
      fs2Core,
      fs2IO,
      pureConfig,
      pureConfigGeneric,
      kubernetes,
      "com.google.cloud" % "google-cloud-storage" % "2.45.0"
    )

  lazy val actor: Seq[sbt.ModuleID] = fs2All

  lazy val algebras: Seq[sbt.ModuleID] =
    circe ++
    protobufSpecs ++
    munitScalamock ++
    catsEffect.map(_ % Test) ++
    Seq(catsSlf4j % Test)

  val commonApplication: Seq[ModuleID] =
    cats ++ catsEffect ++ mainargs ++ logging ++ monocle ++
    http4s ++ Seq(
      catsSlf4j,
      pureConfig,
      pureConfigGeneric,
      circeYaml
    )

  lazy val crypto: Seq[ModuleID] =
    scodec ++
    externalCrypto ++
    cats ++
    mUnitTest ++
    Seq(plasmaScCrypto, plasmaScCrypto.classifier("tests") % Test) ++
    circe.map(_ % Test)

  lazy val eventTree: Seq[ModuleID] =
    Dependencies.mUnitTest ++ Dependencies.catsEffect

  lazy val catsUtils: Seq[ModuleID] =
    cats ++ catsEffect ++ logging ++ Seq(fs2Core, fs2IO, fs2ReactiveStreams) ++ mUnitTest

  lazy val models: Seq[ModuleID] =
    cats ++ scodec ++ protobufSpecs ++
    Seq(plasmaScSdk, plasmaScSdk.classifier("tests") % Test) ++
    Seq(quivr4s, quivr4s.classifier("tests") % Test) ++
    Seq(jacksonScala)

  lazy val consensus: Seq[ModuleID] =
    Dependencies.mUnitTest ++ externalCrypto ++ catsEffect ++ logging ++ scalacache

  lazy val minting: Seq[ModuleID] =
    Dependencies.mUnitTest ++ Dependencies.catsEffect

  lazy val networking: Seq[ModuleID] =
    Dependencies.mUnitTest ++ Dependencies.catsEffect ++ Seq(ipaddress, apacheCommonLang)

  lazy val transactionGenerator: Seq[ModuleID] =
    Dependencies.mUnitTest ++ Dependencies.catsEffect ++ Seq(Dependencies.fs2Core)

  lazy val ledger: Seq[ModuleID] =
    Dependencies.mUnitTest ++ Dependencies.catsEffect ++ Dependencies.protobufSpecs ++ scalacache ++
    Seq(Dependencies.plasmaScSdk, Dependencies.plasmaScSdk.classifier("tests") % Test)

  lazy val blockchain: Seq[ModuleID] =
    Dependencies.mUnitTest ++ Dependencies.catsEffect ++ logging ++ Seq(fs2Core)

  lazy val commonInterpreters: Seq[sbt.ModuleID] =
    mUnitTest ++
    cats ++
    catsEffect ++
    scalacache ++
    monitoring ++
    Seq(
      commonsNet,
      catsSlf4j % Test
    )

  lazy val byteCodecs: Seq[sbt.ModuleID] =
    munitScalamock ++
    scodec ++
    cats

  lazy val grpc: Seq[ModuleID] =
    cats ++
    catsEffect ++
    mUnitTest ++
    protobufSpecs ++
    Seq(
      "io.grpc" % "grpc-netty-shaded" % ioGrpcVersion,
      grpcServices
    ) ++ http4sServer

  lazy val levelDbStore: Seq[ModuleID] =
    levelDb ++
    cats ++
    catsEffect ++
    mUnitTest ++
    Seq(fs2Core, fs2IO)

  lazy val orientDb: Seq[ModuleID] =
    Seq(
      "com.orientechnologies" % "orientdb-core"    % orientDbVersion,
      "com.orientechnologies" % "orientdb-server"  % orientDbVersion,
      "com.orientechnologies" % "orientdb-tools"   % orientDbVersion,
      "com.orientechnologies" % "orientdb-graphdb" % orientDbVersion exclude (
        "commons-beanutils",
        "commons-beanutils"
      ) exclude ("commons-beanutils", "commons-beanutils-core"),
      "com.googlecode.concurrentlinkedhashmap" % "concurrentlinkedhashmap-lru" % "1.4.2",
      "org.lz4"                                % "lz4-java"                    % "1.8.0"
      // Add jna
    )

  lazy val indexer: Seq[ModuleID] =
    logging ++
    orientDb ++
    mUnitTest

  lazy val munitScalamock: Seq[sbt.ModuleID] =
    mUnitTest

  lazy val byzantineIt: Seq[ModuleID] =
    (mUnitTestBase :+ dockerClient :+ fastparse).map(_ % Test)
}
