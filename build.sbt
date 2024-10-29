import sbt.Keys.{organization, test}
import sbtassembly.MergeStrategy

val scala3 = "3.4.1"

inThisBuild(
  List(
    organization := "org.plasmalabs",
    scalaVersion := scala3,
    versionScheme := Some("early-semver"),
    dynverSeparator := "-",
    version := dynverGitDescribeOutput.value.mkVersion(versionFmt, fallbackVersion(dynverCurrentDate.value)),
    dynver := {
      val d = new java.util.Date
      sbtdynver.DynVer.getGitDescribeOutput(d).mkVersion(versionFmt, fallbackVersion(d))
    },
    testFrameworks += TestFrameworks.MUnit
  )
)

Global / concurrentRestrictions += Tags.limit(Tags.Test, 1)

enablePlugins(ReproducibleBuildsPlugin, ReproducibleBuildsAssemblyPlugin)

lazy val commonSettings = Seq(
  sonatypeCredentialHost := "s01.oss.sonatype.org",
  scalacOptions ++= commonScalacOptions,
  semanticdbEnabled := true, // enable SemanticDB for Scalafix
  scalaVersion := scala3,
  resolvers ++= Seq(
    "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/",
    "Sonatype Staging" at "https://s01.oss.sonatype.org/content/repositories/staging",
    "Sonatype Snapshots" at "https://s01.oss.sonatype.org/content/repositories/snapshots/",
    "Bintray" at "https://jcenter.bintray.com/",
    "jitpack" at "https://jitpack.io"
  ),
  testFrameworks += TestFrameworks.MUnit,
  dependencyOverrides ++= Dependencies.protobufSpecs ++ Seq(Dependencies.quivr4s)
)

lazy val dockerSettings = Seq(
  dockerBaseImage := "eclipse-temurin:21-jre",
  dockerUpdateLatest := sys.env.get("DOCKER_PUBLISH_LATEST_TAG").fold(false)(_.toBoolean),
  dockerLabels ++= Map(
    "plasma-node.version" -> version.value
  ),
  dockerAliases := dockerAliases.value.flatMap { alias =>
    Seq(
      alias.withRegistryHost(Some("docker.io/stratalab")),
      alias.withRegistryHost(Some("ghcr.io/plasmalaboratories"))
    )
  }
)

lazy val nodeDockerSettings =
  dockerSettings ++ Seq(
    dockerExposedPorts := Seq(9084, 9085),
    Docker / packageName := "plasma-node",
    dockerExposedVolumes += "/node",
    dockerExposedVolumes += "/node-staking",
    dockerEnvVars ++= Map(
      "NODE_APPLICATION_DATA_DIR"    -> "/node/data/{genesisBlockId}",
      "NODE_APPLICATION_STAKING_DIR" -> "/node-staking/{genesisBlockId}",
      "NODE_CONFIG_FILE"             -> "/node/config/user.yaml"
    ),
    dockerAliases ++= (
      if (sys.env.get("DOCKER_PUBLISH_DEV_TAG").fold(false)(_.toBoolean))
        Seq(
          DockerAlias(Some("docker.io"), Some("stratalab"), "plasma-node", Some("dev")),
          DockerAlias(Some("ghcr.io"), Some("plasmalaboratories"), "plasma-node", Some("dev"))
        )
      else Seq()
    )
  )

lazy val indexerDockerSettings =
  dockerSettings ++ Seq(
    dockerExposedPorts := Seq(9084),
    Docker / packageName := "plasma-indexer",
    dockerExposedVolumes += "/indexer",
    dockerAliases ++= (
      if (sys.env.get("DOCKER_PUBLISH_DEV_TAG").fold(false)(_.toBoolean))
        Seq(
          DockerAlias(Some("docker.io"), Some("stratalab"), "plasma-indexer", Some("dev")),
          DockerAlias(Some("ghcr.io"), Some("plasmalaboratories"), "plasma-indexer", Some("dev"))
        )
      else Seq()
    )
  )

lazy val networkDelayerDockerSettings =
  dockerSettings ++ Seq(
    Docker / packageName := "network-delayer"
  )

lazy val testnetSimulationOrchestratorDockerSettings =
  dockerSettings ++ Seq(
    Docker / packageName := "testnet-simulation-orchestrator"
  )

def assemblySettings(main: String) = Seq(
  assembly / mainClass := Some(main),
  assembly / test := {},
  assemblyJarName := s"plasma-node-${version.value}.jar",
  assembly / assemblyMergeStrategy ~= { old: (String => MergeStrategy) =>
    {
      case ps if ps.endsWith(".SF")  => MergeStrategy.discard
      case ps if ps.endsWith(".DSA") => MergeStrategy.discard
      case ps if ps.endsWith(".RSA") => MergeStrategy.discard
      case ps if ps.endsWith(".xml") => MergeStrategy.first
      case PathList(ps @ _*) if ps.last endsWith "module-info.class" =>
        MergeStrategy.discard // https://github.com/sbt/sbt-assembly/issues/370
      case x if x.contains("simulacrum")               => MergeStrategy.last
      case PathList("org", "iq80", "leveldb", xs @ _*) => MergeStrategy.first
      case PathList("module-info.java")                => MergeStrategy.discard
      case PathList("local.conf")                      => MergeStrategy.discard
      case "META-INF/io.netty.versions.properties"     => MergeStrategy.last
      case "META-INF/truffle/instrument"               => MergeStrategy.concat
      case "META-INF/truffle/language"                 => MergeStrategy.rename
      case "META-INF/okio.kotlin_module"               => MergeStrategy.first
      case x if x.contains("google/protobuf")          => MergeStrategy.last
      case x                                           => old(x)
    }
  }
)

lazy val commonScalacOptions = Seq(
  "-deprecation",
  "-feature",
  "-language:higherKinds",
  "-language:postfixOps",
  "-unchecked",
  "-Ykind-projector:underscores",
  "-source:3.4-migration",
  "-Wunused:imports"
)

javaOptions ++= Seq(
  // Force the JVM to exit the first time it encounters an OOM error.  By default, it might not exit.
  "-XX:+ExitOnOutOfMemoryError",
  // Disables the shared memory space for JVM stats, thus preventing external processes from viewing memory/CPU stats.
  // Disabled to prevent a potential security threat
  "-XX:+PerfDisableSharedMem"
)

connectInput / run := true
outputStrategy := Some(StdoutOutput)

def versionFmt(out: sbtdynver.GitDescribeOutput): String = {
  val dirtySuffix = out.dirtySuffix.dropPlus.mkString("-", "")
  if (out.isCleanAfterTag) out.ref.dropPrefix + dirtySuffix // no commit info if clean after tag
  else out.ref.dropPrefix + out.commitSuffix.mkString("-", "-", "") + dirtySuffix
}

def fallbackVersion(d: java.util.Date): String = s"HEAD-${sbtdynver.DynVer timestamp d}"

lazy val plasmaNode = project
  .in(file("."))
  .settings(
    moduleName := "plasmaNode",
    commonSettings,
    publish / skip := true,
    crossScalaVersions := Nil
  )
  .aggregate(
    node,
    typeclasses,
    config,
    grpc,
    nodeCrypto,
    catsUtils,
    models,
    numerics,
    eventTree,
    algebras,
    actor,
    commonInterpreters,
    minting,
    networking,
    byteCodecs,
    tetraByteCodecs,
    consensus,
    ledger,
    blockchainCore,
    blockchain,
    levelDbStore,
    commonApplication,
    networkDelayer,
    indexer,
    transactionGenerator,
    testnetSimulationOrchestrator
  )

lazy val node = project
  .in(file("node"))
  .settings(
    name := "plasma-node",
    commonSettings,
    assemblySettings("org.plasmalabs.node.NodeApp"),
    assemblyJarName := s"plasma-node-${version.value}.jar",
    nodeDockerSettings,
    Compile / mainClass := Some("org.plasmalabs.node.NodeApp"),
    publish / skip := true,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "org.plasmalabs.buildinfo.node",
    libraryDependencies ++= Dependencies.node,
    excludeDependencies += Dependencies.scodec213ExlusionRule
  )
  .settings(
    classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.Flat // required for correct loading https://github.com/kamon-io/sbt-kanela-runner
  )
  .dependsOn(
    models % "compile->compile;test->test",
    config,
    typeclasses,
    consensus,
    minting,
    commonInterpreters,
    networking,
    catsUtils,
    grpc,
    blockchainCore,
    blockchain,
    levelDbStore,
    commonApplication,
    indexer
  )
  .enablePlugins(BuildInfoPlugin, JavaAppPackaging, DockerPlugin)

lazy val config = project
  .in(file("config"))
  .settings(
    name := "config",
    commonSettings,
    libraryDependencies ++= Dependencies.monocle :+ Dependencies.pureConfig,
    excludeDependencies += Dependencies.scodec213ExlusionRule
  )
  .dependsOn(models, numerics)

lazy val networkDelayer = project
  .in(file("network-delayer"))
  .settings(
    name := "network-delayer",
    commonSettings,
    coverageEnabled := false,
    assemblySettings("org.plasmalabs.networkdelayer.NetworkDelayer"),
    assemblyJarName := s"network-delayer-${version.value}.jar",
    networkDelayerDockerSettings,
    Compile / mainClass := Some("org.plasmalabs.networkdelayer.NetworkDelayer"),
    publish / skip := true,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "org.plasmalabs.buildinfo.networkdelayer",
    libraryDependencies ++= Dependencies.networkDelayer,
    excludeDependencies += Dependencies.scodec213ExlusionRule
  )
  .enablePlugins(BuildInfoPlugin, JavaAppPackaging, DockerPlugin)
  .dependsOn(catsUtils, commonApplication)

lazy val testnetSimulationOrchestrator = project
  .in(file("testnet-simulation-orchestrator"))
  .settings(
    name := "testnet-simulation-orchestrator",
    commonSettings,
    coverageEnabled := false,
    assemblySettings("org.plasmalabs.testnetsimulationorchestrator.app.Orchestrator"),
    assemblyJarName := s"testnet-simulation-orchestrator-${version.value}.jar",
    testnetSimulationOrchestratorDockerSettings,
    Compile / mainClass := Some("org.plasmalabs.testnetsimulationorchestrator.app.Orchestrator"),
    publish / skip := true,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "org.plasmalabs.buildinfo.testnetsimulationorchestator",
    libraryDependencies ++= Dependencies.testnetSimulationOrchestator,
    excludeDependencies += Dependencies.scodec213ExlusionRule
  )
  .enablePlugins(BuildInfoPlugin, JavaAppPackaging, DockerPlugin)
  .dependsOn(commonApplication, transactionGenerator)

lazy val commonApplication = project
  .in(file("common-application"))
  .settings(
    name := "common-application",
    commonSettings,
    libraryDependencies ++= Dependencies.commonApplication,
    excludeDependencies += Dependencies.scodec213ExlusionRule
  )
  .dependsOn(catsUtils)

lazy val models = project
  .in(file("models"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "models",
    commonSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "org.plasmalabs.buildinfo.models",
    libraryDependencies ++= Dependencies.models ++ Dependencies.mUnitTest,
    excludeDependencies += Dependencies.scodec213ExlusionRule
  )
  .dependsOn(munitScalamock % "test->test")

lazy val numerics = project
  .in(file("numerics"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "numerics",
    commonSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "org.plasmalabs.buildinfo.numerics",
    libraryDependencies ++= Dependencies.mUnitTest ++ Dependencies.scalacache,
    excludeDependencies += Dependencies.scodec213ExlusionRule
  )
  .dependsOn(models)

lazy val eventTree = project
  .in(file("event-tree"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "event-tree",
    commonSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "org.plasmalabs.buildinfo.eventtree",
    libraryDependencies ++= Dependencies.eventTree,
    excludeDependencies += Dependencies.scodec213ExlusionRule
  )
  .dependsOn(algebras % "compile->test")

lazy val byteCodecs = project
  .in(file("byte-codecs"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "byte-codecs",
    commonSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "org.plasmalabs.buildinfo.codecs.bytes",
    libraryDependencies ++= Dependencies.byteCodecs ++ Dependencies.protobufSpecs,
    excludeDependencies += Dependencies.scodec213ExlusionRule
  )
  .dependsOn(munitScalamock % "test->test")

lazy val tetraByteCodecs = project
  .in(file("tetra-byte-codecs"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "tetra-byte-codecs",
    commonSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "org.plasmalabs.buildinfo.codecs.bytes.tetra",
    libraryDependencies ++= Dependencies.munitScalamock ++ Dependencies.protobufSpecs,
    excludeDependencies += Dependencies.scodec213ExlusionRule
  )
  .dependsOn(
    models     % "compile->compile;test->test",
    byteCodecs % "compile->compile;test->test",
    nodeCrypto % "compile->compile;test->test"
  )

lazy val typeclasses: Project = project
  .in(file("typeclasses"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "typeclasses",
    commonSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "org.plasmalabs.buildinfo.typeclasses",
    libraryDependencies ++= Dependencies.mUnitTest ++ Dependencies.logging ++ Dependencies.circe,
    excludeDependencies += Dependencies.scodec213ExlusionRule
  )
  .dependsOn(models % "compile->compile;test->test", nodeCrypto, tetraByteCodecs)

lazy val algebras = project
  .in(file("algebras"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "algebras",
    commonSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "org.plasmalabs.buildinfo.algebras",
    libraryDependencies ++= Dependencies.algebras,
    excludeDependencies += Dependencies.scodec213ExlusionRule
  )
  .dependsOn(models, nodeCrypto, tetraByteCodecs, munitScalamock % "test->test")

lazy val actor = project
  .in(file("actor"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "actor",
    commonSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "org.plasmalabs.buildinfo.actor",
    libraryDependencies ++= Dependencies.actor,
    excludeDependencies += Dependencies.scodec213ExlusionRule
  )
  .dependsOn(
    munitScalamock % "test->test"
  )

lazy val commonInterpreters = project
  .in(file("common-interpreters"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "common-interpreters",
    commonSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "org.plasmalabs.buildinfo.commoninterpreters",
    libraryDependencies ++= Dependencies.commonInterpreters,
    excludeDependencies += Dependencies.scodec213ExlusionRule
  )
  .dependsOn(
    models,
    algebras,
    typeclasses,
    byteCodecs,
    tetraByteCodecs,
    catsUtils,
    eventTree,
    munitScalamock % "test->test",
    levelDbStore   % "test->test"
  )

lazy val consensus = project
  .in(file("consensus"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "consensus",
    commonSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "org.plasmalabs.buildinfo.consensus",
    libraryDependencies ++= Dependencies.mUnitTest ++ Dependencies.consensus,
    excludeDependencies += Dependencies.scodec213ExlusionRule
  )
  .dependsOn(
    ledger,
    models % "compile->compile;test->test",
    typeclasses,
    nodeCrypto,
    tetraByteCodecs % "compile->compile;test->test",
    algebras        % "compile->compile;test->test",
    numerics,
    eventTree,
    commonInterpreters % "compile->test",
    munitScalamock     % "test->test"
  )

lazy val minting = project
  .in(file("minting"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "minting",
    commonSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "org.plasmalabs.buildinfo.minting",
    libraryDependencies ++= Dependencies.minting,
    excludeDependencies += Dependencies.scodec213ExlusionRule
  )
  .dependsOn(
    models % "compile->compile;test->test",
    typeclasses,
    nodeCrypto,
    tetraByteCodecs,
    algebras % "compile->compile;test->test",
    consensus,
    catsUtils,
    ledger,
    munitScalamock     % "test->test",
    commonInterpreters % "test->test"
  )

lazy val networking = project
  .in(file("networking"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "networking",
    commonSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "org.plasmalabs.buildinfo.networking",
    libraryDependencies ++= Dependencies.networking,
    excludeDependencies += Dependencies.scodec213ExlusionRule
  )
  .dependsOn(
    models % "compile->compile;test->test",
    config,
    typeclasses,
    nodeCrypto,
    byteCodecs,
    tetraByteCodecs,
    algebras % "compile->compile;test->test",
    consensus,
    commonInterpreters,
    catsUtils,
    eventTree,
    ledger,
    actor,
    munitScalamock % "test->test",
    blockchainCore
  )

lazy val transactionGenerator = project
  .in(file("transaction-generator"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "transaction-generator",
    commonSettings,
    coverageEnabled := false,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "org.plasmalabs.buildinfo.transactiongenerator",
    libraryDependencies ++= Dependencies.transactionGenerator,
    excludeDependencies += Dependencies.scodec213ExlusionRule
  )
  .dependsOn(
    models % "compile->compile;test->test",
    typeclasses,
    nodeCrypto,
    byteCodecs,
    tetraByteCodecs,
    munitScalamock,
    algebras,
    grpc,
    commonApplication,
    commonInterpreters,
    numerics
  )

lazy val ledger = project
  .in(file("ledger"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "ledger",
    commonSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "org.plasmalabs.buildinfo.ledger",
    libraryDependencies ++= Dependencies.ledger,
    excludeDependencies += Dependencies.scodec213ExlusionRule
  )
  .dependsOn(
    models   % "compile->compile;test->test",
    algebras % "compile->compile;test->test",
    typeclasses,
    eventTree,
    munitScalamock % "test->test",
    numerics,
    tetraByteCodecs % "test->test"
  )

lazy val blockchainCore = project
  .in(file("blockchain-core"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "blockchain-core",
    commonSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "org.plasmalabs.buildinfo.blockchaincore",
    libraryDependencies ++= Dependencies.blockchain,
    excludeDependencies += Dependencies.scodec213ExlusionRule
  )
  .dependsOn(
    models   % "compile->compile;test->test",
    algebras % "compile->compile;test->test",
    config,
    typeclasses,
    eventTree,
    ledger,
    munitScalamock % "test->test",
    consensus,
    minting,
    commonInterpreters,
    catsUtils
  )

lazy val blockchain = project
  .in(file("blockchain"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "blockchain",
    commonSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "org.plasmalabs.buildinfo.blockchain",
    libraryDependencies ++= Dependencies.blockchain,
    excludeDependencies += Dependencies.scodec213ExlusionRule
  )
  .dependsOn(
    models   % "compile->compile;test->test",
    algebras % "compile->compile;test->test",
    config,
    typeclasses,
    eventTree,
    ledger,
    munitScalamock % "test->test",
    consensus,
    minting,
    commonInterpreters,
    networking % "compile->compile;test->test",
    catsUtils,
    grpc,
    blockchainCore
  )

lazy val grpc = project
  .in(file("grpc"))
  .settings(
    name := "grpc",
    commonSettings,
    libraryDependencies ++= Dependencies.grpc,
    excludeDependencies += Dependencies.scodec213ExlusionRule
  )
  .dependsOn(
    models % "compile->compile;test->test",
    byteCodecs,
    tetraByteCodecs,
    algebras,
    catsUtils,
    typeclasses,
    munitScalamock % "test->test",
    blockchainCore
  )

lazy val levelDbStore = project
  .in(file("level-db-store"))
  .settings(
    name := "level-db-store",
    commonSettings,
    libraryDependencies ++= Dependencies.levelDbStore,
    excludeDependencies += Dependencies.scodec213ExlusionRule
  )
  .dependsOn(
    byteCodecs,
    algebras % "compile->compile;test->test",
    catsUtils
  )

lazy val nodeCrypto = project
  .in(file("node-crypto"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "node-crypto",
    commonSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "org.plasmalabs.buildinfo.nodecrypto",
    libraryDependencies ++= Dependencies.crypto,
    excludeDependencies += Dependencies.scodec213ExlusionRule
  )

lazy val catsUtils = project
  .in(file("cats-utils"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "cats-utils",
    commonSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "org.plasmalabs.buildinfo.catsUtils",
    libraryDependencies ++= Dependencies.catsUtils,
    excludeDependencies += Dependencies.scodec213ExlusionRule
  )

lazy val indexer = project
  .in(file("indexer"))
  .settings(
    name := "indexer",
    commonSettings,
    publish / skip := true,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "org.plasmalabs.buildinfo.indexer",
    libraryDependencies ++= Dependencies.indexer,
    excludeDependencies += Dependencies.scodec213ExlusionRule
  )
  .settings(indexerDockerSettings)
  .dependsOn(
    typeclasses,
    models % "compile->compile;test->test",
    tetraByteCodecs,
    grpc,
    commonInterpreters,
    commonApplication,
    munitScalamock % "test->test",
    numerics       % "test->compile"
  )
  .enablePlugins(BuildInfoPlugin, JavaAppPackaging, DockerPlugin)

lazy val munitScalamock = project
  .in(file("munit-scalamock"))
  .settings(
    name := "munit-scalamock",
    commonSettings,
    libraryDependencies ++= Dependencies.munitScalamock,
    excludeDependencies += Dependencies.scodec213ExlusionRule
  )

lazy val nodeIt = project
  .in(file("node-it"))
  .settings(
    name := "node-it",
    commonSettings,
    libraryDependencies ++= Dependencies.nodeIt,
    excludeDependencies += Dependencies.scodec213ExlusionRule
  )
  .dependsOn(
    node,
    models               % "test->compile",
    transactionGenerator % "test->compile"
  )

lazy val byzantineIt = project
  .in(file("byzantine-it"))
  .settings(
    name := "byzantine-it",
    commonSettings,
    Test / parallelExecution := false,
    libraryDependencies ++= Dependencies.byzantineIt,
    excludeDependencies ++= Seq(Dependencies.scodec213ExlusionRule, Dependencies.geny213ExlusionRule)
  )
  .dependsOn(
    node
  )

lazy val integration = (project in file("integration"))
  .aggregate(nodeIt, byzantineIt)

addCommandAlias("checkPR", s"; scalafixAll --check; scalafmtCheckAll; test; integration/Test/compile")
addCommandAlias("preparePR", s"; scalafixAll; scalafmtAll; test; integration/Test/compile")
addCommandAlias("checkPRTestQuick", s"; scalafixAll --check; scalafmtCheckAll; testQuick; integration/Test/compile")
