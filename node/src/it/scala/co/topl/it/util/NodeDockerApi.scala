package co.topl.it.util

import com.spotify.docker.client.DockerClient
import com.typesafe.config.{Config, ConfigRenderOptions}
import org.slf4j.{Logger, LoggerFactory}

import java.nio.file.{Files, Path, Paths}
import java.util.Comparator
import scala.collection.JavaConverters._

case class NodeDockerApi(containerId: String)(implicit dockerClient: DockerClient) {

  val logger: Logger = LoggerFactory.getLogger(this.toString)

  def start(): Unit = {
    logger.info(s"Starting")
    dockerClient.startContainer(containerId)
  }

  def stop(): Unit = {
    logger.info(s"Stopping")
    dockerClient.stopContainer(containerId, 10)
  }

  def ipAddress(): String =
    dockerClient.inspectContainer(containerId).networkSettings().ipAddress()

  def reconfigure(config: Config): Unit = {
    logger.info(s"Reconfiguring to: ${config.root().render(ConfigRenderOptions.concise())}")
    val wasRunning = dockerClient.inspectContainer(containerId).state().running()
    if (wasRunning) {
      stop()
    }

    val tmpConfigDirectory = Files.createTempDirectory("bifrost-test-config")
    val renderedConfig = config.root().render(ConfigRenderOptions.concise())
    val testConfigPath = Paths.get(tmpConfigDirectory.toString, "testConfig.conf")
    Files.write(testConfigPath, renderedConfig.getBytes)

    dockerClient.copyToContainer(tmpConfigDirectory, containerId, "/opt/docker/config")

    Files
      .walk(tmpConfigDirectory)
      .sorted(Comparator.reverseOrder[Path]())
      .iterator()
      .asScala
      .foreach(Files.delete)

    if (wasRunning) {
      start()
    }
  }

}

object NodeDockerApi {

  def apply(node: BifrostDockerNode)(implicit dockerClient: DockerClient): NodeDockerApi =
    new NodeDockerApi(node.containerId)

}
