package co.topl

import java.lang.management.ManagementFactory

import akka.actor.{ActorRef, ActorSystem, PoisonPill}
import akka.http.scaladsl.Http
import akka.io.Tcp
import akka.pattern.ask
import akka.util.Timeout
import co.topl.consensus.{Forger, ForgerRef, KeyManager, KeyManagerRef}
import co.topl.http.HttpService
import co.topl.http.api.ApiEndpoint
import co.topl.http.api.endpoints.{DebugApiEndpoint, _}
import co.topl.modifier.block.Block
import co.topl.modifier.transaction.Transaction
import co.topl.network.NetworkController.ReceivableMessages.BindP2P
import co.topl.network._
import co.topl.network.message.BifrostSyncInfo
import co.topl.network.utils.UPnPGateway
import co.topl.nodeView.history.History
import co.topl.nodeView.mempool.MemPool
import co.topl.nodeView.state.State
import co.topl.nodeView.{MempoolAuditor, MempoolAuditorRef, NodeViewHolder, NodeViewHolderRef}
import co.topl.settings._
import co.topl.utils.{Logging, NetworkType}
import co.topl.wallet.{WalletConnectionHandler, WalletConnectionHandlerRef}
import com.sun.management.{HotSpotDiagnosticMXBean, VMOption}
import com.typesafe.config.{Config, ConfigFactory}
import kamon.Kamon

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success}

class BifrostApp(startupOpts: StartupOpts) extends Logging with Runnable {

  type BSI = BifrostSyncInfo
  type TX = Transaction.TX
  type PMOD = Block
  type HIS = History
  type MP = MemPool
  type ST = State

  /** Setup settings file to be passed into the application */
  private val (settings: AppSettings, config: Config) = AppSettings.read(startupOpts)
  log.debug(s"Starting application with settings \n$settings")

  /** check for gateway device and setup port forwarding */
  private val upnpGateway: Option[UPnPGateway] =
    if (settings.network.upnpEnabled) UPnPGateway(settings.network) else None

  /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ---------------- */
  /** Setup the execution environment for running the application */

  implicit protected lazy val actorSystem: ActorSystem = ActorSystem(settings.network.agentName, config)
  implicit private val timeout: Timeout = Timeout(settings.network.controllerTimeout.getOrElse(5 seconds))
  implicit val executionContext: ExecutionContext = actorSystem.dispatcher

  /** save runtime environment into a variable for reference throughout the application */
  protected val appContext = new AppContext(settings, startupOpts, upnpGateway)
  log.debug(
    s"${Console.MAGENTA}Runtime network parameters:" +
    s"type - ${appContext.networkType.verboseName}, " +
    s"prefix - ${appContext.networkType.netPrefix}, " +
    s"forging status: ${settings.forging.forgeOnStartup}" +
    s"${Console.RESET}"
  )

  /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ---------------- */
  /** Create Bifrost singleton actors */
  private val peerManagerRef: ActorRef = PeerManagerRef(PeerManager.actorName, settings, appContext)

  private val networkControllerRef: ActorRef =
    NetworkControllerRef(NetworkController.actorName, settings, peerManagerRef, appContext)

  private val keyManagerRef = KeyManagerRef(KeyManager.actorName, settings, appContext)

  private val forgerRef: ActorRef =
    ForgerRef[HIS, ST, MP](Forger.actorName, settings, appContext, keyManagerRef)

  private val nodeViewHolderRef: ActorRef = NodeViewHolderRef(NodeViewHolder.actorName, settings, appContext)

  private val mempoolAuditor: ActorRef =
    MempoolAuditorRef[ST, MP](MempoolAuditor.actorName, settings, appContext, nodeViewHolderRef, networkControllerRef)

  private val walletConnectionHandlerRef: Option[ActorRef] =
    if (settings.gjallarhorn.enableWallet) {
      Some(WalletConnectionHandlerRef[PMOD](WalletConnectionHandler.actorName, settings, appContext, nodeViewHolderRef))
    } else {
      None
    }

  private val peerSynchronizer: ActorRef =
    PeerSynchronizerRef(PeerSynchronizer.actorName, networkControllerRef, peerManagerRef, settings, appContext)

  private val nodeViewSynchronizer: ActorRef = NodeViewSynchronizerRef[TX, BSI, PMOD, HIS, MP](
    NodeViewSynchronizer.actorName,
    networkControllerRef,
    nodeViewHolderRef,
    settings,
    appContext
  )

  /** Sequence of actors for cleanly shutting now the application */
  private val actorsToStop: Seq[ActorRef] = Seq(
    peerManagerRef,
    networkControllerRef,
    peerSynchronizer,
    nodeViewSynchronizer,
    keyManagerRef,
    forgerRef,
    nodeViewHolderRef,
    mempoolAuditor
  ) ++ walletConnectionHandlerRef

  /** hook for initiating the shutdown procedure */
  sys.addShutdownHook(BifrostApp.shutdown(actorSystem, actorsToStop))

  /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ---------------- */
  /** Create and register controllers for API routes */
  private val apiRoutes: Seq[ApiEndpoint] = Seq(
    UtilsApiEndpoint(settings.rpcApi, appContext),
    AdminApiEndpoint(settings.rpcApi, appContext, forgerRef, keyManagerRef),
    NodeViewApiEndpoint(settings.rpcApi, appContext, nodeViewHolderRef),
    TransactionApiEndpoint(settings.rpcApi, appContext, nodeViewHolderRef),
    DebugApiEndpoint(settings.rpcApi, appContext, nodeViewHolderRef, keyManagerRef)
  )

  private val httpService = HttpService(apiRoutes, settings.rpcApi)

  /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ---------------- */
  /** Am I running on a JDK that supports JVMCI? */
  val vm_version: String = System.getProperty("java.vm.version")
  System.out.printf("java.vm.version = %s%n", vm_version)

  val bean: HotSpotDiagnosticMXBean = ManagementFactory.getPlatformMXBean(classOf[HotSpotDiagnosticMXBean])

  // Is JVMCI enabled?
  try {
    val enableJVMCI: VMOption = bean.getVMOption("EnableJVMCI")
    log.debug(s"$enableJVMCI")
  } catch {
    case e: Throwable =>
      log.error(s"${Console.RED}Unexpected error when checking for JVMCI: $e ${Console.RESET}")
      BifrostApp.shutdown(actorSystem, actorsToStop)
  }

  /** Is the system using the JVMCI compiler for normal compilations? */
  val useJVMCICompiler: VMOption = bean.getVMOption("UseJVMCICompiler")
  log.debug(s"$useJVMCICompiler")

  /** What compiler is selected? */
  val compiler: String = System.getProperty("jvmci.Compiler")
  System.out.printf("jvmci.Compiler = %s%n", compiler)

  /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ---------------- */
  ////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////// METHOD DEFINITIONS ////////////////////////////////
  def run(): Unit = {
    require(settings.network.agentName.length <= settings.network.applicationNameLimit)

    log.debug(s"Available processors: ${Runtime.getRuntime.availableProcessors}")
    log.debug(s"Max memory available: ${Runtime.getRuntime.maxMemory}")
    log.debug(s"RPC is allowed at: ${settings.rpcApi.bindAddress}")

    val httpHost = settings.rpcApi.bindAddress.getHostName
    val httpPort = settings.rpcApi.bindAddress.getPort

    /** Helper function to kill the application if needed */
    def failedP2P(): Unit = {
      log.error(s"${Console.RED}Unable to bind to the P2P port. Terminating application!${Console.RESET}")
      BifrostApp.shutdown(actorSystem, actorsToStop)
    }

    /** Trigger the P2P network bind and check that the protocol bound successfully. */
    /** Terminate the application on failure */
    (networkControllerRef ? BindP2P).onComplete {
      case Success(bindResponse: Future[Any]) =>
        bindResponse.onComplete {
          case Success(Tcp.Bound(addr)) => log.info(s"${Console.YELLOW}P2P protocol bound to $addr${Console.RESET}")
          case Success(_) | Failure(_)  => failedP2P()
        }
      case Success(_) | Failure(_) => failedP2P()
    }

    /** trigger the HTTP server bind and check that the bind is successful. Terminate the application on failure */
    Http().newServerAt(httpHost, httpPort).bind(httpService.compositeRoute).onComplete {
      case Success(serverBinding) =>
        log.info(s"${Console.YELLOW}HTTP server bound to ${serverBinding.localAddress}${Console.RESET}")

      case Failure(ex) =>
        log.error(
          s"${Console.YELLOW}Failed to bind to $httpHost:$httpPort. " +
          s"Terminating application!${Console.RESET}",
          ex
        )
        BifrostApp.shutdown(actorSystem, actorsToStop)
    }

  }
}

/** This is the primary application object and is the entry point for Bifrost to begin execution */
object BifrostApp extends Logging {

  /** Check if Kamon instrumentation should be started. */
  /** DO NOT MOVE!! This must happen before anything else! */
  private val conf: Config = ConfigFactory.load("application")
  if (conf.getBoolean("kamon.enable")) Kamon.init()

  /** import for parsing command line arguments */
  import com.joefkelley.argyle._

  /** parse command line arguments */
  val argParser: Arg[StartupOpts] =
    (optional[String]("--config", "-c") and
      optionalOneOf[NetworkType](NetworkType.all.map(x => s"--${x.verboseName}" -> x): _*) and
      (optional[String]("--seed", "-s") and
      flag("--forge", "-f") and
      optional[String]("--apiKeyHash")).to[RuntimeOpts]).to[StartupOpts]

  ////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////// METHOD DEFINITIONS ////////////////////////////////
  def main(args: Array[String]): Unit =
    argParser.parse(args) match {
      case Success(argsParsed) => new BifrostApp(argsParsed).run()
      case Failure(e)          => throw e
    }

  def forceStopApplication(code: Int = 1): Nothing = sys.exit(code)

  def shutdown(system: ActorSystem, actors: Seq[ActorRef]): Unit = {
    log.warn("Terminating Actors")
    actors.foreach(_ ! PoisonPill)
    log.warn("Terminating ActorSystem")
    val termination = system.terminate()
    Await.result(termination, 60.seconds)
    log.warn("Application has been terminated.")
  }
}
