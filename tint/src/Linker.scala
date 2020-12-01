package tint

import org.scalajs.linker._
import scala.concurrent.ExecutionContext.Implicits.global
import org.scalajs.linker.frontend.IRLoader
import org.scalajs.linker.standard._
import org.scalajs.linker.interface._
import scala.concurrent._
import org.scalajs.logging._
import org.scalajs.ir.Trees._

object Linker {
  private final class StoreLinkingUnitLinkerBackend(originalBackend: LinkerBackend) extends LinkerBackend {

    @volatile
    private var _linkingUnit: LinkingUnit = _

    val coreSpec: CoreSpec = originalBackend.coreSpec

    val symbolRequirements: SymbolRequirement = originalBackend.symbolRequirements

    override def injectedIRFiles: Seq[IRFile] = originalBackend.injectedIRFiles

    def emit(unit: LinkingUnit, output: LinkerOutput, logger: Logger)(
        implicit ec: ExecutionContext): Future[Unit] = {
      _linkingUnit = unit
      Future.successful(())
    }

    def linkingUnit: LinkingUnit = {
      if (_linkingUnit == null)
        throw new IllegalStateException("Cannot access linkingUnit before emit is called")
      _linkingUnit
    }
  }

  def link(classPath: Seq[String], mainClass: String): Future[LinkingUnit] = {
    val stdPath = "std/scalajs-library_2.13-1.2.0.jar"
    val mainName = "main"
    val cache = StandardImpl.irFileCache().newCache

    val config = StandardConfig().withOptimizer(false)
    val frontend = StandardLinkerFrontend(config)
    val backend = new StoreLinkingUnitLinkerBackend(StandardLinkerBackend(config))
    val linker = StandardLinkerImpl(frontend, backend)
    val output = LinkerOutput(MemOutputFile())
    val initializer = ModuleInitializer.mainMethodWithArgs(mainClass, mainName)

    NodeIRContainer.fromClasspath(stdPath +: classPath).flatMap {
      case (irContainers, _) => cache.cached(irContainers)
    }.flatMap { irFiles =>
      linker.link(irFiles, Seq(initializer), output, new ScalaConsoleLogger(Level.Error))
    }.map { _ =>
      backend.linkingUnit
    }
  }
}
