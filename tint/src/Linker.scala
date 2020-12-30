package tint

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import org.scalajs.logging._
import org.scalajs.linker._
import org.scalajs.linker.interface._
import org.scalajs.linker.standard._

object Linker {

  def link(classPath: Seq[String], mainClass: String): Future[ModuleSet] = {
    val stdPath = "std/scalajs-library_2.13-1.3.1.jar"
    val mainName = "main"
 
    val cache = StandardImpl.irFileCache().newCache

    val config = StandardConfig()
      .withOptimizer(false)
      .withCheckIR(false)
      .withBatchMode(false)

    val frontend = StandardLinkerFrontend(config)
    val backend = StandardLinkerBackend(config)
    val symReqs = backend.symbolRequirements
    val initializers = Seq(
      ModuleInitializer.mainMethodWithArgs(mainClass, mainName)
    )

    NodeIRContainer.fromClasspath(stdPath +: classPath)
      .map(_._1)
      .flatMap(cache.cached _)
      .flatMap { irFiles =>
        frontend.link(irFiles ++ backend.injectedIRFiles, initializers, symReqs, new ScalaConsoleLogger)
      }
  }
}
