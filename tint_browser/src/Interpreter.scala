package tint_browser
import tint._

import scala.scalajs.js
import js.annotation._
import scala.concurrent.ExecutionContext.Implicits.global
import org.scalajs.linker.interface.ModuleInitializer
import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._
import org.scalajs.ir.Position
import org.scalajs.linker.interface.unstable.ModuleInitializerImpl._

@JSExportTopLevel("Interpreter")
class Interpreter(
  val irPath: String,
  val mainClass: String,
  val mainMethod: String = "main",
  val stdPath: String = "scalajs-library_2.13-1.3.1.jar"
) extends js.Object {
  val reader = new BrowserReader(stdPath, irPath)

  def run() = {
    println("Reading IR...")
    reader.irFiles.flatMap { irFiles =>
      println(s"Linking ${irFiles.size} files")
      Linker.link(irFiles, ModuleInitializer.mainMethodWithArgs(mainClass, mainMethod))
    }.foreach { moduleSet =>
      println("ModuleSet loaded...")
      val executor = new Executor(ClassManager.fromModuleSet(moduleSet))
      implicit val pos = Position.NoPosition
      moduleSet.modules.foreach { module =>
        module.initializers.foreach {
          case MainMethodWithArgs(className, methodName, args) =>
            val values = List(convertArgs(args))
            val tree = ApplyStatic(ApplyFlags.empty, className, MethodIdent(methodName), values)(NoType)
            executor.execute(tree)
          case VoidMainMethod(className, methodName) =>
            val tree = ApplyStatic(ApplyFlags.empty, className, MethodIdent(methodName), List())(NoType)
            executor.execute(tree)
        }
      }
    }
  }

  def convertArgs(args: List[String]): Tree = ArrayValue(
    ArrayTypeRef.of(ClassRef(ClassName("java.lang.String"))),
    args map (StringLiteral(_)(Position.NoPosition))
  )(Position.NoPosition)
}
