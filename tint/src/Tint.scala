package tint

import org.scalajs.ir.Trees._
import org.scalajs.ir.Position
import org.scalajs.ir.Types._
import org.scalajs.ir.Names.ClassName
import scala.concurrent.ExecutionContext.Implicits.global
import org.scalajs.linker.interface.unstable.ModuleInitializerImpl._
import org.scalajs.linker.interface.ModuleInitializer

object Tint {
  def main(args: Array[String]): Unit = {
    Linker.link(
      Seq("sample/target/scala-2.13/classes/sample"),
      ModuleInitializer.mainMethodWithArgs("sample.Sample", "main")
    ).foreach { moduleSet =>
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
