package tint

import org.scalajs.linker.interface.unstable.ModuleInitializerImpl.MainMethodWithArgs
import org.scalajs.ir.Trees._
import org.scalajs.ir.Position
import org.scalajs.ir.Types._
import org.scalajs.ir.Names.ClassName

object Tint {
  def main(args: Array[String]): Unit = {
    Linker.link(
      Seq("out/sample/compile/dest/classes/sample"),
      "sample.Sample"
    ) { unit =>
      val defs = unit.classDefs.map(c => (c.name.name, c)).toMap
      val executor = new Executor(defs)

      unit.moduleInitializers(0) match {
        case MainMethodWithArgs(className, methodName, args) =>
          implicit val pos = Position.NoPosition
          val values = List(convertArgs(args))
          val tree = ApplyStatic(ApplyFlags.empty, className, MethodIdent(methodName), values)(NoType)
          executor.execute(tree)
      }
    }
  }

  def convertArgs(args: List[String]): Tree = ArrayValue(
    ArrayTypeRef.of(ClassRef(ClassName("java.lang.String"))),
    args map (StringLiteral(_)(Position.NoPosition))
  )(Position.NoPosition)
}
