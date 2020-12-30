package tint

import utest._
import scala.scalajs.js
import org.scalajs.ir.Types._
import org.scalajs.ir.Position
import org.scalajs.ir.Trees._
import org.scalajs.ir.Names.LocalName
import org.scalajs.ir.OriginalName

object ObjectTests extends TestSuite{
  implicit val position = Position.NoPosition
  implicit val env = Env.empty
  val x = LocalName("x")

  val tests = Tests {   
    val executor = new Executor(Map())

    test("construction and select") {
      val objTree = JSObjectConstr(List(
        (StringLiteral("a"), IntLiteral(1)),
        (StringLiteral("b"), BooleanLiteral(true)),
      ))

      executor.eval(JSSelect(objTree, StringLiteral("a"))) ==> 1
      executor.eval(JSSelect(objTree, StringLiteral("b"))) ==> true
      executor.eval(JSSelect(objTree, StringLiteral("c"))) ==> js.undefined
    }

    test("property descriptor") {
      val obj = js.special.objectLiteral()
      val selector = JSSelect(This()(AnyType), StringLiteral("x"))
      val descriptor = executor.evalPropertyDescriptor(JSPropertyDef(
        MemberFlags.empty,
        StringLiteral("y"),
        Some(selector),
        Some((
          ParamDef(LocalIdent(LocalName("value")), OriginalName.NoOriginalName, AnyType, false, false),
          Assign(selector, VarRef(LocalIdent(LocalName("value")))(AnyType))
        ))
      ))
      js.Object.defineProperty(obj, "y", descriptor)

      new js.Function("obj", "return obj.y;")
        .asInstanceOf[js.Function1[js.Object, js.Any]](obj) ==> js.undefined
      new js.Function("obj", "val", "obj.y = val;")
        .asInstanceOf[js.Function2[js.Object, js.Any, js.Any]](obj, 1) ==> js.undefined
      new js.Function("obj", "return obj.y;")
        .asInstanceOf[js.Function1[js.Object, js.Any]](obj) ==> 1
    }
  }
}
