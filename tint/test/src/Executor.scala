package tint

import utest._
import scala.scalajs.js
import org.scalajs.ir.Trees._
import org.scalajs.ir.Names._
import org.scalajs.ir.Types._
import org.scalajs.ir.Position
import org.scalajs.ir.OriginalName

object ExecutorTests extends TestSuite{
  implicit val position = Position.NoPosition
  implicit val env = Env.empty
  val x = LocalIdent(LocalName("x"))

  val tests = Tests {
    val executor = new Executor(Map())
    
    test("block execution returns last expression") {
      executor.eval(Block(List(StringLiteral("LOL")))) ==> "LOL"
    }

    test("block execution returns undefined when last expression is unit") {
      val envX = env.bind(x, 10)
      executor.eval(Block(List(
        Assign(VarRef(x)(IntType), IntLiteral(0))
      )))(envX) ==> js.undefined
      envX.read(x) ==> 0
    }

    test("If then else basic operation") {
      val exp = If(
        BinaryOp(BinaryOp.===, VarRef(x)(IntType), IntLiteral(0)),
        IntLiteral(1),
        IntLiteral(0)
      )(IntType)
      executor.eval(exp)(env.bind(x, 0)) ==> 1
      executor.eval(exp)(env.bind(x, 1)) ==> 0
    }
  }
}