package tint

import utest._
import scala.scalajs.js
import org.scalajs.ir.Types._
import org.scalajs.ir.Position
import org.scalajs.ir.Trees._
import org.scalajs.ir.Names.LocalName
import org.scalajs.ir.OriginalName

object GlobalTests extends TestSuite{
  implicit val position = Position.NoPosition
  implicit val env = Env.empty

  val tests = Tests {   
    val executor = new Executor(Map())

    test("JSGlobalRef") {
      executor.eval(JSGlobalRef("console")) ==> js.Dynamic.global.console
    }

    test("JSTypeOfGlobalRef") {
      executor.eval(JSTypeOfGlobalRef(JSGlobalRef("console"))) ==> "object"
    }

    // test("Imports... some") {
    //   println(js.eval("import('jsdom').then(e => console.log(e))"))
    // }

  }
}
