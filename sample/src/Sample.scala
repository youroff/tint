package sample

import scala.scalajs.js
import scala.scalajs.js.annotation._

// @JSExportTopLevel("M")
// object M {
//   val cnst = 2

//   @JSExport
//   def add2(x: Int) = x + cnst
// }

// class Foo extends js.Object {
//   val x: Int = 4
//   def bar(y: Int): Int = x + y
// }

// object Sample {
//   def main(args: Array[String]): Unit = {
//     // val f = new js.Function("return M.add2(5)").asInstanceOf[js.Function0[Int]]
//     // val m = M.asInstanceOf[js.Dynamic]
//     val m = new Foo()
//     val result = m.bar(5)
//     println(s"Hello world! $result")
//   }
// }

object Sample {
  def main(args: Array[String]): Unit = {
    val names = List("Sébastien", "Antoine", "Sophie", "Alice")
    for (name <- names)
      js.Dynamic.global.console.log(greeting(name))
  }
  def greeting(name: String): String = "Hello " + name
}
