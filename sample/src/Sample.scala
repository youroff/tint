package sample

import scala.scalajs.js
import scala.scalajs.js.annotation._

// @JSExportTopLevel("M")
object M {
  val cnst = 2

  @JSExport
  def add2(x: Int) = x + cnst
}

object Sample {
  def main(args: Array[String]): Unit = {
    // val f = new js.Function("return M.add2(5)").asInstanceOf[js.Function0[Int]]
    val m = M.asInstanceOf[js.Dynamic]
    val result = m.add2(5)
    println(s"Hello world! $result")
  }
}
