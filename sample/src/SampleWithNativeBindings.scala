package sample

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

@js.native
@JSGlobal("Number")
object NumberJS extends js.Any {
  def parseInt(str: String): Int = js.native
}

object SampleWithNativeBindings {

  def run(args: Array[String]): Unit = {
    val result = NumberJS.parseInt("5") + NumberJS.parseInt("10")
    println("5 + 10 = " + result)
  }
}
