package sample

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport

class Foo extends js.Object

trait Bar extends js.Object

class Babar {
  @JSExport("foo")
  def foo(x: Int): Int = x + 1

  // JSMethodDef(StringLiteral("foo"), ParamDef(x: any), any,
  //   Apply(this.asInstanceOf[Babar], foo__I__I, x.asInstanceOf[int]))

  /*
   * function(x) {
   *   return <magically-call-the-interpreter>(body, x)
   * }
   *
   * function(executor, body) {
   *   return function(...args) {
   *     return executor.evalJSExports(body, args);
   *   }
   * }
   */
}

object Sample {
  def main(args: Array[String]): Unit = {
    //println("Hello world!")
    scala.scalajs.js.Dynamic.global.console.log("Hello world!")
  }
}
