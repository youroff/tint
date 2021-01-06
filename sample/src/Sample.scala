package sample

import scala.scalajs.js
import scala.scalajs.js.annotation._

// @JSExportTopLevel("M")
// object M {
//   val cnst = 2

//   @JSExport
//   def add2(x: Int) = x + cnst
// }

// @JSExportTopLevel("Bar")
// object Foo {

//   // @JSExport
//   var bar = 1

//   // @JSExport
//   def sayHello(): Unit = {
//     println("Hello world!")
//   }
// }

class Rect(val width: Int, val height: Int) extends js.Object {
  def area = width * height
  def getWidth = width
  def multWidth(x: Int) = width * x
}

class Square(val side: Int) extends Rect(side, side) {
  def getSuperWidth = super.getWidth
}

object Sample {
  def main(args: Array[String]): Unit = {
    val square = new Square(5)
    val result = square.area
    val lambda = (x: Int) => x * result
    println("Lambda says " + lambda(2))
    println(s"Hello world! $result ${square.getSuperWidth} ${square.multWidth(2)}")
  }
}

// object Sample {
//   def main(args: Array[String]): Unit = {
//     val names = List("Sébastien", "Antoine", "Sophie", "Alice")
//     for (name <- names)
//       js.Dynamic.global.console.log(greeting(name))
//   }
//   def greeting(name: String): String = "Hello " + name
// }


// def makeGreeter(greetingFormat: String): js.Dynamic = {
//   class Greeter extends js.Object {
//     def greet(name: String): String =
//       println(greetingFormat.format(name))
//   }
//   js.constructorOf[Greeter]
// }
// def greetPeople(greeterClass: js.Dynamic): Unit = {
//   val greeter = js.Dynamic.newInstance(greeterClass)()
//   greeter.greet("Jane")
//   greeter.greet("John");
// }
// val englishGreeterClass = makeGreeter("Hello, %s!")
// greetPeople(englishGreeterClass)
// val frenchGreeterClass = makeGreeter("Bonjour, %s!")
// greetPeople(frenchGreeterClass)
// val japaneseGreeterClass = makeGreeter("%sさん、こんにちは。")
// greetPeople(japaneseGreeterClass)