package sample

import scala.scalajs.js

object SampleWithCaptures {
  def makeGreeter(greetingFormat: String): js.Dynamic = {
    class Greeter extends js.Object {
      def greet(name: String) = println(greetingFormat.format(name))
    }
    js.constructorOf[Greeter]
  }

  def greetPeople(greeterClass: js.Dynamic): Unit = {
    val greeter = js.Dynamic.newInstance(greeterClass)()
    greeter.greet("Jane")
    greeter.greet("John");
  }

  def run(args: Array[String]): Unit = {
    val englishGreeterClass = makeGreeter("Hello, %s!")
    greetPeople(englishGreeterClass)
    val frenchGreeterClass = makeGreeter("Bonjour, %s!")
    greetPeople(frenchGreeterClass)
    val japaneseGreeterClass = makeGreeter("%sさん、こんにちは。")
    greetPeople(japaneseGreeterClass)
  }
}
