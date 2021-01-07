package sample

import scala.scalajs.js
import scala.scalajs.js.annotation._

class Rect(val width: Int, val height: Int) extends js.Object {
  def area = width * height
  def getWidth = width
  def multWidth(x: Int) = width * x
}

class Square(val side: Int) extends Rect(side, side) {
  def getSuperWidth = super.getWidth
}

object SampleWithJSClasses {
  def run(args: Array[String]): Unit = {
    val square = new Square(5)
    val result = square.area
    val lambda = (x: Int) => x * result
    println("Lambda says " + lambda(2))
    println(s"Hello world! $result ${square.getSuperWidth} ${square.multWidth(2)}")
  }
}
