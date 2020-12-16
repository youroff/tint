package tint

import scala.scalajs.js

object Purifier {

  def asBoolean(value: Any): Boolean = value match {
    case _: Boolean => value.asInstanceOf[Boolean]
    case _ => throw new Error("Interpreter Error: Not a Boolean")
  }

  def asInt(value: Any): Int = value match {
    case _: Int => value.asInstanceOf[Int]
    case _ => throw new Error("Interpreter Error: Not an Int")
  }

  def asFloat(value: Any): Float = value match {
    case _: Float => value.asInstanceOf[Float]
    case _ => throw new Error("Interpreter Error: Not a Float")
  }

  def asDouble(value: Any): Double = value match {
    case _: Double => value.asInstanceOf[Double]
    case _ => throw new Error("Interpreter Error: Not a Double")
  }

}
