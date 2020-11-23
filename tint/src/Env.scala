package tint

import org.scalajs.ir.Trees._
import scala.scalajs.js

class EnvVar(var value: js.Any) {
  def update(newValue: js.Any) = {
    value = newValue
  }

  override def toString(): String = s"EnvVar<$value>"
}

class Env(table: Map[LocalIdent, EnvVar], ths: Option[js.Any] = None) {

  /** Augments the environment with a variable binding: returns new Env */
  def bind(name: LocalIdent, value: js.Any) = {
    // if (value.toString() == "[object Object]")
    //   println(s"binding $name with {${js.Object.entries(value.asInstanceOf[js.Object])}}")
    // else
    //   println(s"binding $name with $value")
    new Env(table + (name -> new EnvVar(value)))
  }

  /** Updates variable value */
  def assign(name: LocalIdent, value: js.Any) {
    // println(s"assigning $name with $value")
    lookup(name).update(value)
  }

  /** Reads variable value */
  def read(name: LocalIdent): js.Any = {
    lookup(name).value
  }

  def setThis(instance: js.Any) = {
    new Env(table, Some(instance))
  }

  def getThis: js.Any = ths match {
    case Some(instance) => instance
    case None => throw new NoSuchElementException("No THIS in current Env")
  }

  override def toString(): String = table.toString()

  private
  def lookup(name: LocalIdent): EnvVar = {
    table.get(name) match {
      case Some(value) => value
      case None => throw new NoSuchElementException(s"No variable $name in Env")
    }
  }
}

object Env {
  def empty = new Env(Map())  
}
