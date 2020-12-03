package tint

import org.scalajs.ir.Trees._
import scala.scalajs.js
import org.scalajs.ir.Names.LocalName
import utils.Utils.OptionsOps

class EnvVar(var value: js.Any) {
  def update(newValue: js.Any) = {
    value = newValue
  }

  override def toString(): String = s"EnvVar<$value>"
}

class Env(table: Map[LocalName, EnvVar], ths: Option[js.Any]) {

  def exposeThis = ths match {
    case Some(t) => println("This: " + t)
    case None => println("No This")
  }

  /** Augments the environment with a variable binding: returns new Env */
  def bind(name: LocalName, value: js.Any) = {
    // if (value.toString() == "[object Object]")
    //   println(s"binding $name with {${js.Object.entries(value.asInstanceOf[js.Object])}}")
    // else
    //   println(s"binding $name with $value")
    new Env(table + (name -> new EnvVar(value)), ths)
  }

  /** Updates variable value */
  def assign(name: LocalName, value: js.Any) {
    // println(s"assigning $name with $value")
    lookup(name).update(value)
  }

  /** Reads variable value */
  def read(name: LocalName): js.Any = {
    lookup(name).value
  }

  def setThis(instance: js.Any) = {
    new Env(table, Some(instance))
  }

  def getThis: js.Any = ths.getOrThrow("No THIS in current Env")

  override def toString(): String = table.toString()

  private
  def lookup(name: LocalName): EnvVar = table.get(name).getOrThrow(s"No variable $name in Env")
}

object Env {
  def empty = new Env(Map(), None)  
}
