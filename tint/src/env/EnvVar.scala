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
