package tint

import scala.scalajs.js
import org.scalajs.ir.Trees.LabelIdent

final case class LabelException(
  val label: LabelIdent,
  val value: js.Any
) extends Exception(s"Uncaught Labeled jump: ${label.name} - $value", null)
