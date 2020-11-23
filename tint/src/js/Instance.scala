package tint

import scala.collection.mutable
import scala.scalajs.js
import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._

class Instance(val className: ClassName) extends js.Object {
  val fields: mutable.Map[FieldIdent, js.Any] = mutable.Map()

  def setField(field: FieldIdent, value: js.Any) = fields.update(field, value)
  def getField(field: FieldIdent): js.Any = fields.get(field).get

  override def toString(): String = s"Instance<$className>"
}
