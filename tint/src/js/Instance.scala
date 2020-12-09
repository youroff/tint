package tint

import scala.collection.mutable
import scala.scalajs.js
import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._
import utils.Utils.OptionsOps
import org.scalajs.linker.standard.LinkedClass

class Instance(val className: ClassName) extends js.Object {
  val fields: mutable.Map[(ClassName, FieldName), js.Any] = mutable.Map()

  def initFields(classes: Map[ClassName, LinkedClass]): Unit = {
    def aux(cls: Option[ClassName]): Unit = cls.foreach { className =>
      val linkedClass = classes.get(className).getOrThrow("No $className among linked classes")
      linkedClass.fields.foreach {
        case FieldDef(_, ident, _, tpe) =>
          fields.update((className, ident.name), zeroOf(tpe))
        case JSFieldDef(flags, name, ftpe) =>
          println(s"Unimplemented JSFieldDef $name")
          ???
      }
      aux(linkedClass.superClass.map(_.name))
    }
    aux(Some(className))
  }

  def setField(field: (ClassName, FieldName), value: js.Any) = fields.update(field, value)
  def getField(field: (ClassName, FieldName)): js.Any = fields.get(field).get

  private def zeroOf(tpe: Type): js.Any = tpe match {
    case BooleanType => false
    case CharType    => new CharInstance('\u0000')
    case LongType    => new LongInstance(0L)
    case ByteType | ShortType | IntType => 0
    case FloatType   => 0.0f
    case DoubleType  => 0.0
    case StringType  => ""
    case UndefType   => js.undefined
    case _           => null
  }

  override def toString(): String = s"Instance<$className>"
}
