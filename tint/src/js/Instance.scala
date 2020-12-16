package tint

import scala.collection.mutable
import scala.scalajs.js
import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._
import utils.Utils.OptionsOps
import org.scalajs.linker.standard.LinkedClass

class Instance(val className: ClassName, exec: Executor) extends js.Object {
  val fields: mutable.Map[(ClassName, FieldName), js.Any] = mutable.Map()

  private def initFields(cls: Option[ClassName]): Unit = cls.foreach { className =>
    val linkedClass = exec.lookupClassDef(className)

    linkedClass.fields.foreach {
      case FieldDef(_, ident, _, tpe) =>
        fields.update((className, ident.name), zeroOf(tpe))
      case JSFieldDef(flags, name, ftpe) =>
        println(s"Unimplemented JSFieldDef $name")
        ???
    }

    linkedClass.exportedMembers.map(_.value).foreach {
      case JSMethodDef(flags, name, args, body) =>
        implicit val env = Env.empty.setThis(this)
        val methodName = exec.eval(name).asInstanceOf[String]
        val methodBody = exec.evalJsMethodBody(args, body)
        this.asInstanceOf[js.Dynamic].updateDynamic(methodName)(methodBody)
      case JSPropertyDef(flags, name, getterBody, setterArgAndBody) =>
        println(s"Unimplemented JSPropertyDef $name")
        ???
    }

    initFields(linkedClass.superClass.map(_.name))
  }
  initFields(Some(className))

  def setField(field: (ClassName, FieldName), value: js.Any) =
    fields.update(field, value)

  def getField(field: (ClassName, FieldName)): js.Any =
    fields.get(field).getOrThrow(s"Instance doesn't have $field")

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
