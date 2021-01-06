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
    implicit val env = Env.empty.setThis(this)

    linkedClass.fields.foreach {
      case FieldDef(_, ident, _, tpe) =>
        fields.update((className, ident.name), Types.zeroOf(tpe))
      case JSFieldDef(flags, name, ftpe) =>
        println(s"Unimplemented JSFieldDef $name")
        ???
    }

    linkedClass.exportedMembers.map(_.value).foreach {
      case JSMethodDef(flags, name, args, body) =>
        val methodName = exec.eval(name).asInstanceOf[String]
        val methodBody = exec.evalJsFunction(args, body)
        this.asInstanceOf[js.Dynamic].updateDynamic(methodName)(methodBody)
      
      case desc @ JSPropertyDef(_, name, _, _) =>
        val propertyName = exec.eval(name).asInstanceOf[String]
        val descriptor = exec.evalPropertyDescriptor(desc)
        js.Object.defineProperty(this.asInstanceOf[js.Object], propertyName, descriptor)
    }

    initFields(linkedClass.superClass.map(_.name))
  }
  initFields(Some(className))

  def setField(field: (ClassName, FieldName), value: js.Any) =
    fields.update(field, value)

  def getField(field: (ClassName, FieldName)): js.Any =
    fields.get(field).getOrThrow(s"Instance doesn't have $field")

  override def toString(): String = s"Instance<$className>"
}
