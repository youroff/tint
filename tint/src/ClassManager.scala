package tint

import scala.scalajs.js
import scala.collection.mutable
import org.scalajs.linker.standard.{ModuleSet, LinkedClass}
import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._
import tint.utils.Utils.OptionsOps
import org.scalajs.ir.ClassKind.Interface

class ClassManager(val classes: Map[ClassName, LinkedClass]) {
  val staticFields: mutable.Map[(ClassName, FieldName), js.Any] = mutable.Map()
  val classInstances: mutable.Map[ClassName, Instance] = mutable.Map()
  val modules: mutable.Map[ClassName, Instance] = mutable.Map()
  val names = new utils.NameGen()

  classes.values.foreach { linkedClass =>
    linkedClass.fields.foreach {
      case FieldDef(flags, FieldIdent(name), _, tpe) if flags.namespace.isStatic =>
        staticFields.update((linkedClass.className, name), Types.zeroOf(tpe))
      case _ => ()
    }
  }

  def lookupClassDef(name: ClassName): LinkedClass = {
    classes.get(name).getOrThrow(s"No class $name in class cache")
  }

  def lookupMethodDef(className: ClassName, methodName: MethodName, nspace: MemberNamespace): MethodDef = {
    def superChain(pivot: Option[ClassName]): Option[MethodDef] = pivot.flatMap { className =>
      val classDef = lookupClassDef(className)
      classDef.methods.find { methodDef =>
        methodDef.value.methodName == methodName &&
        methodDef.value.flags.namespace == nspace
      }.map(_.value).orElse(superChain(classDef.superClass.map(_.name)))
    }

    // def interfaceChain(pivot: ClassName): Option[MethodDef] = {
    //   val classDef = lookupClassDef(className)
    //   classDef.interfaces.find { ifaceName =>
    //     // val ifaceDef = lookupClassDef(ifaceName)
    //   }
    // }

    superChain(Some(className))
      // .orElse()
      .getOrThrow(s"No method $methodName in $className")
  }

  def lookupInitializer(name: ClassName): MethodDef = {
    lookupClassDef(name).methods
      .find(_.value.methodName == NoArgConstructorName)
      .getOrThrow(s"Constructor for $name not found").value
  }

  def loadModule(className: ClassName, orElse: => Instance): Instance =
    modules.getOrElseUpdate(className, orElse)

  def storeModule(className: ClassName, instance: Instance) =
    modules.update(className, instance)

  def lookupClassInstance(className: ClassName, orElse: => Instance): Instance =
    classInstances.getOrElseUpdate(className, orElse)

  def genTypeData(className: ClassName): js.Any = {
    val classDef = lookupClassDef(className)
    js.Dynamic.literal(
      name = classDef.fullName,
      isPrimitive = false,
      isInterface = classDef.kind == Interface,
      isArrayClass = false
    )
  }

  def getStaticField(key: (ClassName, FieldName)): js.Any = 
    staticFields.get(key).getOrThrow(s"Static field ${key._2} on ${key._1} not found")

  def setStaticField(key: (ClassName, FieldName), value: js.Any) = {
    assert(
      staticFields.contains(key),
      s"Static field ${key._2} on ${key._1} not found (for assignment)"
    )
    staticFields.update(key, value)
  }

  /** 
   * Run callback on a LinkedClass and all its parent classes
  */
  def superChain(className: ClassName)(callback: LinkedClass => Unit): Unit = {
    val linkedClass = lookupClassDef(className)
    callback(linkedClass)
    linkedClass.superClass.map(_.name).foreach(clazz => superChain(clazz)(callback))
  }

  /** 
   * Check if left className is a subclass of the right className
   * - classNames are equal
   * - recursively call on a superClass of left className
   * - recursively check interfaces using the same algorithm
  */
  def isSubclassOf(lhs: ClassName, rhs: ClassName): Boolean = {
    val classDef = lookupClassDef(lhs)
    lhs.equals(rhs) ||
    classDef.superClass.map(_.name).map(isSubclassOf(_, rhs)).getOrElse(false) ||
    classDef.interfaces.map(_.name).exists(isSubclassOf(_, rhs))
  }
}

object ClassManager {
  def fromModuleSet(moduleSet: ModuleSet): ClassManager =
    new ClassManager(moduleSet.modules.flatMap(_.classDefs).map(c => (c.name.name, c)).toMap)
  
  def empty = new ClassManager(Map())
}