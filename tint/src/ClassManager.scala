package tint

import scala.scalajs.js
import scala.collection.mutable
import org.scalajs.linker.standard.{ModuleSet, LinkedClass}
import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._
import tint.utils.Utils.OptionsOps
import org.scalajs.ir.ClassKind.Interface

class ClassManager(val classes: Map[ClassName, LinkedClass]) {
  val staticFields: mutable.Map[(ClassName, FieldName), js.Any] = mutable.Map()
  val classInstances: mutable.Map[TypeRef, Instance] = mutable.Map()
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
    def methodMatch(m: MethodDef): Boolean =
      m.methodName == methodName && m.flags.namespace == nspace && m.body.isDefined

    def superChain(pivot: Option[LinkedClass]): Option[MethodDef] = pivot.flatMap { classDef =>
      classDef.methods.map(_.value).find(methodMatch)
        .orElse(superChain(classDef.superClass.map(_.name).flatMap(classes.get(_))))
    }

    def interfaceChain(pivot: LinkedClass): List[(ClassName, MethodDef)] = {
      pivot.methods.map(_.value).filter(methodMatch).map((pivot.className, _)) ++
      pivot.interfaces.map(_.name).map(lookupClassDef).flatMap(interfaceChain)
    }

    def lookupInInterfaces(className: ClassName): Option[MethodDef] = {
      val candidates = interfaceChain(lookupClassDef(className))
      if (candidates.isEmpty)
        None
      else
        Some(candidates.reduceLeft[(ClassName, MethodDef)] {
          case ((cl, ml), (cr, mr)) if isSubclassOf(cl, cr) => (cl, ml)
          case ((cl, ml), (cr, mr)) if isSubclassOf(cr, cl) => (cr, mr)
          case ((cl, ml), (cr, mr)) => throw new AssertionError(s"Method overlap in interfaces: $cl and $cr are unrelated")
        }._2)
    }

    superChain(classes.get(className))
      .orElse(lookupInInterfaces(className))
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

  def lookupClassInstance(typeRef: TypeRef, orElse: => Instance): Instance =
    classInstances.getOrElseUpdate(typeRef, orElse)

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
