package tint

import scala.collection.mutable
import scala.scalajs.js
import org.scalajs.ir.Trees._
import org.scalajs.ir.Names._
import org.scalajs.ir.Types._
import org.scalajs.linker.standard.LinkedClass
import org.scalajs.ir.Position.NoPosition
import scala.scalajs.LinkingInfo
import org.scalajs.ir.ScalaJSVersions
import scala.scalajs.js.annotation.JSExport

class Executor(classes: Map[ClassName, LinkedClass]) {
  val modules: mutable.Map[ClassName, Instance] = mutable.Map()
  val linkingInfo = js.Dictionary(
    // TODO: Add other flags when required
    "linkerVersion" -> ScalaJSVersions.current
  )
  implicit val pos = NoPosition

  def execute(program: Tree): Unit = {
    eval(program)(Env.empty)
  }

  @JSExport("evalJSExports")
  def evalJSExports(body: Tree, args: js.Array[js.Any]): js.Any = {
    ???
  }

  def eval(program: Tree)(implicit env: Env): js.Any = {
    println("EVAL======")
    println(program)
    val result: js.Any = program match {
      case Block(trees) => evalBlock(trees)
      case Skip() => ()
      case JSMethodApply(receiver, method, args) =>
        val obj = eval(receiver).asInstanceOf[RawJSValue]
        val met = eval(method)
        // TODO: Take care of Spread
        val eargs = args.asInstanceOf[List[Tree]] map eval
        obj.jsMethodApply(met)(eargs: _*)
      case JSGlobalRef(name) =>
        lookupGlobalVar(name)
      case StringLiteral(value) => value
      case CharLiteral(value) => value // !!! implicitly converts to Int; not what we want
      case IntLiteral(value) => value
      case LongLiteral(value) => value // !!! implicitly converts to Double; not what we want
      case DoubleLiteral(value) => value
      case BooleanLiteral(value) => value
      case Null() => null
      case Undefined() => js.undefined
      case ArrayValue(_, value) => value // !!! implicitly creates a js.Function1; not what we want
      case This() => env.getThis
      case VarRef(name) => env.read(name)

      case JSLinkingInfo() => linkingInfo

      // !!! cannot ignore className
      case Select(tree, _, field) => eval(tree) match {
        case instance: Instance =>
          instance.getField(field)
        case rest => unimplemented(rest, "Select") // ~~~ I think this cannot happen in a valid program
      }

      case JSSelect(receiver, prop) =>
        val obj = eval(receiver).asInstanceOf[RawJSValue]
        val idx = eval(prop)
        obj.jsPropertyGet(idx)

      case Apply(flags, receiver, method, args) => eval(receiver) match {
        case instance: Instance => {
          val methodDef = lookupMethodDef(instance.className, method)
          eval(methodDef.body.get)(bindArgs(methodDef.args, args).setThis(instance))
        }
        /*case instance: String =>
          // ...*/
        case rest => js.typeOf(rest) match {
          case "string" =>
            val met = lookupMethodDef(BoxedStringClass, method)
            eval(met.body.get)(bindArgs(met.args, args).setThis(rest))
          case something => unimplemented(rest, s"Apply as ${something}") // !!! need to handle the other primitive types
        }
      }

      case ApplyStatically(_flags, tree, className, methodIdent, args) => eval(tree) match {
        case instance: Instance => // !!! ApplyStatically does not require an Instance; it can work with any type of value
          val methodDef = lookupMethodDef(className, methodIdent)
          eval(methodDef.body.get)(bindArgs(methodDef.args, args).setThis(instance))
        case rest => unimplemented(rest, "ApplyStatically")
      }

      case ApplyStatic(_flags, className, methodIdent, args) =>
        val methodDef = lookupMethodDef(className, methodIdent)
        eval(methodDef.body.get)(bindArgs(methodDef.args, args))

      case New(name, ctor, args) =>
        val instance = new Instance(name)
        // !!! Need to initialize the fields according to the FieldDefs in the LinkedClass and its parent classes
        val ctorDef = lookupMethodDef(name, ctor)
        eval(ctorDef.body.get)(bindArgs(ctorDef.args, args).setThis(instance))
        // !!! the `body` of a constructor is a statement with result value; so need to explicitly return `instance` here

      case LoadModule(name) => modules.getOrElse(name, { // !!! use getOrElseUpdate here, to really match the semantics
        val instance = new Instance(name)
        val initializer = lookupInitializer(name)
        eval(initializer.body.get)(env.setThis(instance))
        instance
      })

      case StoreModule(name, tree) => eval(tree) match {
        case mod: Instance => modules.update(name, mod)
        case rest => unimplemented(rest, "StoreModule") // ~~~ cannot happen in a valid program
      }

      case Assign(lhs, rhs) => lhs match {
        case VarRef(name) => {
          env.assign(name, eval(rhs))
          ()
        }
        case Select(qualifier, _, field) => eval(qualifier) match {
          case instance: Instance =>
            instance.setField(field, eval(rhs))
          case rest => unimplemented(rest, "Assign -> Select") // ~~~ cannot happen in a valid program
        }
        case JSSelect(VarRef(name), prop) => { // !!! the qualifier could be anything; it should be eval'ed
          val obj = env.read(name).asInstanceOf[RawJSValue]
          obj.jsPropertySet(eval(prop), eval(rhs))
        }
        case rest => unimplemented(rest, "Assign")
      }

      // TODO: Implement TryCatch and Error propagation
      case TryCatch(block, errVar, _, handler) =>
        try {
          eval(block)
        } catch {
          case js.JavaScriptException(e) =>
            eval(handler)(env.bind(errVar, e.asInstanceOf[js.Any]))
        }

      case If(cond, thenp, elsep) =>
        if (eval(cond).asInstanceOf[Boolean]) eval(thenp) else eval(elsep)

      case While(cond, body) =>
        while (eval(cond).asInstanceOf[Boolean]) eval(body)

      // TODO: Implement object construction
      case JSObjectConstr(List()) =>
        // println(fields)
        new js.Object()

      case AsInstanceOf(tree, tpe) => cast(eval(tree), tpe)

      case BinaryOp(BinaryOp.===, l, r) => eval(l) == eval(r) // !!! must be js.special.strictEquals(...)
      case BinaryOp(BinaryOp.!==, l, r) => eval(l) != eval(r) // !!! must be !js.special.strictEquals(...)
      case BinaryOp(BinaryOp.String_+, l, r) => //3
        // !!! Need to *convert* to string here (String_+ is special); in practice `"" + eval(l) + eval(r)` will do the right thing
        eval(l).asInstanceOf[String] + eval(r).asInstanceOf[String]
      case BinaryOp(BinaryOp.Boolean_==, l, r) => // 4
        eval(l).asInstanceOf[Boolean] == eval(r).asInstanceOf[Boolean]
      case BinaryOp(BinaryOp.Boolean_!=, l, r) => // 5
        eval(l).asInstanceOf[Boolean] != eval(r).asInstanceOf[Boolean]
      case BinaryOp(BinaryOp.Boolean_|, l, r) => // 6
        eval(l).asInstanceOf[Boolean] | eval(r).asInstanceOf[Boolean]
      case BinaryOp(BinaryOp.Boolean_&, l, r) => // 7
        eval(l).asInstanceOf[Boolean] & eval(r).asInstanceOf[Boolean]

      case BinaryOp(BinaryOp.Int_+, l, r) => // 8
        eval(l).asInstanceOf[Int] + eval(r).asInstanceOf[Int]
      case BinaryOp(BinaryOp.Int_-, l, r) => // 9
        eval(l).asInstanceOf[Int] - eval(r).asInstanceOf[Int]

      case BinaryOp(BinaryOp.Int_==, l, r) => // 19
        eval(l).asInstanceOf[Int] == eval(r).asInstanceOf[Int]
      case BinaryOp(BinaryOp.Int_!=, l, r) => // 20
        eval(l).asInstanceOf[Int] != eval(r).asInstanceOf[Int]
      case BinaryOp(BinaryOp.Int_<, l, r) => // 21
        eval(l).asInstanceOf[Int] < eval(r).asInstanceOf[Int]
      case BinaryOp(BinaryOp.Int_<=, l, r) => // 22
        eval(l).asInstanceOf[Int] <= eval(r).asInstanceOf[Int]
      case BinaryOp(BinaryOp.Int_>, l, r) => // 23
        eval(l).asInstanceOf[Int] > eval(r).asInstanceOf[Int]
      case BinaryOp(BinaryOp.Int_>=, l, r) => // 24
        eval(l).asInstanceOf[Int] >= eval(r).asInstanceOf[Int]

      case BinaryOp(BinaryOp.Double_+, l, r) => // 47
        eval(l).asInstanceOf[Double] + eval(r).asInstanceOf[Double]
      case BinaryOp(BinaryOp.Double_-, l, r) => // 48
        eval(l).asInstanceOf[Double] - eval(r).asInstanceOf[Double]
      case BinaryOp(BinaryOp.Double_*, l, r) => // 49
        eval(l).asInstanceOf[Double] * eval(r).asInstanceOf[Double]


      case BinaryOp(BinaryOp.Double_>, l, r) => // 56
        eval(l).asInstanceOf[Double] > eval(r).asInstanceOf[Double]
      case BinaryOp(BinaryOp.Double_>=, l, r) => // 57
        eval(l).asInstanceOf[Double] >= eval(r).asInstanceOf[Double]

      case UnaryOp(UnaryOp.Boolean_!, t) => !eval(t).asInstanceOf[Boolean] // 1

      // TODO: asInstanceOf[Char].toInt isn't working
      // !!! Chars, like Longs, need to be stored in their own special objects (similar to Instance)
      case UnaryOp(UnaryOp.CharToInt, t) => eval(t).asInstanceOf[Int]//.toInt // 2
      case UnaryOp(UnaryOp.ByteToInt, t) => eval(t).asInstanceOf[Byte].toInt // 3
      case UnaryOp(UnaryOp.ShortToInt, t) => eval(t).asInstanceOf[Short].toInt // 4
      case UnaryOp(UnaryOp.IntToLong, t) => eval(t).asInstanceOf[Int].toLong // 5
      case UnaryOp(UnaryOp.IntToDouble, t) => eval(t).asInstanceOf[Int].toDouble // 6
      case UnaryOp(UnaryOp.FloatToDouble, t) => eval(t).asInstanceOf[Float].toDouble // 7
      case UnaryOp(UnaryOp.IntToChar, t) => eval(t).asInstanceOf[Int].toChar // 8

      // TODO: asInstanceOf[Long] is failing with undefined behavior: not an instance of Long
      case UnaryOp(UnaryOp.LongToDouble, t) => eval(t).asInstanceOf[Int].toDouble // 14

      case rest =>
        unimplemented(rest, "root")
    }
    println("----------")
    result
  }

  def bindArgs(args: List[ParamDef], values: List[Tree])(implicit env: Env): Env = {
    // !!! should start with an empty Env here
    args.zip(values map eval).foldLeft(env) {
      case (env, (paramDef, arg)) => env.bind(paramDef.name, arg)
    }
  }

  def bar(y: Int): Int = {
    //def foo(x: Int): Int = x + 1 + y
    foo(y, y)
  }

  private def foo(x: Int, y: Int): Int = x + 1 + y

  def evalBlock(stmts: List[Tree])(implicit env: Env): js.Any = stmts match {
    case VarDef(name, _, _, _, e) :: rest =>
      evalBlock(rest)(env.bind(name, eval(e)))
    case e :: Nil =>
      eval(e)
    case e :: rest =>
      eval(e)
      evalBlock(rest)
    case Nil => ()
  }

  def lookupClassDef(name: ClassName): LinkedClass = {
    classes.get(name).get
  }

  // !!! Use MethodName's for lookups, not MethodIdent's
  def lookupMethodDef(name: ClassName, method: MethodIdent): MethodDef = {
    // !!! Need to take the MemberNamespace into account
    // !!! And if it is MemberNamespace.Public, need to search in parent classes as well; and then even in implemented interfaces
    lookupClassDef(name).methods.find(_.value.name == method).get.value
  }

  def lookupInitializer(name: ClassName): MethodDef = {
    // There can be many initializers with different signatures,
    // some more elaborate logic to pick the right one?
    // ~~~ It should be the one whose full name is NoArgConstructorName
    lookupClassDef(name).methods.find(_.value.methodName.simpleName == ConstructorSimpleName).get.value
  }

  def lookupGlobalVar(name: String): js.Any = {
    new js.Function(s"return $name;").asInstanceOf[js.Function0[js.Any]]()
  }

  def cast(value: js.Any, tpe: Type)(implicit env: Env): js.Any = tpe match {
    case ClassType(BoxedStringClass) => value.asInstanceOf[String]
    case BooleanType => value.asInstanceOf[Boolean]
    case IntType => value.asInstanceOf[Int]
    case rest => unimplemented(tpe, "CAST")(Env.empty) // !!! at worst, just return `value`; don't crash
  }

  def unimplemented(t: Any, site: String = "default")(implicit env: Env) = {
    println(s"Called at $site")
    // println(env)
    println(s"Unimplemented $t")
    ???
  }

  def dumpObj(obj: js.Any): Unit = {
    println(js.Object.entries(obj.asInstanceOf[js.Object]))
  }
}
