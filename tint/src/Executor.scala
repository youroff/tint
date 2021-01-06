package tint

import scala.collection.mutable
import scala.scalajs.js
import org.scalajs.ir.Trees._
import org.scalajs.ir.Names._
import org.scalajs.ir.Types._
import org.scalajs.linker.standard.LinkedClass
import org.scalajs.ir.Position.NoPosition
import org.scalajs.ir.ScalaJSVersions
import utils.Utils.OptionsOps
import Purifier._
import org.scalajs.ir.ClassKind._
import org.scalajs.ir.Trees.JSNativeLoadSpec.Global

class Executor(classes: Map[ClassName, LinkedClass]) {
  val modules: mutable.Map[ClassName, Instance] = mutable.Map()
  val jsClasses: mutable.Map[ClassName, js.Dynamic] = mutable.Map()
  val classInstances: mutable.Map[ClassName, Instance] = mutable.Map()
  val names = new NameGen()
  implicit val pos = NoPosition

  def execute(program: Tree): Unit = {
    eval(program)(Env.empty)
  }

  def eval(program: Tree)(implicit env: Env): js.Any = {
    // println(program)
    val result: js.Any = program match {
      case Block(trees) => evalStmts(trees)._1
      case Skip() => ()
      case StringLiteral(value) => value
      case CharLiteral(value) => new CharInstance(value)
      case IntLiteral(value) => value.intValue()
      case LongLiteral(value) => new LongInstance(value)
      case DoubleLiteral(value) => value.doubleValue()
      case BooleanLiteral(value) => value.booleanValue()
      case Null() => null
      case Undefined() => js.undefined
      case ArrayValue(typeRef, value) => ArrayInstance.fromList(typeRef, value map eval)
      case This() => env.getThis
      case VarRef(LocalIdent(name)) => env.read(name)

      case JSLinkingInfo() => scala.scalajs.runtime.linkingInfo

      case Select(tree, className, field) => eval(tree) match {
        case instance: Instance =>
          instance.getField((className, field.name))
        case rest => unimplemented(rest, "Select")
      }

      // case Select

      case ArraySelect(array, index) =>
        val instance = eval(array).asInstanceOf[ArrayInstance]
        val i = eval(index).asInstanceOf[Int]
        instance(i)

      case JSSelect(receiver, prop) =>
        val obj = eval(receiver).asInstanceOf[RawJSValue]
        val idx = eval(prop)
        obj.jsPropertyGet(idx)

      case JSSuperSelect(superClass, receiver, item) =>
        val clazz = eval(superClass).asInstanceOf[js.Dynamic]
        val propName = eval(item).asInstanceOf[String]
        val propDesc = resolvePropertyDescriptor(clazz, propName)
          .getOrThrow(s"Cannot resolve super property $propName on $clazz")
        if (propDesc.get.isDefined) {
          propDesc.get.get.call(eval(receiver))
        } else {
          propDesc.value.get.asInstanceOf[js.Any]
        }

      case Apply(flags, receiver, method, args) =>
        val instance = eval(receiver)
        val className = (instance: Any) match {
          case instance: Instance => instance.className
          case _: String => BoxedStringClass
          case rest =>
            unimplemented(rest, s"Apply className resolution")
        }
        val methodDef = lookupMethodDef(className, method.name, MemberNamespace.Public)
        val eargs = evalArgs(methodDef.args, args)
        eval(methodDef.body.get)(Env.empty.bind(eargs).setThis(instance))

      case ApplyStatically(flags, tree, className, methodIdent, args) => eval(tree) match {
        case instance: Instance =>
          val nspace = MemberNamespace.forNonStaticCall(flags)
          val methodDef = lookupMethodDef(className, methodIdent.name, nspace)
          val eargs = evalArgs(methodDef.args, args)
          eval(methodDef.body.get)(Env.empty.bind(eargs).setThis(instance))
        case rest => unimplemented(rest, "ApplyStatically")
      }

      case ApplyStatic(flags, className, methodIdent, args) =>
        val nspace = MemberNamespace.forStaticCall(flags)
        val methodDef = lookupMethodDef(className, methodIdent.name, nspace)
        val eargs = evalArgs(methodDef.args, args)
        eval(methodDef.body.get)(Env.empty.bind(eargs))
      
      case New(name, ctor, args) =>
        val instance = new Instance(name, this)
        val ctorDef = lookupMethodDef(name, ctor.name, MemberNamespace.Constructor)
        val eargs = evalArgs(ctorDef.args, args)
        eval(ctorDef.body.get)(Env.empty.bind(eargs).setThis(instance))
        instance

      case LoadModule(name) => modules.getOrElseUpdate(name, {
        val instance = new Instance(name, this)
        val initializer = lookupInitializer(name)
        eval(initializer.body.get)(env.setThis(instance))
        instance
      })

      case StoreModule(name, tree) =>
        modules.update(name, eval(tree).asInstanceOf[Instance])

      case Assign(lhs, rhs) => lhs match {
        case VarRef(LocalIdent(name)) =>
          env.assign(name, eval(rhs))

        case ArraySelect(array, index) =>
          val instance = eval(array).asInstanceOf[ArrayInstance]
          val i = eval(index).asInstanceOf[Int]
          instance(i) = eval(rhs)

        case Select(qualifier, className, field) => eval(qualifier) match {
          case instance: Instance =>
            instance.setField((className, field.name), eval(rhs))
          case rest => unimplemented(rest, "Assign -> Select")
        }

        case JSSelect(target, prop) => {
          val obj = eval(target).asInstanceOf[RawJSValue]
          obj.jsPropertySet(eval(prop), eval(rhs))
        }

        case JSSuperSelect(superClass, receiver, item) =>
          val clazz = eval(superClass).asInstanceOf[js.Dynamic]
          val propName = eval(item).asInstanceOf[String]
          val propDesc = resolvePropertyDescriptor(clazz, propName)
            .getOrThrow(s"Cannot resolve super property $propName on $clazz")
          if (propDesc.set.isDefined) {
            propDesc.set.get.call(eval(receiver), eval(rhs))
          } else {
            propDesc.value = eval(rhs)
          }

        case nonSelect =>
          throw new AssertionError(s"Selector expected in lhs of Assign, given: $nonSelect")
      }

      case TryCatch(block, err, _, handler) => try {
        eval(block)
      } catch {
        case js.JavaScriptException(e) =>
          eval(handler)(env.bind(err.name, e.asInstanceOf[js.Any]))
      }

      case TryFinally(block, finalizer) => try {
        eval(block)          
      } finally {
        eval(finalizer)
      }

      case Throw(e) =>
        new js.Function("e", "throw e;").asInstanceOf[js.Function1[js.Any, js.Any]](eval(e))

      case If(cond, thenp, elsep) =>
        if (asBoolean(eval(cond))) eval(thenp) else eval(elsep)

      case While(cond, body) =>
        while (asBoolean(eval(cond))) eval(body)

      case DoWhile(body, cond) =>
        do { eval(body) } while (asBoolean(eval(cond)))

      case ForIn(obj, key, _, body) =>
        js.special.forin(eval(obj)) { (arg) =>
          eval(body)(env.bind(key.name, arg.asInstanceOf[js.Any]))
        }

      case Match(selector, cases, default) =>
        val alt = asInt(eval(selector))
        val exp = cases.find {
          case (alts, _) => alts.contains(IntLiteral(alt))
        }.map(_._2).getOrElse(default)
        eval(exp)

      case Debugger() =>
        throw new AssertionError("Trying to debug undebuggable? :)")

      case Closure(true, captureParams, params, body, captureValues) =>
        evalJsClosure(params, body)(Env.empty.bind(evalArgs(captureParams, captureValues)))

      case Closure(false, captureParams, params, body, captureValues) =>
        evalJsFunction(params, body)(Env.empty.bind(evalArgs(captureParams, captureValues)))

      case JSObjectConstr(props) =>
        val inits = props.map {
          case (k, v) => (eval(k), eval(v))
        }
        js.special.objectLiteral(inits: _*)
 
      case JSDelete(qualifier, item) =>
        js.special.delete(eval(qualifier), eval(item))

      case JSFunctionApply(fun, args) =>
        eval(fun).asInstanceOf[js.Function].call(js.undefined, evalSpread(args): _*)

      case JSMethodApply(receiver, method, args) =>
        val obj = eval(receiver).asInstanceOf[RawJSValue]
        obj.jsMethodApply(eval(method))(evalSpread(args): _*)

      case JSGlobalRef(name) =>
        js.eval(name).asInstanceOf[js.Any]

      case JSTypeOfGlobalRef(JSGlobalRef(name)) =>
        js.eval(s"typeof $name").asInstanceOf[String]

      case JSNew(ctor, args) =>
        new js.Function("clazz", "args", s"return new clazz(...args);")
          .asInstanceOf[js.Function2[js.Any, js.Array[_], js.Any]]
          .apply(eval(ctor), js.Array(evalSpread(args): _*))

      case JSArrayConstr(items) =>
        js.Array(evalSpread(items): _*)

      case LoadJSConstructor(className) =>
        val classDef = lookupClassDef(className)
        classDef.kind match {
          case NativeJSClass => classDef.jsNativeLoadSpec.get match {
            case Global(ref, path) => eval(JSGlobalRef((path :+ ref).mkString(".")))
            case _ => unimplemented(classDef.jsNativeLoadSpec, "NativeJSClass load spec")
          }
          case JSClass =>
            initJSClass(className)
          case _ => unimplemented(classDef.kind, "LoadJSConstructor")
        }

      case JSSuperConstructorCall(_) =>
        throw new AssertionError("JSSuperConstructorCall should never be called in eval loop")

      case NewArray(typeRef, lengths) =>
        new ArrayInstance(typeRef, (lengths map eval).asInstanceOf[List[Int]])

      case ArrayLength(array) =>
        eval(array).asInstanceOf[ArrayInstance].length

      case AsInstanceOf(tree, tpe) =>
        val e = eval(tree)
        cast(e, tpe)

      case IsInstanceOf(expr, tpe) => instanceOf(eval(expr), tpe)

      case GetClass(e) => eval(e) match {
        case instance: Instance => lookupClassInstance(instance.className)
        case _ => null
      }

      case ClassOf(ClassRef(className)) =>
        lookupClassInstance(className)

      case IdentityHashCode(expr) =>
        scala.scalajs.runtime.identityHashCode(eval(expr))

      case Labeled(label, _, body) => try {
        eval(body)
      } catch {
        case LabelException(retLabel, value) if label == retLabel =>
          value
      }

      case Return(expr, label) =>
        throw LabelException(label, eval(expr))

      case BinaryOp(BinaryOp.===, l, r) => // 1
        js.special.strictEquals(eval(l), eval(r))
      case BinaryOp(BinaryOp.!==, l, r) => // 2
        !js.special.strictEquals(eval(l), eval(r))
      case BinaryOp(BinaryOp.String_+, l, r) => // 3
        "" + eval(l) + eval(r)

      case BinaryOp(BinaryOp.Boolean_==, l, r) => // 4
        asBoolean(eval(l)) == asBoolean(eval(r))
      case BinaryOp(BinaryOp.Boolean_!=, l, r) => // 5
        asBoolean(eval(l)) != asBoolean(eval(r))
      case BinaryOp(BinaryOp.Boolean_|, l, r) => // 6
        asBoolean(eval(l)) | asBoolean(eval(r))
      case BinaryOp(BinaryOp.Boolean_&, l, r) => // 7
        asBoolean(eval(l)) & asBoolean(eval(r))

      case BinaryOp(BinaryOp.Int_+, l, r) => // 8
        asInt(eval(l)) + asInt(eval(r))
      case BinaryOp(BinaryOp.Int_-, l, r) => // 9
        asInt(eval(l)) - asInt(eval(r))
      case BinaryOp(BinaryOp.Int_*, l, r) => // 10
        asInt(eval(l)) * asInt(eval(r))
      case BinaryOp(BinaryOp.Int_/, l, r) => // 11
        asInt(eval(l)) / asInt(eval(r))
      case BinaryOp(BinaryOp.Int_%, l, r) => // 12
        asInt(eval(l)) % asInt(eval(r))
      case BinaryOp(BinaryOp.Int_|, l, r) => // 13
        asInt(eval(l)) | asInt(eval(r))
      case BinaryOp(BinaryOp.Int_&, l, r) => // 14
        asInt(eval(l)) & asInt(eval(r))
      case BinaryOp(BinaryOp.Int_^, l, r) => // 15
        asInt(eval(l)) ^ asInt(eval(r))
      case BinaryOp(BinaryOp.Int_<<, l, r) => // 16
        asInt(eval(l)) << asInt(eval(r))
      case BinaryOp(BinaryOp.Int_>>>, l, r) => // 17
        asInt(eval(l)) >>> asInt(eval(r))
      case BinaryOp(BinaryOp.Int_>>, l, r) => // 18
        asInt(eval(l)) >> asInt(eval(r))
      case BinaryOp(BinaryOp.Int_==, l, r) => // 19
        asInt(eval(l)) == asInt(eval(r))
      case BinaryOp(BinaryOp.Int_!=, l, r) => // 20
        asInt(eval(l)) != asInt(eval(r))
      case BinaryOp(BinaryOp.Int_<, l, r) => // 21
        asInt(eval(l)) < asInt(eval(r))
      case BinaryOp(BinaryOp.Int_<=, l, r) => // 22
        asInt(eval(l)) <= asInt(eval(r))
      case BinaryOp(BinaryOp.Int_>, l, r) => // 23
        asInt(eval(l)) > asInt(eval(r))
      case BinaryOp(BinaryOp.Int_>=, l, r) => // 24
        asInt(eval(l)) >= asInt(eval(r))

      case BinaryOp(BinaryOp.Long_+, l, r) => // 25
        eval(l).asInstanceOf[LongInstance] >= eval(r).asInstanceOf[LongInstance]
      case BinaryOp(BinaryOp.Long_-, l, r) => // 26
        eval(l).asInstanceOf[LongInstance] - eval(r).asInstanceOf[LongInstance]
      case BinaryOp(BinaryOp.Long_*, l, r) => // 27
        eval(l).asInstanceOf[LongInstance] * eval(r).asInstanceOf[LongInstance]
      case BinaryOp(BinaryOp.Long_/, l, r) => // 28
        eval(l).asInstanceOf[LongInstance] / eval(r).asInstanceOf[LongInstance]
      case BinaryOp(BinaryOp.Long_%, l, r) => // 29
        eval(l).asInstanceOf[LongInstance] % eval(r).asInstanceOf[LongInstance]
      case BinaryOp(BinaryOp.Long_|, l, r) => // 30
        eval(l).asInstanceOf[LongInstance] | eval(r).asInstanceOf[LongInstance]
      case BinaryOp(BinaryOp.Long_&, l, r) => // 31
        eval(l).asInstanceOf[LongInstance] & eval(r).asInstanceOf[LongInstance]
      case BinaryOp(BinaryOp.Long_^, l, r) => // 32
        eval(l).asInstanceOf[LongInstance] ^ eval(r).asInstanceOf[LongInstance]
      case BinaryOp(BinaryOp.Long_<<, l, r) => // 33
        eval(l).asInstanceOf[LongInstance] << eval(r).asInstanceOf[LongInstance]
      case BinaryOp(BinaryOp.Long_>>>, l, r) => // 34
        eval(l).asInstanceOf[LongInstance] >>> eval(r).asInstanceOf[LongInstance]
      case BinaryOp(BinaryOp.Long_>>, l, r) => // 35
        eval(l).asInstanceOf[LongInstance] >> eval(r).asInstanceOf[LongInstance]
      case BinaryOp(BinaryOp.Long_==, l, r) => // 36
        eval(l).asInstanceOf[LongInstance] == eval(r).asInstanceOf[LongInstance]
      case BinaryOp(BinaryOp.Long_!=, l, r) => // 37
        eval(l).asInstanceOf[LongInstance] != eval(r).asInstanceOf[LongInstance]
      case BinaryOp(BinaryOp.Long_<, l, r) => // 38
        eval(l).asInstanceOf[LongInstance] < eval(r).asInstanceOf[LongInstance]
      case BinaryOp(BinaryOp.Long_<=, l, r) => // 39
        eval(l).asInstanceOf[LongInstance] <= eval(r).asInstanceOf[LongInstance]
      case BinaryOp(BinaryOp.Long_>, l, r) => // 40
        eval(l).asInstanceOf[LongInstance] > eval(r).asInstanceOf[LongInstance]
      case BinaryOp(BinaryOp.Long_>=, l, r) => // 41
        eval(l).asInstanceOf[LongInstance] >= eval(r).asInstanceOf[LongInstance]

      case BinaryOp(BinaryOp.Float_+, l, r) => // 42
        asFloat(eval(l)) + asFloat(eval(r))
      case BinaryOp(BinaryOp.Float_-, l, r) => // 43
        asFloat(eval(l)) - asFloat(eval(r))
      case BinaryOp(BinaryOp.Float_*, l, r) => // 44
        asFloat(eval(l)) * asFloat(eval(r))
      case BinaryOp(BinaryOp.Float_/, l, r) => // 45
        asFloat(eval(l)) / asFloat(eval(r))
      case BinaryOp(BinaryOp.Float_%, l, r) => // 46
        asFloat(eval(l)) % asFloat(eval(r))

      case BinaryOp(BinaryOp.Double_+, l, r) => // 47
        asDouble(eval(l)) + asDouble(eval(r))
      case BinaryOp(BinaryOp.Double_-, l, r) => // 48
        asDouble(eval(l)) - asDouble(eval(r))
      case BinaryOp(BinaryOp.Double_*, l, r) => // 49
        asDouble(eval(l)) * asDouble(eval(r))
      case BinaryOp(BinaryOp.Double_/, l, r) => // 50
        asDouble(eval(l)) / asDouble(eval(r))
      case BinaryOp(BinaryOp.Double_%, l, r) => // 51
        asDouble(eval(l)) % asDouble(eval(r))
      case BinaryOp(BinaryOp.Double_==, l, r) => // 52
        asDouble(eval(l)) == asDouble(eval(r))
      case BinaryOp(BinaryOp.Double_!=, l, r) => // 53
        asDouble(eval(l)) != asDouble(eval(r))
      case BinaryOp(BinaryOp.Double_<, l, r) => // 54
        asDouble(eval(l)) < asDouble(eval(r))
      case BinaryOp(BinaryOp.Double_<=, l, r) => // 55
        asDouble(eval(l)) <= asDouble(eval(r))
      case BinaryOp(BinaryOp.Double_>, l, r) => // 56
        asDouble(eval(l)) > asDouble(eval(r))
      case BinaryOp(BinaryOp.Double_>=, l, r) => // 57
        asDouble(eval(l)) >= asDouble(eval(r))

      case UnaryOp(UnaryOp.Boolean_!, t) => // 1
        !asBoolean(eval(t))
      case UnaryOp(UnaryOp.CharToInt, t) => // 2
        eval(t).asInstanceOf[CharInstance].value.toInt
      case UnaryOp(UnaryOp.ByteToInt, t) => // 3
        eval(t).asInstanceOf[Byte].toInt
      case UnaryOp(UnaryOp.ShortToInt, t) => // 4
        eval(t).asInstanceOf[Short].toInt
      case UnaryOp(UnaryOp.IntToLong, t) => // 5
        new LongInstance(asInt(eval(t)).toLong)
      case UnaryOp(UnaryOp.IntToDouble, t) => // 6
        asInt(eval(t)).toDouble
      case UnaryOp(UnaryOp.FloatToDouble, t) => // 7
        eval(t).asInstanceOf[Float].toDouble
      case UnaryOp(UnaryOp.IntToChar, t) => // 8
        new CharInstance(asInt(eval(t)).toChar)
      case UnaryOp(UnaryOp.IntToByte, t) => // 9
        asInt(eval(t)).toByte
      case UnaryOp(UnaryOp.IntToShort, t) => // 10
        asInt(eval(t)).toShort
      case UnaryOp(UnaryOp.LongToInt, t) => // 11
        eval(t).asInstanceOf[LongInstance].value.toInt
      case UnaryOp(UnaryOp.DoubleToInt, t) => // 12
        asDouble(eval(t)).toInt
      case UnaryOp(UnaryOp.DoubleToFloat, t) => // 13
        asDouble(eval(t)).toFloat
      case UnaryOp(UnaryOp.LongToDouble, t) => // 14
        eval(t).asInstanceOf[LongInstance].value.toDouble
      case UnaryOp(UnaryOp.DoubleToLong, t) => // 15
        new LongInstance(asDouble(eval(t)).toLong)

      case JSBinaryOp(JSBinaryOp.===, l, r) => // 1
        js.special.strictEquals(eval(l), eval(r))
      case JSBinaryOp(JSBinaryOp.!==, l, r) => // 2
        !js.special.strictEquals(eval(l), eval(r))
      case JSBinaryOp(JSBinaryOp.+, l, r) => // 3
        eval(l).asInstanceOf[js.Dynamic] + eval(r).asInstanceOf[js.Dynamic]
      case JSBinaryOp(JSBinaryOp.-, l, r) => // 4
        eval(l).asInstanceOf[js.Dynamic] - eval(r).asInstanceOf[js.Dynamic]
      case JSBinaryOp(JSBinaryOp.*, l, r) => // 5
        eval(l).asInstanceOf[js.Dynamic] * eval(r).asInstanceOf[js.Dynamic]
      case JSBinaryOp(JSBinaryOp./, l, r) => // 6
        eval(l).asInstanceOf[js.Dynamic] / eval(r).asInstanceOf[js.Dynamic]
      case JSBinaryOp(JSBinaryOp.%, l, r) => // 7
        eval(l).asInstanceOf[js.Dynamic] % eval(r).asInstanceOf[js.Dynamic]
      case JSBinaryOp(JSBinaryOp.|, l, r) => // 8
        eval(l).asInstanceOf[js.Dynamic] | eval(r).asInstanceOf[js.Dynamic]
      case JSBinaryOp(JSBinaryOp.&, l, r) => // 9
        eval(l).asInstanceOf[js.Dynamic] & eval(r).asInstanceOf[js.Dynamic]
      case JSBinaryOp(JSBinaryOp.^, l, r) => // 10
        eval(l).asInstanceOf[js.Dynamic] ^ eval(r).asInstanceOf[js.Dynamic]
      case JSBinaryOp(JSBinaryOp.<<, l, r) => // 11
        eval(l).asInstanceOf[js.Dynamic] << eval(r).asInstanceOf[js.Dynamic]
      case JSBinaryOp(JSBinaryOp.>>, l, r) => // 12
        eval(l).asInstanceOf[js.Dynamic] >> eval(r).asInstanceOf[js.Dynamic]
      case JSBinaryOp(JSBinaryOp.>>>, l, r) => // 13
        eval(l).asInstanceOf[js.Dynamic] >>> eval(r).asInstanceOf[js.Dynamic]
      case JSBinaryOp(JSBinaryOp.<, l, r) => // 14
        eval(l).asInstanceOf[js.Dynamic] < eval(r).asInstanceOf[js.Dynamic]
      case JSBinaryOp(JSBinaryOp.<=, l, r) => // 15
        eval(l).asInstanceOf[js.Dynamic] <= eval(r).asInstanceOf[js.Dynamic]
      case JSBinaryOp(JSBinaryOp.>, l, r) => // 16
        eval(l).asInstanceOf[js.Dynamic] > eval(r).asInstanceOf[js.Dynamic]
      case JSBinaryOp(JSBinaryOp.>=, l, r) => // 17
        eval(l).asInstanceOf[js.Dynamic] >= eval(r).asInstanceOf[js.Dynamic]
      case JSBinaryOp(JSBinaryOp.&&, l, r) => // 18
        eval(l).asInstanceOf[js.Dynamic] && eval(r).asInstanceOf[js.Dynamic]
      case JSBinaryOp(JSBinaryOp.||, l, r) => // 19
        eval(l).asInstanceOf[js.Dynamic] || eval(r).asInstanceOf[js.Dynamic]
      case JSBinaryOp(JSBinaryOp.in, l, r) => // 20
        js.special.in(eval(l), eval(r))
      case JSBinaryOp(JSBinaryOp.instanceof, l, r) => // 21
        js.special.instanceof(eval(l), eval(r))

      case JSUnaryOp(JSUnaryOp.+, t) => // 1
        eval(t).asInstanceOf[js.Dynamic].unary_+
      case JSUnaryOp(JSUnaryOp.-, t) => // 2
        eval(t).asInstanceOf[js.Dynamic].unary_-
      case JSUnaryOp(JSUnaryOp.~, t) => // 3
        eval(t).asInstanceOf[js.Dynamic].unary_~
      case JSUnaryOp(JSUnaryOp.!, t) => // 4
        eval(t).asInstanceOf[js.Dynamic].unary_!
      case JSUnaryOp(JSUnaryOp.typeof, t) => // 5
        js.typeOf(eval(t))

      case rest =>
        unimplemented(rest, "root")
    }
    // println("EVAL======")
    // println("Env: " + env)
    // println(s"Result: $result")
    // println("----------")
    result
  }

  def evalArgs(args: List[ParamDef], values: List[Tree])(implicit env: Env): Map[LocalName, js.Any] = {
    args.map(_.name.name).zip(values map eval).toMap
  }

  /**
  * TODO: Return js.Array as vast use cases expect that
  */
  def evalSpread(args: List[TreeOrJSSpread])(implicit env: Env): List[js.Any] = args flatMap {
    case t: Tree => List(eval(t))
    case JSSpread(items) => eval(items).asInstanceOf[js.Array[js.Any]].toList
  }

  def evalStmts(stmts: List[Tree])(implicit initialEnv: Env): (js.Any, Env) = {
    var result: js.Any = js.undefined
    var env = initialEnv
    stmts.foreach {
      case VarDef(LocalIdent(name), _, _, _, e) =>
        result = js.undefined
        env = env.bind(name, eval(e)(env))
      case anything =>
        result = eval(anything)(env)
    }
    (result, env)
  }

  def evalJsFunction(params: List[ParamDef], body: Tree)(implicit env: Env): js.Any = {
    val call: js.Function2[js.Any, js.Array[js.Any], js.Any] = { (thizz, args) =>
      val argsMap = params.map(_.name.name).zip(args).toMap
      eval(body)(env.bind(argsMap).setThis(thizz))
    }
    new js.Function("body", "return function(...args) { return body(this, args); };")
      .asInstanceOf[js.Function1[js.Function, js.Any]].apply(call)
  }

  def evalJsClosure(params: List[ParamDef], body: Tree)(implicit env: Env): js.Any = {
    val call: js.Function1[js.Array[js.Any], js.Any] = { (args) =>
      val argsMap = params.map(_.name.name).zip(args).toMap
      eval(body)(env.bind(argsMap))
    }
    new js.Function("body", "return (...args) => { return body(args); };")
      .asInstanceOf[js.Function1[js.Function, js.Any]].apply(call)
  }

  def evalPropertyDescriptor(desc: JSPropertyDef)(implicit env: Env): js.PropertyDescriptor = {    
    js.Dynamic.literal(
      get = desc.getterBody.map { body =>
        { (thiz) => eval(body)(env.setThis(thiz)) } : js.ThisFunction0[js.Any, js.Any]
      }.getOrElse(js.undefined),
      set = desc.setterArgAndBody.map {
        case (param, body) => { (thiz: js.Any, arg: js.Any) =>
          eval(body)(env.bind(param.name.name, arg).setThis(thiz))
        } : js.ThisFunction1[js.Any, js.Any, js.Any]
      }.getOrElse(js.undefined)
    ).asInstanceOf[js.PropertyDescriptor]
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

  def lookupClassInstance(className: ClassName): Instance = {
    classInstances.getOrElseUpdate(className, {
      val tmp = LocalName("dataTmp")
      eval(New(
        ClassClass,
        MethodIdent(MethodName(ConstructorSimpleName, List(ClassRef(ObjectClass)), VoidRef)),
        List(VarRef(LocalIdent(tmp))(AnyType))
      ))(Env.empty.bind(tmp, genTypeData(className))).asInstanceOf[Instance]
    })
  }

  def genTypeData(className: ClassName): js.Any = {
    val classDef = lookupClassDef(className)
    val args = js.Array[js.Any](
      js.special.objectLiteral((names.genName(className), 0)),
      classDef.kind == Interface,
      classDef.fullName, // Something else needed here?
      js.special.objectLiteral(
        classDef.ancestors
          .map(names.genName(_))
          .map((_, 1)): _*
      )
    )
    new js.Function("args", "return new $TypeData().initClass(...args);")
      .asInstanceOf[js.Function1[js.Array[js.Any], js.Any]](args)
  }

  def cast(value: js.Any, tpe: Type)(implicit env: Env): js.Any = tpe match {
    case ClassType(BoxedStringClass) => value.asInstanceOf[String]
    case BooleanType => value.asInstanceOf[Boolean]
    case IntType => value.asInstanceOf[Int]
    case _ => value
  } 

  def instanceOf(value: js.Any, tpe: Type): js.Any = (value, tpe) match {
    case (instance: Instance, ClassType(ObjectClass)) => true
    case (instance: Instance, ClassType(className)) =>
      isSubclassOf(instance.className, className)
    case (_, AnyType) => true
    case _ =>
      unimplemented((value, tpe), "instanceOf")
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

  /** 
   * Assuming `linkedClass` is a JSClass, find JSMethodDef named `constructor`
  */
  def lookupJSConstructor(linkedClass: LinkedClass): JSMethodDef = {
    linkedClass.exportedMembers
      .map(_.value)
      .find {
        case JSMethodDef(_, StringLiteral("constructor"), _, _) => true
        case _ => false
      }.getOrThrow(s"Cannot find constructor in ${linkedClass.className} exportedMembers")
      .asInstanceOf[JSMethodDef]
  }

  /** Split constructor body into prelude, args tree and epilog
   * This function automatically checks invariant (either):
   * - JSSuperConstructorCall(args)
   * - Block(..., JSSuperConstructorCall(args), ...)
  */
  def splitJSConstructor(tree: Tree): (List[Tree], List[TreeOrJSSpread], List[Tree]) = tree match {
    case JSSuperConstructorCall(args) => (Nil, args, Nil)
    case Block(stmts) =>
      val ctor = stmts.find {
        case JSSuperConstructorCall(_) => true
        case _ => false
      }.getOrThrow("Invariant violation: JSConstructor block doesn't have JSSuperConstructorCall")
        .asInstanceOf[JSSuperConstructorCall]
      val (prelude, _::epilog) = stmts.splitAt(stmts.indexOf(ctor))
      (prelude, ctor.args, epilog)
    case _ =>
      throw new AssertionError("Invariant violation: JSConstructor is neither Block nor JSSuperConstructorCall")
  }

  /** Generates JSClass value
    */
  def initJSClass(className: ClassName): js.Dynamic = {
    jsClasses.getOrElseUpdate(className, {
      val linkedClass = lookupClassDef(className) 
      val jsClass = names.genName(className)
      val jsSuperClass = linkedClass.superClass.map { superClass =>
        if (superClass.name == ClassName("scala.scalajs.js.Object")) {
          "Object"
        } else {
          initJSClass(superClass.name)
          names.genName(superClass.name)
        }
      }.getOrThrow("JSClass must have a super class")

      val ctorDef = lookupJSConstructor(linkedClass)
      val (preludeTree, superArgs, epilogTree) = splitJSConstructor(ctorDef.body)
      
      val prelude = { (args: js.Array[js.Any]) =>
        val argsMap = ctorDef.args.map(_.name.name).zip(args).toMap
        evalStmts(preludeTree)(Env.empty.bind(argsMap))._2
      } : js.Function1[js.Array[js.Any], Env]

      val evalSuperArgs = { (env: Env) =>
        js.Array(evalSpread(superArgs)(env): _*)
      } : js.Function1[Env, js.Array[js.Any]]

      val epilog = { (thiz: js.Object, env: Env) =>
        linkedClass.fields.foreach {
          case JSFieldDef(flags, StringLiteral(field), tpe) =>
            val descriptor = js.Dynamic.literal(
              configurable = true,
              enumerable = true,
              writable = true,
              value = Types.zeroOf(tpe)
            ).asInstanceOf[js.PropertyDescriptor]
            js.Object.defineProperty(thiz, field, descriptor)
          case _ =>
            throw new Exception("Only JSFieldDefs are allowed at JSClasses")  
        }

        eval(Block(epilogTree))(env.setThis(thiz))
      } : js.Function2[js.Object, Env, js.Any]

      val extending = { () =>
        linkedClass.superClass.map { superClass =>
          if (superClass.name == ClassName("scala.scalajs.js.Object")) {
            js.constructorOf[js.Object]
          } else {
            initJSClass(superClass.name)
          }
        }.getOrThrow("JSClass must have a super class").asInstanceOf[js.Object]
      } : js.Function0[js.Object]

      val classInstance = new js.Function(
        s"prelude_$jsClass",
        s"evalSuperArgs_$jsClass",
        s"epilog_$jsClass",
        s"super_$jsClass",
        s"""return class $jsClass extends super_$jsClass() {
          constructor(...args) {
            const env = prelude_$jsClass(args);
            const superArgs = evalSuperArgs_$jsClass(env)
            super(...superArgs);
            epilog_$jsClass(this, env);
          }
        };"""
      ).asInstanceOf[js.Function4[js.Function, js.Function, js.Function, js.Function, js.Any]].apply(
        prelude,
        evalSuperArgs,
        epilog,
        extending
      ).asInstanceOf[js.Dynamic]

      val prototype = classInstance.selectDynamic("prototype")
      linkedClass.exportedMembers.map(_.value).foreach {
        case desc @ JSPropertyDef(flags, StringLiteral(name), _, _) =>
          val descriptor = evalPropertyDescriptor(desc)(Env.empty)
          js.Object.defineProperty(prototype.asInstanceOf[js.Object], name, descriptor)
        case JSMethodDef(flags, StringLiteral(name), args, body) =>
          prototype.updateDynamic(name)(evalJsFunction(args, body)(Env.empty))
      }
      classInstance
    })
  }

  def resolvePropertyDescriptor(clazz: js.Dynamic, prop: String): Option[js.PropertyDescriptor] = {
    var superProto = clazz.selectDynamic("prototype").asInstanceOf[js.Object]
    while (superProto != null) {
      val desc = js.Object.getOwnPropertyDescriptor(superProto, prop)
      if (desc != null) {
        return Some(desc)
      }
      superProto = js.Object.getPrototypeOf(superProto)
    }
    None
  }

  def unimplemented(t: Any, site: String = "default") = {
    println(s"Unimplemented $t")
    ???
  }

  def dumpObj(obj: js.Any): Unit = {
    println(js.Object.entries(obj.asInstanceOf[js.Object]))
  }
  
  def p(obj: js.Any) = {
    js.Dynamic.global.console.log(obj)
  }
}
