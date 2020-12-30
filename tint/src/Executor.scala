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
  val jsClasses: mutable.Map[ClassName, js.Any] = mutable.Map()
  val classInstances: mutable.Map[ClassName, Instance] = mutable.Map()
  val names = new NameGen()
  implicit val pos = NoPosition

  def execute(program: Tree): Unit = {
    initJSClasses
    eval(program)(Env.empty)
  }

  def eval(program: Tree)(implicit env: Env): js.Any = {
    // println(program)
    val result: js.Any = program match {
      case Block(trees) => evalBlock(trees)
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

      case ArraySelect(array, index) =>
          val instance = eval(array).asInstanceOf[ArrayInstance]
          val i = eval(index).asInstanceOf[Int]
          instance(i)

      case JSSelect(receiver, prop) =>
        val obj = eval(receiver).asInstanceOf[RawJSValue]
        val idx = eval(prop)
        obj.jsPropertyGet(idx)

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

        case rest => unimplemented(rest, "Assign")
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

      case Match(selector, cases, default) =>
        val alt = asInt(eval(selector))
        val exp = cases.find {
          case (alts, _) => alts.contains(IntLiteral(alt))
        }
          .map(_._2)
          .getOrElse(default)
        eval(exp)

      case Closure(arrow, captureParams, params, body, captureValues) =>
        val captures = evalArgs(captureParams, captureValues)
        val call: js.Function1[js.Array[js.Any], js.Any] = { args =>
          val argsMap = params.map(_.name.name).zip(args).toMap
          eval(body)(Env.empty.bind(captures).bind(argsMap))
        }
        new js.Function("body", "return function(...args) { return body(args); };")
          .asInstanceOf[js.Function1[js.Function, js.Any]].apply(call)

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
        new js.Function(s"return $name;").asInstanceOf[js.Function0[js.Any]]()

      case JSTypeOfGlobalRef(JSGlobalRef(name)) =>
        new js.Function(s"return typeof $name;").asInstanceOf[js.Function0[String]]()

      case JSNew(ctor, args) =>
        val eargs = evalSpread(args)
        val clazz = eval(ctor).asInstanceOf[js.Dynamic]
        js.Dynamic.newInstance(clazz)(eargs: _*)

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
            jsClasses.get(className).getOrThrow(s"No $className in JS Class cache")
          case _ => unimplemented(classDef.kind, "LoadJSConstructor")
        }

      case JSSuperConstructorCall(args) =>
        // println("PROTO CALL")
        // println(env)
        ()

      case NewArray(typeRef, lengths) =>
        new ArrayInstance(typeRef, (lengths map eval).asInstanceOf[List[Int]])

      case ArrayLength(array) =>
        eval(array).asInstanceOf[ArrayInstance].length

      case AsInstanceOf(tree, tpe) =>
        val e = eval(tree)
        cast(e, tpe)

      case IsInstanceOf(expr, tpe) => instanceOf(eval(expr), tpe)

      case GetClass(e) => eval(e) match {
        case instance: Instance => lookupClassInstance(instance)
      }

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

  def evalSpread(args: List[TreeOrJSSpread])(implicit env: Env): List[js.Any] = args flatMap {
    case t: Tree => List(eval(t))
    case JSSpread(items) => eval(items).asInstanceOf[js.Array[js.Any]].toList
  }

  def evalBlock(stmts: List[Tree])(implicit env: Env): js.Any = {
    stmts match {
      case VarDef(LocalIdent(name), _, _, _, e) :: rest => 
        val result = eval(e)
        // p(result)
        evalBlock(rest)(env.bind(name, result))
      case e :: Nil =>
        eval(e)
      case e :: rest =>
        eval(e)
        evalBlock(rest)
      case Nil => ()
    }
  }

  def evalJsMethodBody(params: List[ParamDef], body: Tree)(implicit env: Env): js.Any = {
    val call: js.Function1[js.Array[js.Any], js.Any] = { args =>
      val argsMap = params.map(_.name.name).zip(args).toMap
      eval(body)(env.bind(argsMap))
    }
    new js.Function("body", "return function(...args) { return body(args); };")
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

  def lookupClassInstance(instance: Instance): Instance = {
    classInstances.getOrElseUpdate(instance.className, {
      val tmp = LocalName("dataTmp")
      eval(New(
        ClassClass,
        MethodIdent(MethodName(ConstructorSimpleName, List(ClassRef(ObjectClass)), VoidRef)),
        List(VarRef(LocalIdent(tmp))(AnyType))
      ))(Env.empty.bind(tmp, genTypeData(instance.className))).asInstanceOf[Instance]
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
    case _ =>
      unimplemented((value, tpe), "instanceOf")
  }

  def isSubclassOf(lhs: ClassName, rhs: ClassName): Boolean = {
    val classDef = lookupClassDef(lhs)
    lhs.equals(rhs) ||
    classDef.superClass.map(_.name).map(isSubclassOf(_, rhs)).getOrElse(false) ||
    classDef.interfaces.map(_.name).exists(isSubclassOf(_, rhs))
  }

  def initJSClasses = {
    for ((className, linkedClass) <- classes if linkedClass.kind == JSClass) {
      val jsName = names.genName(className)
      val classInstance = new js.Function(
        s"""return class $jsName {

        };"""
      ).asInstanceOf[js.Function0[js.Any]]().asInstanceOf[js.Dynamic]

      linkedClass.fields.foreach {
        case JSFieldDef(flags, StringLiteral(field), tpe) =>
          classInstance.updateDynamic(field)(Types.zeroOf(tpe))
        case _ =>
          throw new Exception("Only JSFieldDefs allowed with JSClasses")
      }

      // linkedClass.jsClassCaptures.foreach(println(_))
      linkedClass.exportedMembers.map(_.value).foreach {
        case JSMethodDef(flags, StringLiteral(name), args, body) =>
          val methodBody = evalJsMethodBody(args, body)(Env.empty.setThis(classInstance))
          classInstance.updateDynamic(name)(methodBody)
      }
      jsClasses.put(className, classInstance)
    }
  }

  def unimplemented(t: Any, site: String = "default") = {
    // p(t.asInstanceOf[js.Any])
    // println(s"Called at $site")
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
