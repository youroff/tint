package tint

import scala.scalajs.js
import org.scalajs.ir.Types._

object Types {

  def zeroOf(tpe: Type): js.Any = tpe match {
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

  def zeroOfRef(ref: NonArrayTypeRef): js.Any = ref match {
    case BooleanRef => false
    case CharRef    => new CharInstance('\u0000')
    case ByteRef    => 0
    case ShortRef   => 0
    case IntRef     => 0
    case LongRef    => new LongInstance(0L)
    case FloatRef   => 0.0f
    case DoubleRef  => 0.0
    case _          => null
  }

  def asBoolean(value: Any): Boolean = value match {
    case _: Boolean => value.asInstanceOf[Boolean]
    case _ => throw new Error("Interpreter Error: Not a Boolean")
  }

  def asByte(value: Any): Byte = value match {
    case _: Byte => value.asInstanceOf[Byte]
    case _ => throw new Error("Interpreter Error: Not a Byte")
  }

  def asShort(value: Any): Short = value match {
    case _: Short => value.asInstanceOf[Short]
    case _ => throw new Error("Interpreter Error: Not a Short")
  }

  def asInt(value: Any): Int = value match {
    case _: Int => value.asInstanceOf[Int]
    case _ => throw new Error("Interpreter Error: Not an Int")
  }

  def asLong(value: Any): LongInstance = value match {
    case _: LongInstance => value.asInstanceOf[LongInstance]
    case _ => throw new Error(s"Interpreter Error: Not a Long but ${js.typeOf(value)}")
  }

  def asChar(value: Any): CharInstance = value match {
    case _: CharInstance => value.asInstanceOf[CharInstance]
    case _ => throw new Error(s"Interpreter Error: Not a Char but ${js.typeOf(value)}")
  }

  def asFloat(value: Any): Float = value match {
    case _: Float => value.asInstanceOf[Float]
    case _ => throw new Error("Interpreter Error: Not a Float")
  }

  def asDouble(value: Any): Double = value match {
    case _: Double => value.asInstanceOf[Double]
    case _ => throw new Error("Interpreter Error: Not a Double")
  }
}
