package tint
package ops

import scala.scalajs.js
import org.scalajs.ir.Trees.UnaryOp._
import Types._

object UnaryOps {

  def apply(op: Code, t: js.Any): js.Any = op match {
    case Boolean_! => !asBoolean(t)
    case CharToInt => t.asInstanceOf[CharInstance].value.toInt
    case ByteToInt => t.asInstanceOf[Byte].toInt
    case ShortToInt => t.asInstanceOf[Short].toInt
    case IntToLong => new LongInstance(asInt(t).toLong)
    case IntToDouble => asInt(t).toDouble
    case FloatToDouble => t.asInstanceOf[Float].toDouble
    case IntToChar => new CharInstance(asInt(t).toChar)
    case IntToByte => asInt(t).toByte
    case IntToShort => asInt(t).toShort
    case LongToInt => t.asInstanceOf[LongInstance].value.toInt
    case DoubleToInt => asDouble(t).toInt
    case DoubleToFloat => asDouble(t).toFloat
    case LongToDouble => t.asInstanceOf[LongInstance].value.toDouble
    case DoubleToLong => new LongInstance(asDouble(t).toLong)
  }
}
