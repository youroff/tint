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
}
