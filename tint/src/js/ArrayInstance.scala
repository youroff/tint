package tint

import scala.collection.mutable
import scala.scalajs.js
import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._
import scala.scalajs.js.annotation.JSName

class ArrayInstance(typeRef: ArrayTypeRef, lengths: List[Int]) extends js.Object {
  require(typeRef.dimensions >= lengths.size && lengths.size > 0, "dimensions don't match")
  
  val array = new Array[js.Any](lengths.head)

  for (i <- 0 until lengths.head) {
    array(i) = if (lengths.tail.isEmpty) {
      if (typeRef.dimensions > 1) null else zeroOf(typeRef.base)
    } else {
      new ArrayInstance(ArrayTypeRef(typeRef.base, typeRef.dimensions - 1), lengths.tail)
    }
  }

  @JSName("apply")
  def apply(index: Int): js.Any = array(index)

  def update(index: Int, v: js.Any) = array(index) = v

  private def zeroOf(ref: NonArrayTypeRef): js.Any = ref match {
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

  def length = array.length
}

object ArrayInstance {
  def fromList(typeRef: ArrayTypeRef, list: List[js.Any]): ArrayInstance = {
    val instance = new ArrayInstance(ArrayTypeRef(typeRef.base, 1), List(list.size))    
    list.zipWithIndex.foreach {
      case (element, i) => instance.array(i) = element
    }
    instance
  }
}
