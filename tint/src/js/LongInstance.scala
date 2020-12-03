package tint

import scala.scalajs.js

class LongInstance(val value: Long) extends js.Object {

  def +(that: LongInstance): LongInstance = new LongInstance(value + that.value)
  def -(that: LongInstance): LongInstance = new LongInstance(value - that.value)
  def *(that: LongInstance): LongInstance = new LongInstance(value * that.value)
  def /(that: LongInstance): LongInstance = new LongInstance(value / that.value)
  def %(that: LongInstance): LongInstance = new LongInstance(value % that.value)
  def <<(that: LongInstance): LongInstance = new LongInstance(value << that.value)
  def >>(that: LongInstance): LongInstance = new LongInstance(value >> that.value)
  def >>>(that: LongInstance): LongInstance = new LongInstance(value >>> that.value)
  def &(that: LongInstance): LongInstance = new LongInstance(value & that.value)
  def |(that: LongInstance): LongInstance = new LongInstance(value | that.value)
  def ^(that: LongInstance): LongInstance = new LongInstance(value ^ that.value)

  def <(that: LongInstance): Boolean = value < that.value
  def >(that: LongInstance): Boolean = value > that.value
  def <=(that: LongInstance): Boolean = value <= that.value
  def >=(that: LongInstance): Boolean = value >= that.value

  override def toString(): String = s"LongInstance($value)"
}
