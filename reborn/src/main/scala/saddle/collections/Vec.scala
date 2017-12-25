package saddle.collections

import scala.reflect.ClassTag

import breeze.linalg._
import breeze.linalg.{sum => bsum}
import breeze.math._
import breeze.storage.Zero

class Vec[@specialized(Double, Int, Float, Long, Boolean) T](private val back: DenseVector[T]) {

  def sum()(implicit impl: bsum.Impl[DenseVector[T], T]): T = bsum(back)

  override def toString: String = back.valuesIterator.mkString("Vec(", ", ", ")")

}

object Vec extends App {
  def zeros[T: ClassTag: Zero](n: Int): Vec[T]    = new Vec[T](DenseVector.zeros[T](n))
  def ones[T: ClassTag: Semiring](n: Int): Vec[T] = new Vec[T](DenseVector.ones[T](n))
}
