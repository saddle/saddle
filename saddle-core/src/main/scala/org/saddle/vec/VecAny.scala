/**
 * Copyright (c) 2013 Saddle Development Team
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 **/

package org.saddle.vec

import scala.{specialized => spec}
import scala.Predef._
import org.saddle._
import org.saddle.scalar._
import util.Concat.Promoter

/**
 * Vec of Any
 */
class VecAny[T: ST](values: Array[T]) extends Vec[T] { self =>
  def length = values.length

  def scalarTag = implicitly[ST[T]]

  def apply(i: Int): T = values(i)

  def copy: Vec[T] = Vec(toArray.clone())

  def take(locs: Array[Int]): Vec[T] = array.take(toArray, locs, scalarTag.missing)

  def without(locs: Array[Int]): Vec[T] = array.remove(toArray, locs)

  def dropNA: Vec[T] = filter(_ => true)

  def hasNA: Boolean = VecImpl.findOneNA(this)

  def unary_-(): Vec[T] = sys.error("Cannot negate AnyVec")

  def concat[B, C](v: Vec[B])(implicit wd: Promoter[T, B, C], mc: ST[C]): Vec[C] =
    Vec(util.Concat.append[T, B, C](toArray, v.toArray))

  def foldLeft[@spec(Boolean, Int, Long, Double) B: ST](init: B)(f: (B, T) => B): B =
    VecImpl.foldLeft(this)(init)(f)

  def foldLeftWhile[@spec(Boolean, Int, Long, Double) B: ST](init: B)(f: (B, T) => B)(cond: (B, T) => Boolean): B =
    VecImpl.foldLeftWhile(this)(init)(f)(cond)

  def filterFoldLeft[@spec(Boolean, Int, Long, Double) B: ST](pred: T => Boolean)(init: B)(f: (B, T) => B): B =
    VecImpl.filterFoldLeft(this)(pred)(init)(f)

  def filterScanLeft[@spec(Boolean, Int, Long, Double) B: ST](pred: (T) => Boolean)(init: B)(f: (B, T) => B): Vec[B] =
    VecImpl.filterScanLeft(this)(pred)(init)(f)

  def rolling[@spec(Boolean, Int, Long, Double) B: ST](winSz: Int, f: Vec[T] => B): Vec[B] =
    VecImpl.rolling(this)(winSz, f)

  def map[@spec(Boolean, Int, Long, Double) B: ST](f: T => B): Vec[B] = VecImpl.map(this)(f)

  def flatMap[@spec(Boolean, Int, Long, Double) B : ST](f: T => Vec[B]): Vec[B] = VecImpl.flatMap(this)(f)

  def scanLeft[@spec(Boolean, Int, Long, Double) B: ST](init: B)(f: (B, T) => B): Vec[B] = VecImpl.scanLeft(this)(init)(f)

  def zipMap[@spec(Int, Long, Double) B: ST, @spec(Boolean, Int, Long, Double) C: ST](other: Vec[B])(f: (T, B) => C): Vec[C] =
    VecImpl.zipMap(this, other)(f)

  def slice(from: Int, until: Int, stride: Int = 1) = {
    val b = math.max(from, 0)
    val e = math.min(until, self.length)

    if (e <= b) Vec.empty else new VecAny(values) {
      private val ub = math.min(self.length, e)

      override def length = math.ceil((ub - b) / stride.toDouble).toInt

      override def apply(i: Int): T = {
        val loc = b + i * stride
        if (loc >= ub)
          throw new ArrayIndexOutOfBoundsException("Cannot access location %d >= length %d".format(loc, ub))
        self.apply(loc)
      }

      override def needsCopy = true
    }
  }

  // ex. shift(1)  : [1 2 3 4] => [NA 1 2 3]
  //     shift(-1) : [1 2 3 4] => [2 3 4 NA]
  def shift(n: Int) = {
    val m = math.min(n, self.length)
    val b = -m
    val e = self.length - m

    new VecAny(values) {
      override def length = self.length

      override def apply(i: Int): T = {
        val loc = b + i
        if (loc >= e || loc < b)
          throw new ArrayIndexOutOfBoundsException("Cannot access location %d (vec length %d)".format(i, self.length))
        else if (loc >= self.length || loc < 0)
          scalarTag.missing
        else
          self.apply(loc)
      }

      override def needsCopy = true
    }
  }

  private[saddle] def toArray: Array[T] = {
    // need to check if we're a view on an array
    if (!needsCopy)
      values
    else {
      val buf = new Array[T](length)
      var i = 0
      while( i < length ) {
        buf(i) = apply(i)
        i += 1
      }
      buf
    }
  }

  /** Default equality does an iterative, element-wise equality check of all values. */
  override def equals(o: Any): Boolean = o match {
    case rv: VecAny[_] => (this eq rv) || (this.length == rv.length) && {
      var i = 0
      var eq = true
      while(eq && i < this.length) {
        eq &&= (apply(i) == rv(i) || this.scalarTag.isMissing(apply(i)) && rv.scalarTag.isMissing(rv(i)))
        i += 1
      }
      eq
    }
    case _ => super.equals(o)
  }
}
