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
import org.saddle._
import org.saddle.util
import org.saddle.util.Concat.Promoter
import org.saddle.scalar._

class VecLong(values: Array[Long]) extends Vec[Long] { self =>
  def length = values.length

  def scalarTag = ScalarTagLong

  def apply(i: Int): Long = values(i)

  def copy: Vec[Long] = Vec(toArray.clone())

  def take(locs: Array[Int]): Vec[Long] = array.take(toArray, locs, scalarTag.missing)

  def without(locs: Array[Int]): Vec[Long] = array.remove(toArray, locs)

  def dropNA: Vec[Long] = filter(_ => true)

  def hasNA: Boolean = VecImpl.findOneNA(this)

  def unary_-(): Vec[Long] = map(-_)

  def concat[B, C](v: Vec[B])(implicit wd: Promoter[Long, B, C], mc: ST[C]): Vec[C] =
    Vec(util.Concat.append[Long, B, C](toArray, v.toArray))

  def foldLeft[@spec(Boolean, Int, Long, Double) B: ST](init: B)(f: (B, Long) => B): B =
    VecImpl.foldLeft(this)(init)(f)

  def foldLeftWhile[@spec(Boolean, Int, Long, Double) B: ST](init: B)(f: (B, Long) => B)(cond: (B, Long) => Boolean): B =
    VecImpl.foldLeftWhile(this)(init)(f)(cond)

  def filterFoldLeft[@spec(Boolean, Int, Long, Double) B: ST](pred: Long => Boolean)(init: B)(f: (B, Long) => B): B =
    VecImpl.filterFoldLeft(this)(pred)(init)(f)

  def filterScanLeft[@spec(Boolean, Int, Long, Double) B: ST](pred: (Long) => Boolean)(init: B)(f: (B, Long) => B): Vec[B] =
    VecImpl.filterScanLeft(this)(pred)(init)(f)

  def rolling[@spec(Boolean, Int, Long, Double) B: ST](winSz: Int, f: Vec[Long] => B): Vec[B] =
    VecImpl.rolling(this)(winSz, f)

  def map[@spec(Boolean, Int, Long, Double) B: ST](f: Long => B): Vec[B] = VecImpl.map(this)(f)

  def flatMap[@spec(Boolean, Int, Long, Double) B : ST](f: Long => Vec[B]): Vec[B] = VecImpl.flatMap(this)(f)

  def scanLeft[@spec(Boolean, Int, Long, Double) B: ST](init: B)(f: (B, Long) => B): Vec[B] = VecImpl.scanLeft(this)(init)(f)

  def zipMap[@spec(Int, Long, Double) B: ST, @spec(Boolean, Int, Long, Double) C: ST](other: Vec[B])(f: (Long, B) => C): Vec[C] =
    VecImpl.zipMap(this, other)(f)

  def slice(from: Int, until: Int, stride: Int = 1) = {
    val b = math.max(from, 0)
    val e = math.min(until, self.length)

    if (e <= b) Vec.empty else new VecLong(values) {
      private val ub = math.min(self.length, e)

      override def length = math.ceil((ub - b) / stride.toDouble).toInt

      override def apply(i: Int): Long = {
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

    new VecLong(values) {
      override def length = self.length

      override def apply(i: Int): Long = {
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

  private[saddle] def toArray: Array[Long] = {
    // need to check if we're a view on an array
    if (!needsCopy)
      values
    else {
      val buf = new Array[Long](length)
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
    case rv: VecLong => (this eq rv) || (this.length == rv.length) && {
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
