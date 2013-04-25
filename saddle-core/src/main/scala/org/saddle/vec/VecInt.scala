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
import org.saddle.scalar._
import org.saddle.util
import org.saddle.util.Concat.Promoter

class VecInt(values: Array[Int]) extends Vec[Int] { self =>
  def length = values.length

  def scalarTag = ScalarTagInt

  def apply(i: Int): Int = values(i)

  def copy: Vec[Int] = Vec(toArray.clone())

  def take(locs: Array[Int]): Vec[Int] = array.take(toArray, locs, scalarTag.missing)

  def without(locs: Array[Int]): Vec[Int] = array.remove(toArray, locs)

  def dropNA: Vec[Int] = filter(_ => true)

  def hasNA: Boolean = VecImpl.findOneNA(this)

  def unary_-(): Vec[Int] = map(-_)

  def concat[B, C](v: Vec[B])(implicit wd: Promoter[Int, B, C], mc: ST[C]): Vec[C] =
    Vec(util.Concat.append[Int, B, C](toArray, v.toArray))

  def foldLeft[@spec(Boolean, Int, Long, Double) B: ST](init: B)(f: (B, Int) => B): B =
    VecImpl.foldLeft(this)(init)(f)

  def foldLeftWhile[@spec(Boolean, Int, Long, Double) B: ST](init: B)(f: (B, Int) => B)(cond: (B, Int) => Boolean): B =
    VecImpl.foldLeftWhile(this)(init)(f)(cond)

  def filterFoldLeft[@spec(Boolean, Int, Long, Double) B: ST](pred: (Int) => Boolean)(init: B)(f: (B, Int) => B): B =
    VecImpl.filterFoldLeft(this)(pred)(init)(f)

  def rolling[@spec(Boolean, Int, Long, Double) B: ST](winSz: Int, f: Vec[Int] => B): Vec[B] =
    VecImpl.rolling(this)(winSz, f)

  def map[@spec(Boolean, Int, Long, Double) B: ST](f: Int => B): Vec[B] = VecImpl.map(this)(f)

  def scanLeft[@spec(Boolean, Int, Long, Double) B: ST](init: B)(f: (B, Int) => B): Vec[B] = VecImpl.scanLeft(this)(init)(f)

  def filterScanLeft[@spec(Boolean, Int, Long, Double) B: ST](pred: (Int) => Boolean)(init: B)(f: (B, Int) => B): Vec[B] =
    VecImpl.filterScanLeft(this)(pred)(init)(f)

  def zipMap[@spec(Int, Long, Double) B: ST, @spec(Boolean, Int, Long, Double) C: ST](other: Vec[B])(f: (Int, B) => C): Vec[C] =
    VecImpl.zipMap(this, other)(f)

  def slice(from: Int, until: Int, stride: Int = 1) = {
    val b = math.max(from, 0)
    val e = math.min(until, self.length)

    if (e <= b) Vec.empty else new VecInt(values) {
      private val ub = math.min(self.length, e)

      override def length = math.ceil((ub - b) / stride.toDouble).toInt

      override def apply(i: Int): Int = {
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

    new VecInt(values) {
      override def length = self.length

      override def apply(i: Int): Int = {
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

  private[saddle] def toArray: Array[Int] = {
    // need to check if we're a view on an array
    if ( !needsCopy )
      values
    else {
      val buf = new Array[Int](length)
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
    case rv: VecInt => (this eq rv) || (this.length == rv.length) && {
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
