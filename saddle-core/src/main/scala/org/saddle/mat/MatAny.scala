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

package org.saddle.mat

import scala.{specialized => spec}
import org.saddle._

/**
 * A Mat instance containing elements of type Any
 */
class MatAny[T: ST](r: Int, c: Int, values: Array[T]) extends Mat[T] {
  def repr = this

  def numRows = r

  def numCols = c

  lazy val scalarTag = implicitly[ST[T]]

  def toVec = scalarTag.makeVec(toArray)

  def map[@spec(Boolean, Int, Long, Double) B: ST](f: (T) => B): Mat[B] = MatImpl.map(this)(f)

  // Cache the transpose: it's much faster to transpose and slice a continuous
  // bound than to take large strides, especially on large matrices where it
  // seems to eject cache lines on each stride (something like 10x slowdown)
  lazy val cachedT = {
    val arrT = values.clone()

    if (this.isSquare)
      MatMath.squareTranspose(numCols, arrT)
    else
      MatMath.blockTranspose(numRows, numCols, this.toArray, arrT)

    new MatAny[T](numCols, numRows, arrT)
  }

  def transpose = cachedT

  def takeRows(locs: Array[Int]): Mat[T] = MatImpl.takeRows(this, locs)

  def withoutRows(locs: Array[Int]): Mat[T] = MatImpl.withoutRows(this, locs)

  def reshape(r: Int, c: Int): Mat[T] = new MatAny(r, c, values)

  // access like vector in row-major order
  private[saddle] def apply(i: Int) = values(i)

  // implement access like matrix(i, j)
  private[saddle] def apply(r: Int, c: Int) = apply(r * numCols + c)

  // use with caution, may not return copy
  private[saddle] def toArray = values

  private[saddle] def toDoubleArray(implicit ev: NUM[T]): Array[Double] = arrCopyToDblArr(values)

  private[saddle] def arrCopyToDblArr(r: Array[T])(implicit n: NUM[T]): Array[Double] = {
    val arr = Array.ofDim[Double](r.length)
    var i = 0
    while(i < r.length) {
      arr(i) = scalarTag.toDouble(r(i))
      i += 1
    }
    arr
  }

  /** Row-by-row equality check of all values. */
  override def equals(o: Any): Boolean = o match {
    case rv: Mat[_] => (this eq rv) || this.numRows == rv.numRows && this.numCols == rv.numCols && {
      var i = 0
      var eq = true
      while(eq && i < length) {
        eq &&= (apply(i) == rv(i) || this.scalarTag.isMissing(apply(i)) && rv.scalarTag.isMissing(rv(i)))
        i += 1
      }
      eq
    }
    case _ => super.equals(o)
  }
}

