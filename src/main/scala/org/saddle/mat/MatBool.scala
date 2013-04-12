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
import org.saddle.scalar._

/**
 * A Mat instance containing elements of type Boolean
 * */
class MatBool(r: Int, c: Int, values: Array[Boolean]) extends Mat[Boolean] {
  def repr = this

  def numRows = r

  def numCols = c

  def scalarTag = ScalarTagBool

  def toVec = scalarTag.makeVec(toArray)

  def map[@spec(Boolean, Int, Long, Double) B: ST](f: (Boolean) => B): Mat[B] = MatImpl.map(this)(f)

  // Cache the transpose: it's much faster to transpose and slice a continuous
  // bound than to take large strides, especially on large matrices where it
  // seems to eject cache lines on each stride (something like 10x slowdown)
  lazy val cachedT = {
    val arrT = values.clone()

    if (this.isSquare)
      MatMath.squareTranspose(numCols, arrT)
    else
      MatMath.blockTranspose(numRows, numCols, this.toArray, arrT)

    new MatBool(numCols, numRows, arrT)
  }

  def transpose = cachedT

  def copy: Mat[Boolean] = new MatBool(numRows, numCols, values.clone())

  def takeRows(locs: Array[Int]): Mat[Boolean] = MatImpl.takeRows(this, locs)

  def withoutRows(locs: Array[Int]): Mat[Boolean] = MatImpl.withoutRows(this, locs)

  def reshape(r: Int, c: Int): Mat[Boolean] = new MatBool(r, c, values)

  // access like vector in row-major order
  private[saddle] def apply(i: Int) = values(i)

  // implement access like matrix(i, j)
  private[saddle] def apply(r: Int, c: Int) = apply(r * numCols + c)

  // use with caution, may not return copy
  private[saddle] def toArray = values

  private[saddle] def toDoubleArray(implicit ev: NUM[Boolean]): Array[Double] = arrCopyToDblArr(values)

  private[saddle] def arrCopyToDblArr(r: Array[Boolean]): Array[Double] = {
    val arr = Array.ofDim[Double](r.length)
    var i = 0
    while(i < r.length) {
      arr(i) = if (r(i)) 1.0 else 0.0
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
