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
import org.saddle.{ST, Vec, array, Mat}
import org.saddle.scalar.Scalar

/**
  * An IndexedSeq of Vecs which must all have the same length; a container for
  * 2D data for a Frame.
  */
class MatCols[@spec(Int, Long, Double) A: ST](cols: IndexedSeq[Vec[A]])
    extends IndexedSeq[Vec[A]] {
  require(
    cols.length < 2 || cols.forall(_.length == cols(0).length),
    "Vecs must all be the same length"
  )

  def scalarTag = implicitly[ST[A]]

  def numRows = cols.headOption.map(_.length) getOrElse 0

  def numCols = cols.length

  def length = numCols

  // the r'th element of the c'th vector
  // danger - could expose internal NA's
  private[saddle] def apply(r: Int, c: Int): A = cols(c).raw(r)

  // ith vector
  def apply(i: Int): Vec[A] = cols(i)

  def at(r: Int, c: Int): Scalar[A] = cols(c).at(r)

  // take vectors at particular locations
  def take(locs: Array[Int]): MatCols[A] = {
    lazy val nullVec = {
      val arr = array.empty[A](numRows)
      array.fill(arr, scalarTag.missing)
      Vec(arr)
    }
    val res = Array.ofDim[Vec[A]](locs.length)
    var i = 0
    while (i < locs.length) {
      val idx = locs(i)
      if (idx == -1)
        res(i) = nullVec
      else
        res(i) = cols(idx)
      i += 1
    }
    MatCols(res)
  }

  // take all vectors except those at points in loc
  def without(locs: Array[Int]): MatCols[A] =
    MatCols(array.remove(this.toArray, locs))

  // take all vecs that match provided type, along with their locations
  private[saddle] def takeType[B: ST]: (IndexedSeq[Vec[B]], Array[Int]) = {
    val bSt = implicitly[ST[B]]
    val filt = cols.zipWithIndex.filter {
      case (col, _) =>
        col.scalarTag.runtimeClass.isPrimitive && (bSt.isAny || bSt.isAnyVal) ||
          !bSt.isAnyVal && bSt.runtimeClass.isAssignableFrom(
            col.scalarTag.runtimeClass
          )
    }
    val (vecs, locs) = filt.unzip
    (vecs.asInstanceOf[IndexedSeq[Vec[B]]], locs.toArray)
  }
}

object MatCols {

  def empty[@spec(Int, Long, Double) A: ST]: MatCols[A] =
    apply(Array.empty[Vec[A]])

  def apply[@spec(Int, Long, Double) A: ST](cols: Vec[A]*): MatCols[A] =
    new MatCols[A](cols.toIndexedSeq)

  def apply[@spec(Int, Long, Double) A: ST](cols: Array[Vec[A]]): MatCols[A] =
    new MatCols[A](cols)

  def apply[@spec(Int, Long, Double) A: ST](mat: Mat[A]): MatCols[A] =
    new MatCols[A](mat.cols())

  // Logic to get string widths of columns in a sequence of vectors
  private[saddle] def colLens[@spec(Int, Long, Double) A: ST](
      cols: MatCols[A],
      numCols: Int,
      len: Int
  ): Map[Int, Int] = {
    val half = len / 2
    val maxf = (a: Int, b: String) => a.max(b.length)

    if (numCols <= len) {
      Range(0, numCols) zip cols.map { v =>
        val takeCol = v.head(half) concat v.tail(half)
        takeCol.map(k => v.scalarTag.show(k)).foldLeft(2)(maxf)
      }
    } else {
      val colnums = Range(0, half) ++ Range(numCols - half, numCols)
      colnums zip (cols.take(half) ++ cols.takeRight(half)).map { v =>
        val takeCol = v.head(half) concat v.tail(half)
        takeCol.map(k => v.scalarTag.show(k)).foldLeft(2)(maxf)
      }
    }
  }.toMap
}
