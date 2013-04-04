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

import org.saddle._
import org.saddle.scalar._

/**
 * A sequence of Vecs; a container for 2D data. Isomorphic to IndexedSeq; adds some additional
 * functionality.
 */
private[saddle] class VecSeq[A: ST](vecs: IndexedSeq[Vec[A]]) extends IndexedSeq[Vec[A]] {
  require(vecs.length < 2 || vecs.foldLeft(true)(_ && _.length == vecs(0).length),
          "Vecs must all be the same length")

  def scalarTag = implicitly[ST[A]]

  def numRows = vecs.headOption.map(_.length) getOrElse 0

  def numCols = vecs.length

  def length = numCols

  // the r'th element of the c'th vector
  // danger - could expose internal NA's
  private[saddle] def apply(r: Int, c: Int): A = vecs(c)(r)

  // ith vector
  def apply(i: Int): Vec[A] = vecs(i)

  def at(r: Int, c: Int): Scalar[A] = vecs(c)(r)

  // take vectors at particular locations
  def take(locs: Array[Int]): VecSeq[A] = {
    lazy val nullVec = {
      val arr = array.empty[A](numRows)
      array.fill(arr, scalarTag.missing)
      Vec(arr)
    }
    val res = Array.ofDim[Vec[A]](locs.length)
    var i = 0
    while(i < locs.length) {
      val idx = locs(i)
      if (idx == -1)
        res(i) = nullVec
      else
        res(i) = vecs(idx)
      i += 1
    }
    VecSeq(res)
  }

  // take all vectors except those at points in loc
  def without(locs: Array[Int]): VecSeq[A] = array.remove(this.toArray, locs).toIndexedSeq

  // take all vecs that match provided type, along with their locations
  private[saddle] def takeType[B: ST]: (IndexedSeq[Vec[B]], Array[Int]) = {
    val (v, i) = vecs.zipWithIndex.filter(_._1.scalarTag == implicitly[ST[B]]).unzip
    (v.asInstanceOf[IndexedSeq[Vec[B]]], i.toArray)
  }
}

object VecSeq {
  def empty[A: ST]: VecSeq[A] = apply(Array.empty[Vec[A]])

  def apply[A: ST](vecs: Vec[A]*): VecSeq[A] = new VecSeq[A](vecs.toIndexedSeq)

  def apply[A: ST](vecs: Array[Vec[A]]): VecSeq[A] = apply(vecs : _*)

  def apply[A: ST](mat: Mat[A]): VecSeq[A] = apply(mat.cols() : _*)

  // isomorphism
  implicit def IndexSeq2VecSeq[A: ST](vecs: IndexedSeq[Vec[A]]): VecSeq[A] = apply(vecs : _*)
  implicit def VecSeq2IndexSeq[A: ST](vecs: VecSeq[A]): IndexedSeq[Vec[A]] = vecs

  // Logic to get string widths of columns in a sequence of vectors
  private[saddle] def colLens[A: ST](vecs: VecSeq[A], numCols: Int, len: Int): Map[Int, Int] = {
    val half = len / 2
    val maxf = (a: Int, b: String) => a.max(b.length)

    if (numCols <= len) {
      Range(0, numCols) zip vecs.map { v =>
        val takeCol = v.head(half) concat v.tail(half)
        takeCol.map(k => v.scalarTag.show(k)).foldLeft(2)(maxf)
      }
    }
    else {
      val colnums = Range(0, half) ++ Range(numCols - half, numCols)
      colnums zip (vecs.take(half) ++ vecs.takeRight(half)).map { v =>
        val takeCol = v.head(half) concat v.tail(half)
        takeCol.map(k => v.scalarTag.show(k)).foldLeft(2)(maxf)
      }
    }
  }.toMap
}