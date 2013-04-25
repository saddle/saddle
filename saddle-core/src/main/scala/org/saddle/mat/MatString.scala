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

import org.saddle._
import org.saddle.Mat
import org.saddle.scalar.ScalarTagString
import scala.{ specialized => spec }
import vec.VecString

/**
 * A compact byte buffer representation of UTF8 strings which conforms to and extends
 * the interface of Mat[String]
 *
 * @param data An array of bytes representing UTF-8 encoded strings all smashed together in row-major order
 * @param offsets Offsets into byte buffer, where UTF8 strings begin; offset < 0 to represent NA
 * @param lengths Lengths of strings embedded in byte buffer, where each array cell corresponds
 */
class MatString(val data: Array[Byte], val offsets: Mat[Int], val lengths: Mat[Int]) extends Mat[String] {

  require(offsets.numRows == lengths.numRows && offsets.numCols == lengths.numCols,
          "Offsets matrix and lengths matrix do not match dimensions")

  def scalarTag = ScalarTagString

  def numRows = offsets.numRows

  def numCols = offsets.numCols

  def map[@spec(Boolean, Int, Long, Double) B: ST](f: (Int, Int, String) => B): Mat[B] = MatImpl.map(this)(f)

  def mapValues[@spec(Boolean, Int, Long, Double) B: ST](f: (String) => B) = MatImpl.mapValues(this)(f)

  def reshape(r: Int, c: Int) =
    new MatString(data, offsets.reshape(r, c), lengths.reshape(r, c))

  def transpose =
    new MatString(data, offsets.transpose, lengths.transpose)

  def takeRows(locs: Array[Int]) =
    new MatString(data, offsets.takeRows(locs), lengths.takeRows(locs))

  def withoutRows(locs: Array[Int]) =
    new MatString(data, offsets.withoutRows(locs), lengths.withoutRows(locs))

  private var rowCache: Map[Int, Vec[String]] = Map.empty

  override def row(r: Int)(implicit ev: ST[String]): Vec[String] = {
    assert(r >= 0 && r < numRows, "Array index %d out of bounds" format r)
    synchronized {
      rowCache.getOrElse(r, {
        val tmp = super.row(r)
        rowCache += (r -> tmp)
        tmp
      })
    }
  }

  private var colCache: Map[Int, Vec[String]] = Map.empty

  override def col(c: Int)(implicit ev: ST[String]): Vec[String] = {
    assert(c >= 0 && c < numCols, "Array index %d out of bounds" format c)
    synchronized {
      colCache.getOrElse(c, {
        val tmp = super.col(c)
        colCache += (c -> tmp)
        tmp
      })
    }
  }

  def toVec: Vec[String] = new VecString(data, offsets.toVec, lengths.toVec)

  // access like vector in row-major order
  private[saddle] def apply(loc: Int) = {
    val len = lengths(loc)
    val off = offsets(loc)
    if (off < 0)
      scalarTag.missing
    else {
      val bytes = Array.ofDim[Byte](len)
      System.arraycopy(data, off, bytes, 0, len)
      new String(bytes, UTF8)
    }
  }

  // implement access like matrix(i, j)
  private[saddle] def apply(r: Int, c: Int) = {
    val len = lengths(r, c)
    val off = offsets(r, c)
    if (off < 0)
      scalarTag.missing
    else {
      val bytes = Array.ofDim[Byte](len)
      System.arraycopy(data, off, bytes, 0, len)
      new String(bytes, UTF8)
    }
  }

  // use with caution, may not return copy
  private[saddle] def toArray = toVec.toArray

  // use with caution, may not return copy
  private[saddle] def toDoubleArray(implicit ev: NUM[String]) =
    throw new UnsupportedOperationException
}

object MatString {
  def apply(r: Int, c: Int, strs: Seq[String]): Mat[String] = {
    val dataVec = VecString(strs)
    val offsets = new MatInt(r, c, dataVec.offsets)
    val lengths = new MatInt(r, c, dataVec.lengths)
    new MatString(dataVec.data, offsets, lengths)
  }
}