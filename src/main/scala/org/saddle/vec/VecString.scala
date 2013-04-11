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

import scala.{specialized => spec}
import util.Concat.Promoter
import org.saddle.buffer.{BufferInt, BufferAny}

/**
 * A compact byte buffer representation of UTF8 strings which conforms to and extends
 * the interface of Vec[String]
 *
 * @param data An array of bytes
 * @param offsets Offsets into byte buffer, where UTF8 strings begin
 */
class VecString(val data: Array[Byte],
                val offsets: Array[Int],
                val lengths: Array[Int]) extends Vec[String] {

  val scalarTag = new ScalarTagAny[String]

  def length = offsets.length

  private[saddle] def apply(loc: Int) = {
    val len = lengths(loc)
    val off = offsets(loc)
    val tmpArr = Array.ofDim[Byte](len)
    System.arraycopy(data, off, tmpArr, 0, len)
    new String(tmpArr, "UTF-8")
  }

  def take(locs: Array[Int]) = null

  def without(locs: Array[Int]) = null

  def concat[B, C](v: Vec[B])(implicit wd: Promoter[String, B, C], mc: ST[C]) = null

  def unary_-() = null

  def map[@spec(Boolean, Int, Long, Double) B: ST](f: (String) => B) = null

  def foldLeft[@spec(Boolean, Int, Long, Double) B: ST](init: B)(f: (B, String) => B) = throw new RuntimeException

  def scanLeft[@spec(Boolean, Int, Long, Double) B: ST](init: B)(f: (B, String) => B) = null

  def filterFoldLeft[@spec(Boolean, Int, Long, Double) B: ST](pred: (String) => Boolean)(init: B)(f: (B, String) => B) = throw new RuntimeException

  def filterScanLeft[@spec(Boolean, Int, Long, Double) B: ST](pred: (String) => Boolean)(init: B)(f: (B, String) => B) = null

  def foldLeftWhile[@spec(Boolean, Int, Long, Double) B: ST](init: B)(f: (B, String) => B)(cond: (B, String) => Boolean) = throw new RuntimeException

  def zipMap[@spec(Boolean, Int, Long, Double) B: ST, @spec(Boolean, Int, Long, Double) C: ST](other: Vec[B])(f: (String, B) => C) = null

  def dropNA = null

  def hasNA = false

  def rolling[@spec(Boolean, Int, Long, Double) B: ST](winSz: Int, f: (Vec[String]) => B) = null

  def slice(from: Int, until: Int, stride: Int) = null

  def shift(n: Int) = null

  protected def copy = null

  private[saddle] def toArray = {
    val arr = Array.ofDim[String](length)
    var i = 0
    while(i < length) {
      arr(i) = apply(i)
      i += 1
    }
    arr
  }
}

object VecString {
  def apply(strings: Seq[String]): VecString = {
    val nStrs = strings.length
    val encod = new BufferAny[Array[Byte]](nStrs)
    val lens = new BufferInt(nStrs)
    val offs = new BufferInt(nStrs)

    val bufSz = strings.foldLeft(0) { (off, str) =>
      val bytes = str.getBytes("UTF-8")
      val len = bytes.length
      encod.add(bytes)
      lens.add(len)
      offs.add(off)
      off + len
    }

    val data = Array.ofDim[Byte](bufSz)

    var i = 0
    while(i < nStrs) {
      System.arraycopy(encod(i), 0, data, offs(i), lens(i))
      i += 1
    }

    new VecString(data, offs.toArray, lens.toArray)
  }
}