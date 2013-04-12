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
import org.saddle.util.Concat._
import org.saddle.buffer.{BufferInt, BufferAny}

/**
 * A compact byte buffer representation of UTF8 strings which conforms to and extends
 * the interface of Vec[String]
 *
 * @param data An array of bytes representing UTF-8 encoded strings all smashed together
 * @param offsets Offsets into byte buffer, where UTF8 strings begin; offset < 0 to represent NA
 * @param lengths Lengths of strings embedded in byte buffer, where each array cell corresponds
 *                with offset's
 */
class VecString(val data: Array[Byte], val offsets: Array[Int], val lengths: Array[Int])
  extends Vec[String] { self =>

  require(offsets.length == lengths.length,
          "Inconsistent offset and length array lengths (%d != %d)".format(offsets.length, lengths.length))

  val scalarTag = ScalarTagString

  def length = offsets.length

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

  def take(locs: Array[Int]) =
    new VecString(data, array.take(offsets, locs, -1), array.take(lengths, locs, 0))

  def without(locs: Array[Int]) =
    new VecString(data, Vec(offsets).without(locs).toArray, Vec(lengths).without(locs).toArray)

  def concat[B, C](v: Vec[B])(implicit wd: Promoter[String, B, C], mc: ST[C]) = {
    Vec(append[String, B, C](toArray, v.toArray).toIndexedSeq : _*)
  }

  def concat(v: Vec[String]) = v match {
    case vs: VecString => new VecString(
      append(data, vs.data),
      append(offsets, vs.offsets.map(_ + data.length)),
      append(lengths, vs.lengths))
    case _             => Vec(append(toArray, v.toArray).toIndexedSeq : _*)
  }

  def unary_-() = throw new UnsupportedOperationException("Cannot negate Vec[String]")

  def map[@spec(Boolean, Int, Long, Double) B: ST](f: (String) => B) =
    VecImpl.map(this)(f)

  def foldLeft[@spec(Boolean, Int, Long, Double) B: ST](init: B)(f: (B, String) => B) =
    VecImpl.foldLeft(this)(init)(f)

  def scanLeft[@spec(Boolean, Int, Long, Double) B: ST](init: B)(f: (B, String) => B) =
    VecImpl.scanLeft(this)(init)(f)

  def filterFoldLeft[@spec(Boolean, Int, Long, Double) B: ST](pred: (String) => Boolean)(init: B)(f: (B, String) => B) =
    VecImpl.filterFoldLeft(this)(pred)(init)(f)

  def filterScanLeft[@spec(Boolean, Int, Long, Double) B: ST](pred: (String) => Boolean)(init: B)(f: (B, String) => B) =
    VecImpl.filterScanLeft(this)(pred)(init)(f)

  def foldLeftWhile[@spec(Boolean, Int, Long, Double) B: ST](init: B)(f: (B, String) => B)(cond: (B, String) => Boolean) =
    VecImpl.foldLeftWhile(this)(init)(f)(cond)

  def zipMap[@spec(Boolean, Int, Long, Double) B: ST, @spec(Boolean, Int, Long, Double) C: ST](other: Vec[B])(f: (String, B) => C) =
    VecImpl.zipMap(this, other)(f)

  def dropNA = filter(_ => true)

  def hasNA = VecImpl.findOneNA(this)

  def rolling[@spec(Boolean, Int, Long, Double) B: ST](winSz: Int, f: (Vec[String]) => B) =
    VecImpl.rolling(this)(winSz, f)

  def slice(from: Int, until: Int, stride: Int) = {
    val b = math.max(from, 0)
    val e = math.min(until, self.length)

    if (e <= b)
      Vec.empty
    else if (from <= 0 && until >= length && stride == 1)
      this
    else {
      val newOff = Vec(offsets).slice(from, until, stride).toArray
      val newLen = Vec(lengths).slice(from, until, stride).toArray
      new VecString(data, newOff, newLen)
    }
  }

  def shift(n: Int) = new VecString(data, Vec(offsets).shift(n).toArray, Vec(lengths).shift(n).toArray)

  // deep copy unnecessary, b/c toArray, which uses this, will copy
  protected def copy = this

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
  /**
   * Create VecString from sequence of strings
   */
  def apply(strings: Seq[String]): VecString = {
    val nStrs = strings.length
    val encodings = new BufferAny[Array[Byte]](nStrs)
    val lengths = new BufferInt(nStrs)
    val offsets = new BufferInt(nStrs)

    val bufSz = strings.foldLeft(0) { (off, str) =>
      val bytes = Option(str).map { _.getBytes(UTF8) } getOrElse Array.empty[Byte]
      val len = bytes.length
      encodings.add(bytes)
      lengths.add(len)
      offsets.add(off)
      off + len
    }

    val data = Array.ofDim[Byte](bufSz)

    var i = 0
    while (i < nStrs) {
      System.arraycopy(encodings(i), 0, data, offsets(i), lengths(i))
      i += 1
    }

    new VecString(data, offsets.toArray, lengths.toArray)
  }

  /**
   * Create VecString from array of strings
   */
  def apply(strings: Array[String]): VecString = apply(strings.toSeq)

  /**
   * Concatenate several Vec[String] instances into one
   */
  def concat(arr: IndexedSeq[Vec[String]]): VecString = {
    val vecs = arr.map { _ match {
      case vs: VecString => vs
      case v             => VecString(v.toArray)
    } }

    // calculate offset for each subsequent vec of bytes
    val voffset = vecs.scanLeft(0) { case (o, vs) => o + vs.data.length }

    val databuf = Array.ofDim[Byte](voffset.last)
    val offsets = new BufferInt(1024)
    val lengths = new BufferInt(1024)

    var bc = 0 // byte counter
    vecs.zipWithIndex.foreach { case (v, vidx) =>
      val vlen = v.data.length
      var i = 0
      while (i < vlen) { databuf(bc) = v.data.apply(i); i += 1; bc += 1 }
      i = 0
      while (i < v.offsets.length) { offsets.add(v.offsets(i) + voffset(vidx)); i += 1 }
      i = 0
      while (i < v.lengths.length) { lengths.add(v.lengths(i)); i += 1 }
    }

    new VecString(databuf, offsets.toArray, lengths.toArray)
  }
}