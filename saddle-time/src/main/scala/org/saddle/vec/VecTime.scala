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

import org.joda.time._
import scala.{specialized => spec}
import org.saddle.time._

/**
  * A compact native int representation of posix times at millisecond resolution which
  * conforms to and extends the interface of Vec[DateTime]
  *
  * @param times A Vec[Long], where each element is a millisecond timestamp
  * @param tzone Optional time zone containing localization info
  */
class VecTime(
    val times: Vec[Long],
    val tzone: DateTimeZone = ISO_CHRONO.getZone
) extends VecDefault[DateTime](null, ScalarTagTime) {

  @transient override val scalarTag = ScalarTagTime

  @transient lazy val chrono = ISO_CHRONO.withZone(tzone)

  @transient lazy private val lmf = scalar.ScalarTagLong

  private def l2t(l: Long) =
    if (lmf.isMissing(l)) scalarTag.missing else new DateTime(l, chrono)
  private def t2l(t: DateTime) =
    if (scalarTag.isMissing(t)) lmf.missing else t.getMillis
  private def vl2vt(l: Vec[Long]) = new VecTime(l, tzone)

  override def length = times.length

  override def raw(loc: Int) = l2t(times.raw(loc))

  override def take(locs: Array[Int]) = vl2vt(times.take(locs))

  override def without(locs: Array[Int]) = vl2vt(times.without(locs))

  // specialized concatenation
  def concat(x: VecTime) =
    vl2vt(Vec(util.Concat.append(times.toArray, x.times.toArray)))

  override def unary_-()(implicit num: NUM[DateTime]) =
    sys.error("Cannot negate VecTime")

  override def map[@spec(Boolean, Int, Long, Double) B: ST](
      f: (DateTime) => B
  ) =
    times.map(v => f(l2t(v)))

  override def flatMap[@spec(Boolean, Int, Long, Double) B: ST](
      f: DateTime => Vec[B]
  ): Vec[B] =
    VecImpl.flatMap(this)(f)

  override def foldLeft[@spec(Boolean, Int, Long, Double) B: ST](
      init: B
  )(f: (B, DateTime) => B) =
    times.foldLeft(init)((a, b) => f(a, l2t(b)))

  override def scanLeft[@spec(Boolean, Int, Long, Double) B: ST](
      init: B
  )(f: (B, DateTime) => B) =
    times.scanLeft(init)((a, b) => f(a, l2t(b)))

  override def filterFoldLeft[@spec(Boolean, Int, Long, Double) B: ST](
      pred: (DateTime) => Boolean
  )(init: B)(f: (B, DateTime) => B) =
    times.filterFoldLeft(l2t _ andThen pred)(init)((a, b) => f(a, l2t(b)))

  override def filterScanLeft[@spec(Boolean, Int, Long, Double) B: ST](
      pred: (DateTime) => Boolean
  )(init: B)(f: (B, DateTime) => B) =
    times.filterScanLeft(l2t _ andThen pred)(init)((a, b) => f(a, l2t(b)))

  override def foldLeftWhile[@spec(Boolean, Int, Long, Double) B: ST](
      init: B
  )(f: (B, DateTime) => B)(cond: (B, DateTime) => Boolean) =
    times.foldLeftWhile(init)((a, b) => f(a, l2t(b)))((a, b) => cond(a, l2t(b)))

  override def zipMap[
      @spec(Boolean, Int, Long, Double) B: ST,
      @spec(Boolean, Int, Long, Double) C: ST
  ](other: Vec[B])(f: (DateTime, B) => C) =
    times.zipMap(other)((a, b) => f(l2t(a), b))

  override def dropNA = vl2vt(times.dropNA)

  override def hasNA = times.hasNA

  override def rolling[@spec(Boolean, Int, Long, Double) B: ST](
      winSz: Int,
      f: (Vec[DateTime]) => B
  ) =
    times.rolling(winSz, vl2vt _ andThen f)

  override def slice(from: Int, until: Int, stride: Int) =
    vl2vt(times.slice(from, until, stride))

  override def shift(n: Int) = vl2vt(times.shift(n))

  override def sorted(implicit ev: ORD[DateTime], st: ST[DateTime]) =
    take(array.argsort(times.toArray))

  override def fillNA(f: Int => DateTime): VecTime =
    vl2vt(times.fillNA(f andThen t2l))

  override def reversed: VecTime = vl2vt(times.reversed)

  override def copy = vl2vt(Vec(times.contents))

  override def toArray = times.toArray.map(l2t)
}

object VecTime {
  @transient lazy private val sm = ScalarTagTime
  @transient lazy private val sl = ScalarTagLong

  /**
    * Create a new VecTime from an array of times
    */
  def apply(times: Array[DateTime]): VecTime = {
    val millis = array.empty[Long](times.length)
    var i = 0
    while (i < millis.length) {
      val t = times(i)
      millis(i) = if (sm.isMissing(t)) sl.missing else t.getMillis
      i += 1
    }
    new VecTime(Vec(millis))
  }

  /**
    * Create a new VecTime from a sequence of times
    */
  def apply(timeSeq: DateTime*): VecTime = {
    val times = timeSeq.toArray
    val millis = array.empty[Long](times.length)
    var i = 0
    while (i < millis.length) {
      val t = times(i)
      millis(i) = if (sm.isMissing(t)) sl.missing else t.getMillis
      i += 1
    }
    new VecTime(Vec(millis))
  }

  /**
    * Concatenate several Vec[DateTime] instances into one
    */
  def concat(arr: IndexedSeq[Vec[DateTime]]): VecTime = {
    val vecs = arr.map {
      _ match {
        case vt: VecTime => vt
        case v           => VecTime(v.toArray)
      }
    }

    // calculate offset for each subsequent vec of bytes
    val sz = vecs.foldLeft(0) { case (o, v) => o + v.length }

    val databuf = Array.ofDim[Long](sz)

    var c = 0 // byte counter
    vecs.zipWithIndex.foreach {
      case (v, _) =>
        val vlen = v.length
        var i = 0
        while (i < vlen) { databuf(c) = v.times.raw(i); i += 1; c += 1 }
    }

    new VecTime(Vec(databuf))
  }
}
