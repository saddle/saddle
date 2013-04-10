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

package org.saddle.time

import org.saddle._
import org.saddle.scalar._
import time._

import org.joda.time._
import scala.{specialized => spec}
import util.Concat.Promoter

/**
 * A compact native int representation of posix times at millisecond resolution which
 * conforms to and extends the interface of Vec[DateTime]
 *
 * @param times A Vec[Long], where each element is a millisecond timestamp
 * @param tzone Optional time zone containing localization info
 */
class VecTime(val times: Vec[Long], val tzone: DateTimeZone = ISO_CHRONO.getZone) extends Vec[DateTime] {

  val scalarTag = ScalarTagTime

  val chrono = ISO_CHRONO.withZone(tzone)

  private val lmf = scalar.ScalarTagLong

  private def l2t(l: Long) = if (lmf.isMissing(l)) scalarTag.missing else new DateTime(l, chrono)
  private def t2l(t: DateTime) = if (scalarTag.isMissing(t)) lmf.missing else t.getMillis
  private def vl2vt(l: Vec[Long]) = new VecTime(l, tzone)

  def length = times.length

  private[saddle] def apply(loc: Int) = l2t(times(loc))

  def take(locs: Array[Int]) = vl2vt(times.take(locs))

  def without(locs: Array[Int]) = vl2vt(times.without(locs))

  // specialized concatenation
  def concat(x: VecTime) = vl2vt(Vec(util.Concat.append(times.toArray, x.times.toArray)))

  // general concatenation
  def concat[B, C](v: Vec[B])(implicit wd: Promoter[DateTime, B, C], mc: ST[C]) =
    Vec(util.Concat.append[DateTime, B, C](toArray, v.toArray))

  def unary_-() = sys.error("Cannot negate TimeVec")

  def map[@spec(Boolean, Int, Long, Double) B: ST](f: (DateTime) => B) =
    times.map(v => f(l2t(v)))

  def foldLeft[@spec(Boolean, Int, Long, Double) B: ST](init: B)(f: (B, DateTime) => B) =
    times.foldLeft(init)((a,b) => f(a, l2t(b)))

  def scanLeft[@spec(Boolean, Int, Long, Double) B: ST](init: B)(f: (B, DateTime) => B) =
    times.scanLeft(init)((a,b) => f(a, l2t(b)))

  def filterFoldLeft[@spec(Boolean, Int, Long, Double) B: ST](pred: (DateTime) => Boolean)(init: B)(f: (B, DateTime) => B) =
    times.filterFoldLeft(l2t _ andThen pred)(init)((a, b) => f(a, l2t(b)))

  def filterScanLeft[@spec(Boolean, Int, Long, Double) B: ST](pred: (DateTime) => Boolean)(init: B)(f: (B, DateTime) => B) =
    times.filterScanLeft(l2t _ andThen pred)(init)((a, b) => f(a, l2t(b)))

  def foldLeftWhile[@spec(Boolean, Int, Long, Double) B: ST](init: B)(f: (B, DateTime) => B)(
    cond: (B, DateTime) => Boolean) = times.foldLeftWhile(init)((a, b) => f(a, l2t(b)))((a, b) => cond(a, l2t(b)))

  def zipMap[@spec(Boolean, Int, Long, Double) B: ST, @spec(Boolean, Int, Long, Double) C: ST](
    other: Vec[B])(f: (DateTime, B) => C) = times.zipMap(other)((a, b) => f(l2t(a), b))

  def dropNA = vl2vt(times.dropNA)

  def hasNA = times.hasNA

  def rolling[@spec(Boolean, Int, Long, Double) B: ST](winSz: Int, f: (Vec[DateTime]) => B) =
    times.rolling(winSz, vl2vt _ andThen f)

  def slice(from: Int, until: Int, stride: Int) =
    vl2vt(times.slice(from, until, stride))

  def shift(n: Int) = vl2vt(times.shift(n))

  override def sorted(implicit ev: ORD[DateTime], st: ST[DateTime]) = take(array.argsort(times.toArray))

  override def pad: VecTime = vl2vt(times.pad)

  override def fillNA(f: Int => DateTime): VecTime = vl2vt(times.fillNA(f andThen t2l))

  override def reversed: VecTime = vl2vt(times.reversed)

  protected def copy = vl2vt(Vec(times.contents))

  private[saddle] def toArray = times.toArray.map(l2t)
}

object VecTime {
  private val sm = ScalarTagTime
  private val sl = ScalarTagLong

  def apply(times : Array[DateTime]): VecTime = {
    val millis = array.empty[Long](times.length)
    var i = 0
    while (i < millis.length) {
      val t = times(i)
      millis(i) = if(sm.isMissing(t)) sl.missing else t.getMillis
      i += 1
    }
    new VecTime(millis)
  }
}