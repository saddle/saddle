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

package org.saddle.index

import scala.{ specialized => spec }

import org.saddle._
import org.saddle.scalar._
import org.saddle.locator._

import org.joda.time._

import org.saddle.vec.VecTime
import org.saddle.time._
import org.saddle.util.Concat.Promoter

/**
 * A compact native int representation of posix times at millisecond resolution which
 * conforms to and extends the interface of Index[DateTime]
 *
 * @param times An Index[Long], where each element is a millisecond timestamp
 * @param tzone Optional time zone containing localization info
 */
class IndexTime(val times: Index[Long],
                val tzone: DateTimeZone = ISO_CHRONO.getZone) extends Index[DateTime] {

  val scalarTag = ScalarTagTime

  val chrono = ISO_CHRONO.withZone(tzone)

  private val lmf = ScalarTagLong

  private def l2t(l: Long) = if (lmf.isMissing(l)) scalarTag.missing else new DateTime(l, chrono)
  private def t2l(t: DateTime) = if (scalarTag.isMissing(t)) lmf.missing else t.getMillis
  private def il2it(l: Index[Long]) = new IndexTime(l, tzone)

  private val _locator = new Locator[DateTime] {
    lazy val _keys = times.uniques.map(l2t)

    def contains(key: DateTime) = times.contains(t2l(key))
    def get(key: DateTime) = times.getFirst(t2l(key))

    def count(key: DateTime) = times.count(t2l(key))

    def keys() = _keys.toArray
    def counts() = times.counts

    def size = _keys.length

    // these should not be accessible
    def put(key: DateTime, value: Int) { throw new IllegalAccessError() }
    def inc(key: DateTime) = throw new IllegalAccessError()
  }

  protected def locator = _locator

  /**
   * Localize TimeIndex using particular time zone. Note, this does not
   * change the values of the times; merely how they are interpreted.
   *
   * @param tzone The time zone
   */
  def withZone(tzone: DateTimeZone) = new IndexTime(times, tzone)

  def length = times.length

  def toVec = new VecTime(times.toVec, tzone)

  def raw(loc: Int) = l2t(times.raw(loc))

  def take(locs: Array[Int]) = il2it(times.take(locs))

  def without(locs: Array[Int]) = il2it(times.without(locs))

  // specialized concatenation
  def concat(x: IndexTime) =
    il2it(Index(util.Concat.append(times.toArray, x.times.toArray)))

  // general concatenation
  def concat[B, C](x: Index[B])(implicit wd: Promoter[DateTime, B, C], mc: ST[C], oc: ORD[C]) =
    Index(util.Concat.append[DateTime, B, C](toArray, x.toArray))

  // find the first location whereby an insertion would maintain a sorted index
  def lsearch(t: DateTime) = times.lsearch(t2l(t))

  // find the last location whereby an insertion would maintain a sorted index
  def rsearch(t: DateTime) = times.rsearch(t2l(t))

  // slice at array locations, [from, until)
  def slice(from: Int, until: Int, stride: Int) =
    il2it(times.slice(from, until, stride))

  def intersect(other: Index[DateTime]) = {
    val tmp = times.intersect(getTimes(other))
    ReIndexer(tmp.lTake, tmp.rTake, il2it(tmp.index))
  }

  def union(other: Index[DateTime]) = {
    val tmp = times.union(getTimes(other))
    ReIndexer(tmp.lTake, tmp.rTake, il2it(tmp.index))
  }

  // default implementation, could be sped up in specialized instances
  def isMonotonic = times.isMonotonic

  // check whether, if not unique, the index values are at least grouped together
  def isContiguous = times.isContiguous

  /** Returns offsets into index that would result in sorted index */
  def argSort = times.argSort

  def reversed: IndexTime = il2it(times.reversed)

  // sql-style joins
  def join(other: Index[DateTime], how: JoinType) = {
    val tmp = times.join(getTimes(other), how)
    ReIndexer(tmp.lTake, tmp.rTake, il2it(tmp.index))
  }

  private def getTimes(other: Index[DateTime]): Index[Long] = other match {
    case ts : IndexTime => ts.times
    case _              => other.map(t2l)
  }

  override def getIndexer(other: Index[DateTime]): Option[Array[Int]] = {
    val otherTs = getTimes(other)
    val ixer = times.join(otherTs, index.RightJoin)
    require(ixer.index.length == other.length, "Could not reindex uniquely")
    ixer.lTake
  }

  // maps

  def map[@spec(Boolean, Int, Long, Double) B: ST: ORD](f: DateTime => B) =
    times.map(v => f(new DateTime(v, chrono)))

  private[saddle] def toArray = {
    val arr = array.empty[DateTime](length)
    var i = 0
    while (i < length) {
      arr(i) = l2t(times.raw(i))
      i += 1
    }
    arr
  }
}

object IndexTime {
  private val st = ScalarTagTime
  private val sl = ScalarTagLong

  /**
   * Create a new IndexTime from a sequence of times
   */
  def apply(times : DateTime*): IndexTime = apply(Vec(times : _*))

  /**
   * Create a new IndexTime from a Vec of times, with an attached timezone
   */
  def apply(times : Vec[DateTime], tzone: DateTimeZone = ISO_CHRONO.getZone): IndexTime = {
    val millis = array.empty[Long](times.length)
    var i = 0
    while (i < millis.length) {
      val t = times(i)
      millis(i) = if(st.isMissing(t)) sl.missing else t.getMillis
      i += 1
    }
    new IndexTime(Index(millis), tzone)
  }
}