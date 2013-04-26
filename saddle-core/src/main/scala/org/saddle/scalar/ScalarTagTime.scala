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

package org.saddle.scalar

import org.joda.time._

import org.saddle._
import org.saddle.array.Sorter
import org.joda.time.format.DateTimeFormat
import org.saddle.vec.VecTime
import org.saddle.index.IndexTime

/**
 * DateTime ScalarTag
 */
object ScalarTagTime extends ScalarTagAny[DateTime] {
  def time2LongArray(arr: Array[DateTime]): Array[Long] = {
    val sz = arr.length
    val larr = Array.ofDim[Long](sz)
    var i = 0
    while (i < sz) {
      larr(i) = Option(arr(i)) match {
        case Some(x) => x.getMillis
        case None    => ScalarTagLong.missing
      }
      i += 1
    }
    larr
  }

  override def makeVec(arr: Array[DateTime]): Vec[DateTime] =
    new VecTime(Vec(time2LongArray(arr)))

  override def makeIndex(vec: Vec[DateTime])(implicit ord: ORD[DateTime]): Index[DateTime] =
    new IndexTime(Index(time2LongArray(vec.toArray)))

  override def makeSorter(implicit ord: ORD[DateTime]): Sorter[DateTime] =
    Sorter.timeSorter

  private val fmtZ = DateTimeFormat.forPattern("YYYY-MM-dd HH:mm:ss.SSSZZ")

  override def show(v: DateTime) = Option(v) map { fmtZ.print(_) } getOrElse("NA")

  // forward 2.10 compatibility
  override def runtimeClass = classOf[DateTime]

  override def toString = "ScalarTagTime"

  override def concat(vecs: IndexedSeq[Vec[DateTime]]): VecTime = VecTime.concat(vecs)
}