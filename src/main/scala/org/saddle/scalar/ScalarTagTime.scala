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
import sun.reflect.generics.reflectiveObjects.NotImplementedException
import org.saddle.buffer.BufferAny
import org.saddle.locator.LocatorAny
import org.saddle.mat.MatAny
import org.joda.time.format.DateTimeFormat
import org.saddle.time.{VecTime, IndexTime}

/**
 * DateTime ScalarTag
 */
object ScalarTagTime extends ScalarTag[DateTime] {
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

  // for numeric scalars
  def toDouble(t: DateTime)(implicit ev: NUM[DateTime]) = 0.0

  def zero(implicit ev: NUM[DateTime]) = throw new NotImplementedException
  def one(implicit ev: NUM[DateTime]) = throw new NotImplementedException
  def inf(implicit ev: NUM[DateTime]) = throw new NotImplementedException
  def negInf(implicit ev: NUM[DateTime]) = throw new NotImplementedException

  // representation of missing data
  def missing = null
  def isMissing(t: DateTime) = (t == null)
  def notMissing(t: DateTime) = (t != null)

  private val fmtZ = DateTimeFormat.forPattern("YYYY-MM-dd HH:mm:ss.SSSZZ")

  def show(v: DateTime) = Option(v) map { fmtZ.print(_) } getOrElse("NA")

  // forward 2.10 compatibility
  def runtimeClass = implicitly[CLM[DateTime]].erasure

  // for comparable scalars
  def compare(a: DateTime, b: DateTime)(implicit ev: ORD[DateTime]) = ev.compare(a, b)

  def makeBuf(sz: Int) = new BufferAny[DateTime]

  def makeLoc(sz: Int) = new LocatorAny[DateTime]

  def makeMat(r: Int, c: Int, arr: Array[DateTime]) = new MatAny[DateTime](r, c, arr)

  override def toString = "ScalarTagTime"
}