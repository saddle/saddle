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

import org.saddle._
import org.saddle.index.IndexAny
import org.saddle.locator.{LocatorAny, Locator}
import org.saddle.array.Sorter
import org.saddle.Buffer

class ScalarTagAny[T: CLM] extends ScalarTag[T] {
  def missing: T = null.asInstanceOf[T]
  def isMissing(v: T): Boolean = v == null
  def notMissing(v: T): Boolean = v != null

  def compare(x: T, y: T)(implicit ev: ORD[T]): Int =
    if (x == null && y == null) 0
    else if (x == null) -1
    else if (y == null) +1
    else ev.compare(x, y)

  def toDouble(t: T)(implicit ev: NUM[T]) = ev.toDouble(t)

  def zero(implicit ev: NUM[T]) = ev.zero
  def one(implicit ev: NUM[T]) = ev.one
  def inf(implicit ev: NUM[T]) = sys.error("Infinities not supported")
  def negInf(implicit ev: NUM[T]) = sys.error("Infinities not supported")

  def show(v: T) = "%s" format (if (v == null) "NA" else v.toString)

  override def runtimeClass = implicitly[CLM[T]].runtimeClass

  def makeBuf(sz: Int = org.saddle.Buffer.INIT_CAPACITY): Buffer[T] =
    new Buffer(new Array[T](sz), 0)
  def makeLoc(sz: Int = Locator.INIT_CAPACITY): Locator[T] =
    new LocatorAny[T](sz)(this)
  def makeVec(arr: Array[T]): Vec[T] = Vec(arr)(this)
  def makeMat(r: Int, c: Int, arr: Array[T]): Mat[T] = Mat(r, c, arr)(this)
  def makeIndex(vec: Vec[T])(implicit ord: ORD[T]): Index[T] =
    new IndexAny[T](vec)(this, ord)
  def makeSorter(implicit ord: ORD[T]): Sorter[T] = Sorter.anySorter[T]

  def concat(arrs: IndexedSeq[Vec[T]]): Vec[T] =
    Vec(array.flatten(arrs.map(_.toArray)))
}
