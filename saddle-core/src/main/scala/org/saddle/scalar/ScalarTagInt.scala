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
import org.saddle.Buffer
import org.saddle.index.IndexInt
import org.saddle.locator.{LocatorInt, Locator}
import org.saddle.array.Sorter

/**
  * Int ScalarTag
  */
object ScalarTagInt extends ScalarTag[Int] {
  def missing: Int = Int.MinValue
  def isMissing(v: Int): Boolean = v == Int.MinValue
  def notMissing(v: Int): Boolean = v != Int.MinValue

  def compare(x: Int, y: Int)(implicit ev: ORD[Int]) =
    if (x == y) 0 else if (x > y) 1 else -1

  def toDouble(t: Int)(implicit ev: NUM[Int]) =
    if (isMissing(t)) ScalarTagDouble.missing else t.asInstanceOf[Double]

  def zero(implicit ev: NUM[Int]) = 0
  def one(implicit ev: NUM[Int]) = 1
  def inf(implicit ev: NUM[Int]) = Int.MaxValue
  def negInf(implicit ev: NUM[Int]) = Int.MinValue

  def show(v: Int) = if (isMissing(v)) "%s" format "NA" else "%d" format v

  override def runtimeClass = classOf[Int]

  def makeBuf(sz: Int = org.saddle.Buffer.INIT_CAPACITY) =
    new Buffer(new Array[Int](sz), 0)
  def makeLoc(sz: Int = Locator.INIT_CAPACITY) = new LocatorInt(sz)
  def makeVec(arr: Array[Int]) = Vec(arr)(this)
  def makeMat(r: Int, c: Int, arr: Array[Int]) = Mat(r, c, arr)(this)
  def makeIndex(vec: Vec[Int])(implicit ord: ORD[Int]): Index[Int] =
    new IndexInt(vec)
  def makeSorter(implicit ord: ORD[Int]): Sorter[Int] = Sorter.intSorter

  def concat(arrs: IndexedSeq[Vec[Int]]): Vec[Int] =
    Vec(array.flatten(arrs.map(_.toArray)))

  override def toString = "ScalarTagInt"
}
