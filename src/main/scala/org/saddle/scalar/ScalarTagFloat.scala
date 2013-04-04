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
import org.saddle.vec.VecAny
import org.saddle.mat.MatAny
import org.saddle.buffer.BufferAny
import org.saddle.index.IndexAny
import org.saddle.locator.LocatorAny
import org.saddle.array.Sorter

/**
 * Float ScalarTag
 */
object ScalarTagFloat extends ScalarTag[Float] {
  def missing: Float = Float.NaN
  def isMissing(v: Float): Boolean = (v != v)
  def notMissing(v: Float): Boolean = (v == v)

  // note, consider N/A's equal
  def compare(x: Float, y: Float)(implicit ev: ORD[Float]) =
    if (x == y) 0 else if (x > y) 1 else if (x < y) -1 else 0

  def toDouble(t: Float)(implicit ev: NUM[Float]): Double = t

  def zero(implicit ev: NUM[Float]) = 0
  def one(implicit ev: NUM[Float]) = 1
  def inf(implicit ev: NUM[Float]) = Float.PositiveInfinity
  def negInf(implicit ev: NUM[Float]) = Float.NegativeInfinity

  def show(v: Float) = if (isMissing(v)) "%s" format "NA" else "%s" format(v)

  override def runtimeClass = implicitly[CLM[Float]].erasure

  def makeBuf(sz: Int = Buffer.INIT_CAPACITY) = new BufferAny[Float](sz)
  def makeLoc(sz: Int = Buffer.INIT_CAPACITY) = new LocatorAny[Float](sz)
  def makeVec(arr: Array[Float]) = new VecAny[Float](arr)
  def makeMat(r: Int, c: Int, arr: Array[Float]) = new MatAny[Float](r, c, arr)
  def makeIndex(vec: Vec[Float])(implicit ord: ORD[Float]): Index[Float] = new IndexAny[Float](vec)
  def makeSorter(implicit ord: ORD[Float]): Sorter[Float] = Sorter.floatSorter
}