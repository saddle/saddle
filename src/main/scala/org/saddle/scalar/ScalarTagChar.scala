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
 * Char ScalarTag
 */
object ScalarTagChar extends ScalarTag[Char] {
  def missing: Char = Char.MinValue
  def isMissing(v: Char): Boolean = (v == Char.MinValue)
  def notMissing(v: Char): Boolean = (v != Char.MinValue)

  // note, consider N/A's equal
  def compare(x: Char, y: Char)(implicit ev: ORD[Char]) =
    if (x == y) 0 else if (x > y) 1 else if (x < y) -1 else 0

  def toDouble(t: Char)(implicit ev: NUM[Char]): Double = t

  def zero(implicit ev: NUM[Char]) = 0
  def one(implicit ev: NUM[Char]) = 1
  def inf(implicit ev: NUM[Char]) = Char.MaxValue
  def negInf(implicit ev: NUM[Char]) = Char.MinValue

  def show(v: Char) = if (isMissing(v)) "%s" format "NA" else "%s" format(v)

  override def runtimeClass = implicitly[CLM[Char]].erasure

  def makeBuf(sz: Int = Buffer.INIT_CAPACITY) = new BufferAny[Char](sz)
  def makeLoc(sz: Int = Buffer.INIT_CAPACITY) = new LocatorAny[Char](sz)
  def makeVec(arr: Array[Char]) = new VecAny[Char](arr)
  def makeMat(r: Int, c: Int, arr: Array[Char]) = new MatAny[Char](r, c, arr)
  def makeIndex(vec: Vec[Char])(implicit ord: ORD[Char]): Index[Char] = new IndexAny[Char](vec)
  def makeSorter(implicit ord: ORD[Char]): Sorter[Char] = Sorter.charSorter
}