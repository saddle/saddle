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
 * Byte ScalarTag
 */
object ScalarTagByte extends ScalarTag[Byte] {
  def missing: Byte = Byte.MinValue
  def isMissing(v: Byte): Boolean = (v == Byte.MinValue)
  def notMissing(v: Byte): Boolean = (v != Byte.MinValue)

  // note, consider N/A's equal
  def compare(x: Byte, y: Byte)(implicit ev: ORD[Byte]) =
    if (x == y) 0 else if (x > y) 1 else if (x < y) -1 else 0

  def toDouble(t: Byte)(implicit ev: NUM[Byte]): Double = t

  def zero(implicit ev: NUM[Byte]) = 0
  def one(implicit ev: NUM[Byte]) = 1
  def inf(implicit ev: NUM[Byte]) = Byte.MaxValue
  def negInf(implicit ev: NUM[Byte]) = Byte.MinValue

  def show(v: Byte) = if(isMissing(v)) "%s" format "NA" else "%s" format(v)

  override def runtimeClass = implicitly[CLM[Byte]].erasure

  def makeBuf(sz: Int = Buffer.INIT_CAPACITY) = new BufferAny[Byte](sz)
  def makeLoc(sz: Int = Buffer.INIT_CAPACITY) = new LocatorAny[Byte](sz)
  def makeVec(arr: Array[Byte]) = new VecAny[Byte](arr)
  def makeMat(r: Int, c: Int, arr: Array[Byte]) = new MatAny[Byte](r, c, arr)
  def makeIndex(vec: Vec[Byte])(implicit ord: ORD[Byte]): Index[Byte] = new IndexAny[Byte](vec)
  def makeSorter(implicit ord: ORD[Byte]): Sorter[Byte] = Sorter.byteSorter
}