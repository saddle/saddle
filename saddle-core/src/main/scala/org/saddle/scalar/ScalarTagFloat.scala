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
import org.saddle.array.Sorter
import org.saddle.buffer.BufferFloat
import org.saddle.locator.LocatorFloat
import org.saddle.vec.VecFloat
import org.saddle.index.IndexFloat
import org.saddle.mat.{MatFloat, MatAny}

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

   def toDouble(t: Float)(implicit ev: NUM[Float]) = if(isMissing(t)) ScalarTagDouble.missing else t.asInstanceOf[Double]
   override def isDouble = false // should this be a isFloat ??

   def zero(implicit ev: NUM[Float]) = 0f
   def one(implicit ev: NUM[Float]) = 1f
   def inf(implicit ev: NUM[Float]) = Float.PositiveInfinity
   def negInf(implicit ev: NUM[Float]) = Float.NegativeInfinity

   def show(v: Float) = if (isMissing(v)) "%s" format "NA" else "%.4f" format(v)

   override def runtimeClass = classOf[Float]

   def makeBuf(sz: Int = Buffer.INIT_CAPACITY) = new BufferFloat(sz)
   def makeLoc(sz: Int = Buffer.INIT_CAPACITY) = new LocatorFloat(sz)
   def makeVec(arr: Array[Float]) = new VecFloat(arr)
   def makeMat(r: Int, c: Int, arr: Array[Float]) = new MatFloat(r, c, arr)//new MatAny[Float](r, c, arr)(this)
  //def makeMat(r: Int, c: Int, arr: Array[T]): Mat[T] = new MatAny[T](r, c, arr)(this)
   def makeIndex(vec: Vec[Float])(implicit ord: ORD[Float]): Index[Float] = new IndexFloat(vec)
   override def makeSorter(implicit ord: ORD[Float]): Sorter[Float] = Sorter.floatSorter

   def concat(arrs: IndexedSeq[Vec[Float]]): Vec[Float] = Vec(array.flatten(arrs.map(_.toArray)))

   override def toString = "ScalarTagFloat"
}