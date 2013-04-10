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

import scala.{ specialized => spec }
import org.saddle._
import org.saddle.locator.Locator
import org.saddle.array.Sorter
import org.joda.time.DateTime

/**
 * Typeclass definition for scalar tags. A ScalarTag contains important meta-data regarding
 * a scalar type, including how to instantiate a Buffer/Vec/Mat/Index of that type, as well
 * as an array. Often implicitly required when dealing with objects in Saddle
 */
trait ScalarTag[@spec(Boolean, Int, Long, Float, Double) T]
  extends SpecializedFactory[T] with ClassManifest[T] with CouldBeOrdered[T] with CouldBeNumber[T] {
  // representation of missing data
  def missing: T
  def isMissing(t: T): Boolean
  def notMissing(t: T): Boolean

  def isTuple: Boolean = false
  def isDouble: Boolean = false

  def strList = (v: T) => List(show(v))

  def show(v: T): String

  override def hashCode(): Int = runtimeClass.hashCode()

  override def equals(o: Any): Boolean = o match {
    case s: ScalarTag[_] => (this eq s) || (runtimeClass == s.runtimeClass)
    case _               => false
  }

  override def toString = "ScalarTag[%s]" format runtimeClass

  override def erasure = runtimeClass

  // forward 2.10 compatibility
  def runtimeClass: Class[_]
}

object ScalarTag extends LowPriorityScalarTagImplicits {
  implicit val stChar = ScalarTagChar
  implicit val stByte = ScalarTagByte
  implicit val stBool = ScalarTagBool
  implicit val stShort = ScalarTagShort
  implicit val stInt = ScalarTagInt
  implicit val stFloat = ScalarTagFloat
  implicit val stLong = ScalarTagLong
  implicit val stDouble = ScalarTagDouble
  implicit val stTime = ScalarTagTime
}

trait LowPriorityScalarTagImplicits extends LowerPriorityScalarTagImplicits {
  implicit def stPrd[T <: Product : CLM] = new ScalarTagProduct[T]
}

trait LowerPriorityScalarTagImplicits {
  implicit def stAny[T : CLM] = new ScalarTagAny[T]
}

trait CouldBeOrdered[@spec(Boolean, Int, Long, Float, Double) T] {
  // for comparable scalars
  def compare(a: T, b: T)(implicit ev: ORD[T]): Int
  def lt(a: T, b: T)(implicit ev: ORD[T]) = compare(a, b) < 0
  def gt(a: T, b: T)(implicit ev: ORD[T]) = compare(a, b) > 0
  def iseq(a: T, b: T)(implicit ev: ORD[T]) = compare(a, b) == 0
}

trait CouldBeNumber[@spec(Boolean, Int, Long, Float, Double) T] {
  // for numeric scalars
  def toDouble(t: T)(implicit ev: NUM[T]): Double
  def isDouble: Boolean

  def zero(implicit ev: NUM[T]): T
  def one(implicit ev: NUM[T]): T
  def inf(implicit ev: NUM[T]): T
  def negInf(implicit ev: NUM[T]): T
}

trait SpecializedFactory[@spec(Boolean, Int, Long, Float, Double) T] {
  def makeBuf(sz: Int = Buffer.INIT_CAPACITY): Buffer[T]
  def makeLoc(sz: Int = Buffer.INIT_CAPACITY): Locator[T]
  def makeVec(arr: Array[T]): Vec[T]
  def makeMat(r: Int, c: Int, arr: Array[T]): Mat[T]
  def makeIndex(vec: Vec[T])(implicit ord: ORD[T]): Index[T]
  def makeSorter(implicit ord: ORD[T]): Sorter[T]
}