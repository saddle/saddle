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

/**
 * Typeclass definition for scalar tags. A ScalarTag contains important meta-data regarding
 * a scalar type, including how to instantiate a Buffer/Vec/Mat/Index of that type, as well
 * as an array. Often implicitly required when dealing with objects in Saddle
 */
trait ScalarTag[@spec(Boolean, Int, Long, Float, Double) T]
  extends ClassManifest[T] with SpecializedFactory[T] with CouldBeOrdered[T] with CouldBeNumber[T] with ScalarHelperOps[T] {
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

object ScalarTag extends ScalarTagImplicits {
  implicit val stChar = ScalarTagChar
  implicit val stByte = ScalarTagByte
  implicit val stBool = ScalarTagBool
  implicit val stShort = ScalarTagShort
  implicit val stInt = ScalarTagInt
  implicit val stFloat = ScalarTagFloat
  implicit val stLong = ScalarTagLong
  implicit val stDouble = ScalarTagDouble
  implicit val stTime = ScalarTagTime

  // experimental: going to change this to different implementation
  // implicit val stString = ScalarTagString
}

trait ScalarTagImplicits extends LowPriorityScalarTagImplicits {
  implicit def stPrd[T <: Product : CLM] = new ScalarTagProduct[T]
}

trait LowPriorityScalarTagImplicits {
  implicit def stAny[T : CLM] = new ScalarTagAny[T]
}

trait CouldBeOrdered[@spec(Boolean, Int, Long, Float, Double) T] {
  // for comparable scalars
  def compare(a: T, b: T)(implicit ev: ORD[T]): Int
  def lt(a: T, b: T)(implicit ev: ORD[T]) = compare(a, b) < 0
  def gt(a: T, b: T)(implicit ev: ORD[T]) = compare(a, b) > 0
  def iseq(a: T, b: T)(implicit ev: ORD[T]) = compare(a, b) == 0
}

trait ScalarHelperOps[@spec(Boolean, Int, Long, Float, Double) T] {
  /**
   * Offer a type-specific way to concat vecs
   */
  def concat(vecs: IndexedSeq[Vec[T]]): Vec[T]
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

  /**
   * An alternative Mat factory method using array of Vecs
   */
  final def makeMat(arr: Array[Vec[T]])(implicit st: ST[T]): Mat[T] = {
    val c = arr.length
    if (c == 0) st.makeMat(0, 0, st.newArray(0)) else {
      val r = arr(0).length
      if (r == 0) st.makeMat(0, 0, st.newArray(0)) else {
        require(arr.foldLeft(true)(_ && _.length == r), "All vec inputs must have the same length")
        altMatConstructor(r, c, arr)
      }
    }
  }

  /**
   * Can override this default construction methodology to avoid the toArray call if you
   * don't want to extract elements that way.
   */
  protected def altMatConstructor(r: Int, c: Int, arr: Array[Vec[T]])(implicit st: ST[T]): Mat[T] =
    makeMat(c, r, st.concat(arr).toArray).T
}