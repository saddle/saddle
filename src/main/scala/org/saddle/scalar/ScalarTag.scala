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

/**
 * Typeclass definition for scalar tags. Contains important meta-data regarding a scalar type;
 * often implicitly required when dealing with objects in Saddle.
 */
trait ScalarTag[@spec(Boolean, Int, Long, Float, Double) T] extends CLM[T] {
  // representation of missing data
  def missing: T
  def isMissing(t: T): Boolean
  def notMissing(t: T): Boolean

  def isTuple: Boolean

  // for comparable scalars
  def compare(a: T, b: T)(implicit ev: ORD[T]): Int
  def lt(a: T, b: T)(implicit ev: ORD[T]) = compare(a, b) < 0
  def gt(a: T, b: T)(implicit ev: ORD[T]) = compare(a, b) > 0
  def iseq(a: T, b: T)(implicit ev: ORD[T]) = compare(a, b) == 0

  // for numeric scalars
  def toDouble(t: T)(implicit ev: NUM[T]): Double
  def isDouble: Boolean

  def zero(implicit ev: NUM[T]): T
  def one(implicit ev: NUM[T]): T
  def inf(implicit ev: NUM[T]): T
  def negInf(implicit ev: NUM[T]): T

  def strList = (v: T) => List(show(v))

  def show(v: T): String

  override def hashCode(): Int = super.hashCode()

  override def equals(o: Any): Boolean = o match {
    case s: ScalarTag[_] => (this eq s) || super.equals(s)
    case _               => false
  }

  override def toString = "ScalarTag[%s]" format erasure
}

object ScalarTag extends LowPriorityScalarTagImplicits {
  implicit val stChr = ScalarTagChar
  implicit val stByt = ScalarTagByte
  implicit val stBoo = ScalarTagBool
  implicit val stSho = ScalarTagShort
  implicit val stInt = ScalarTagInt
  implicit val stFlo = ScalarTagFloat
  implicit val stLon = ScalarTagLong
  implicit val stDub = ScalarTagDouble
}

trait LowPriorityScalarTagImplicits {
  implicit def stPrd[T <: Product : CLM] = ScalarTagProduct(implicitly[CLM[T]])
  implicit def stAny[T : CLM] = ScalarTagAny[T](implicitly[CLM[T]])
}

