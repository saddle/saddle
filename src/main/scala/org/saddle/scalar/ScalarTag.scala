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
 * Concrete implementations provide support to working with scalars
 */
trait ScalarTag[@spec(Boolean, Int, Long, Float, Double) T] {
  def classTag: CLM[T]

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

  override def hashCode(): Int = classTag.hashCode()

  override def equals(o: Any): Boolean = o match {
    case s: ScalarTag[_] => (this eq s) || (this.classTag == s.classTag)
    case _ => false
  }

  override def toString = "ScalarTag[%s]" format classTag.erasure
}
