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

/**
 * Long ScalarTag
 */
private[saddle] object ScalarTagLong extends ScalarTag[Long] {
  def missing: Long = Long.MinValue
  def isMissing(v: Long): Boolean = v == Long.MinValue
  def notMissing(v: Long): Boolean = v != Long.MinValue

  def classTag = implicitly[CLM[Long]]

  def isTuple = false

  def compare(x: Long, y: Long)(implicit ev: ORD[Long]) = if (x == y) 0 else if (x > y) 1 else -1

  def toDouble(t: Long)(implicit ev: NUM[Long]) = if(isMissing(t)) ScalarTagDouble.missing else t.asInstanceOf[Double]
  def isDouble = false

  def zero(implicit ev: NUM[Long]) = 0L
  def one(implicit ev: NUM[Long]) = 1L
  def inf(implicit ev: NUM[Long]) = Long.MaxValue
  def negInf(implicit ev: NUM[Long]) = Long.MinValue

  def show(v: Long) = if (isMissing(v)) "%s" format "NA" else "%d" format v
}
