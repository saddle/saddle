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

package org.saddle.time

import org.joda.time._

/**
 * Enriched DateTime, helps to simplify Date handling API
 */
case class RichDT(dt: DateTime) {
  def +[T <: ReadablePeriod](rp: T): RichDT = dt.plus(rp)
  def -[T <: ReadablePeriod](rp: T): RichDT = dt.minus(rp)

  def <[T](ot: T)(implicit fn: T => ReadableInstant) = dt.isBefore(fn(ot))
  def >[T](ot: T)(implicit fn: T => ReadableInstant) = dt.isAfter(fn(ot))
  def <=[T](ot: T)(implicit fn: T => ReadableInstant) = !dt.isAfter(fn(ot))
  def >=[T](ot: T)(implicit fn: T => ReadableInstant) = !dt.isBefore(fn(ot))

  def stripTime: RichDT = RichDT(dt.toDateMidnight.toDateTime(dt.getZone))

  override def toString: String = dt.toString
}