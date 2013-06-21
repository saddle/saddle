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

/**
 * Enumeration of weekdays for utilizing with an RRule
 */
sealed trait Weekday { this: Weekday =>
  protected[time] def toICal: com.google.ical.values.Weekday = this match {
    case SU => com.google.ical.values.Weekday.SU
    case MO => com.google.ical.values.Weekday.MO
    case TU => com.google.ical.values.Weekday.TU
    case WE => com.google.ical.values.Weekday.WE
    case TH => com.google.ical.values.Weekday.TH
    case FR => com.google.ical.values.Weekday.FR
    case SA => com.google.ical.values.Weekday.SA
  }

  /**
   * Create a WeekdayNum instance: e.g., MO(1) or TU(-2)
   */
  def apply(i: Int): WeekdayNum = WeekdayNum(i, this)
}

object Weekday {
  /**
   * Allow for implicit Weekday => WeekdayNum conversion: e.g., MO => MO(0)
   */

  implicit def w2wn(w: Weekday) = WeekdayNum(0, w)
}

case object SU extends Weekday
case object MO extends Weekday
case object TU extends Weekday
case object WE extends Weekday
case object TH extends Weekday
case object FR extends Weekday
case object SA extends Weekday

/**
 * Weekday + Offset, for utilizing with an RRule
 */
case class WeekdayNum(n: Int, d: Weekday) {
  assert(n <= 53 && n >= -53, "n must be in [-53, 53]")

  protected[time] def toICal: com.google.ical.values.WeekdayNum =
    new com.google.ical.values.WeekdayNum(n, d.toICal)

  override def toString = "%s(%d)" format (d, n)
}