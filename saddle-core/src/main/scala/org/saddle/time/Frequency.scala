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

import org.joda.time.{Period, Duration}

/**
 * Enumeration of frequency base for utilizing with an RRule
 */
sealed trait Frequency { this: Frequency =>
  protected[time] def toICal: com.google.ical.values.Frequency = this match {
    case SECONDLY =>  com.google.ical.values.Frequency.SECONDLY
    case MINUTELY =>  com.google.ical.values.Frequency.MINUTELY
    case HOURLY   =>  com.google.ical.values.Frequency.HOURLY
    case DAILY    =>  com.google.ical.values.Frequency.DAILY
    case WEEKLY   =>  com.google.ical.values.Frequency.WEEKLY
    case MONTHLY  =>  com.google.ical.values.Frequency.MONTHLY
    case YEARLY   =>  com.google.ical.values.Frequency.YEARLY
  }

  protected[time] def toDur: Period = this match {
    case SECONDLY => Period.seconds(1)
    case MINUTELY => Period.minutes(1)
    case HOURLY   => Period.hours(1)
    case DAILY    => Period.days(1)
    case WEEKLY   => Period.weeks(1)
    case MONTHLY  => Period.months(1)
    case YEARLY   => Period.years(1)
  }
}

case object SECONDLY extends Frequency
case object MINUTELY extends Frequency
case object HOURLY extends Frequency
case object DAILY extends Frequency
case object WEEKLY extends Frequency
case object MONTHLY extends Frequency
case object YEARLY extends Frequency
