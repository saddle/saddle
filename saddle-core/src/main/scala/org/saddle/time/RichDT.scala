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
import com.google.ical.iter.{RecurrenceIteratorFactory, RecurrenceIterator}
import com.google.ical.values.DateTimeValue

// import com.google.ical.compat.jodatime.DateTimeIteratorFactory

/**
 * Enriched Joda DateTime to simplify Date handling API
 */
case class RichDT(dt: DateTime) {
  def +[T <: ReadablePeriod](rp: T): RichDT = dt.plus(rp)
  def -[T <: ReadablePeriod](rp: T): RichDT = dt.minus(rp)

  def stripTime: RichDT = RichDT(dt.toDateMidnight.toDateTime(dt.getZone))
 override def toString: String = dt.toString
}

object RichDT {
  // establish isomorphism to joda datetime

  implicit def dt2rd(dt: DateTime): RichDT = new RichDT(dt)
  implicit def rd2dt(rd: RichDT): DateTime = rd.dt

  // convenience methods for constructing ReadablePeriod instances

  def years(i: Int)    = Years.years(i)
  def quarters(i: Int) = Months.months(i * 3)
  def months(i: Int)   = Months.months(i)
  def weeks(i: Int)    = Weeks.weeks(i)
  def days(i: Int)     = Days.days(i)
}

sealed trait Frequency
case object SECONDLY extends Frequency
case object MINUTELY extends Frequency
case object HOURLY extends Frequency
case object DAILY extends Frequency
case object WEEKLY extends Frequency
case object MONTHLY extends Frequency
case object YEARLY extends Frequency

sealed trait Weekday
case object SU extends Weekday
case object MO extends Weekday
case object TU extends Weekday
case object WE extends Weekday
case object TH extends Weekday
case object FR extends Weekday
case object SA extends Weekday

case class RRule(
  freq: Frequency = DAILY,
  interval: Int = 1,
  wkst: Option[Weekday] = None,
  count: Option[Int] = None,
  until: Option[DateTime] = None,
  bysetpos: List[Int] = List.empty,
  bymonth: List[Int] = List.empty,
  bymonthday: List[Int] = List.empty,
  byyearday: List[Int] = List.empty,
  byweekno: List[Int] = List.empty,
  byweekday: List[Weekday] = List.empty,
  byhour: List[Int] = List.empty,
  byminute: List[Int] = List.empty,
  bysecond: List[Int] = List.empty) {

  import com.google.ical.compat.jodatime.DateTimeIteratorFactory
  import scala.collection.JavaConversions.asScalaIterator

  def from(dt: DateTime): Iterator[DateTime] = {
    val rrule = new com.google.ical.values.RRule()

    val f = freq match {
      case SECONDLY =>  com.google.ical.values.Frequency.SECONDLY
      case MINUTELY =>  com.google.ical.values.Frequency.MINUTELY
      case HOURLY   =>  com.google.ical.values.Frequency.HOURLY
      case DAILY    =>  com.google.ical.values.Frequency.DAILY
      case WEEKLY   =>  com.google.ical.values.Frequency.WEEKLY
      case MONTHLY  =>  com.google.ical.values.Frequency.MONTHLY
      case YEARLY   =>  com.google.ical.values.Frequency.YEARLY
    }

    rrule.setFreq(f)
    rrule.setInterval(interval)

    wkst.map { w =>
      rrule.setWkSt(
        w match {
          case SU => com.google.ical.values.Weekday.SU
          case MO => com.google.ical.values.Weekday.MO
          case TU => com.google.ical.values.Weekday.TU
          case WE => com.google.ical.values.Weekday.WE
          case TH => com.google.ical.values.Weekday.TH
          case FR => com.google.ical.values.Weekday.FR
          case SA => com.google.ical.values.Weekday.SA
        })
    }


    val dv = new com.google.ical.values.DateValueImpl(dt.getYear, dt.getMonthOfYear, dt.getDayOfMonth)
    val tzid = dt.getZone.toTimeZone
    val riter = RecurrenceIteratorFactory.createRecurrenceIterator(rrule, dv, tzid)
    val diter = DateTimeIteratorFactory.createDateTimeIterator(riter)
    asScalaIterator(diter)
  }
}
