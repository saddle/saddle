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

import org.joda.time.{Seconds, Days, DateTimeZone, DateTime}
import scala.collection.JavaConversions._
import com.google.ical.iter.{RecurrenceIterator, RecurrenceIteratorFactory}
import com.google.ical.compat.jodatime.DateTimeIteratorFactory
import org.saddle.Index

/**
 * Wrapper of a RFC 2445 RRULE or EXRULE as implemented in the google
 * rfc2445 java library.
 *
 * For _lots_ of examples of recurrence rule constructions, see the
 * following: http://recurrance.sourceforge.net/
 *
 * To create, start by invoking RRule(frequency), e.g.
 *
 *   RRule(DAILY)
 *
 * Use setters to construct the rule you want. For instance:
 *
 *   val rule = RRule(DAILY) byWeekDay(TU, TH) withCount(3)
 *
 * Finally, attach a start date as follows, to get a DateTime iterator:
 *
 *   val iter = rule from datetime(2007,1,1)
 *
 * By default, the times created will be in LOCAL time; however, you may
 * attach a different time zone (eg, UTC).
 *
 * Also, you may join rules or add exceptions via the join and except
 * functions. Eg,
 *
 *   val rules = RRule(DAILY) byWeekDay(TU, TH) join { RRule(DAILY) byWeekDay(MO) }
 *   val dates = rules from datetime(2006,12,31) take(5) toList
 *
 * Please note:
 *
 * Some of the javadoc descriptions of RFC2445 fields are courtesy of python
 * dateutil 2.1:
 *  -- http://labix.org/python-dateutil
 *  -- https://pypi.python.org/pypi/python-dateutil
 */
case class RRule private (freq: Frequency = DAILY,
                          interval: Int = 1,
                          wkst: Option[Weekday] = None,
                          count: Option[Int] = None,
                          until: Option[DateTime] = None,
                          bysetpos: List[Int] = List.empty,
                          bymonth: List[Int] = List.empty,
                          bymonthday: List[Int] = List.empty,
                          byyearday: List[Int] = List.empty,
                          byweekno: List[Int] = List.empty,
                          byday: List[WeekdayNum] = List.empty,
                          byhour: List[Int] = List.empty,
                          byminute: List[Int] = List.empty,
                          bysecond: List[Int] = List.empty,
                          inzone: DateTimeZone = TZ_LOCAL,
                          joins: List[(RRule, Option[DateTime])] = List.empty,
                          excepts: List[(RRule, Option[DateTime])] = List.empty) {


  private def dt2dtv(dt: DateTime): com.google.ical.values.DateTimeValueImpl = {
    new com.google.ical.values.DateTimeValueImpl(dt.getYear, dt.getMonthOfYear, dt.getDayOfMonth,
      dt.getHourOfDay, dt.getMinuteOfHour, dt.getSecondOfMinute)
  }

  protected[time] def toICal: com.google.ical.values.RRule = {
    val rrule = new com.google.ical.values.RRule()

    rrule.setFreq(freq.toICal)
    rrule.setInterval(interval)

    wkst.foreach { w => rrule.setWkSt(w.toICal) }
    count.foreach { rrule.setCount(_) }
    until.foreach { dt => rrule.setUntil(dt2dtv(dt)) }
    bysetpos.headOption.foreach { _ => rrule.setBySetPos(bysetpos.toArray) }
    bymonth.headOption.foreach { _ => rrule.setByMonth(bymonth.toArray) }
    bymonthday.headOption.foreach { _ => rrule.setByMonthDay(bymonthday.toArray) }
    byyearday.headOption.foreach { _ => rrule.setByYearDay(byyearday.toArray) }
    byweekno.headOption.foreach { _ => rrule.setByWeekNo(byweekno.toArray) }
    byday.headOption.foreach { _ => rrule.setByDay(seqAsJavaList(byday.map(v => v.toICal))) }
    byhour.headOption.foreach { _ => rrule.setByHour(byhour.toArray) }
    byminute.headOption.foreach { _ => rrule.setByMinute(byminute.toArray) }
    bysecond.headOption.foreach { _ => rrule.setBySecond(bysecond.toArray) }

    rrule
  }

  // setters, makes for a nice DSL

  /**
   * The week start day. Must be one of the MO, TU, ... constants specifying the first day of the week.
   * This will affect recurrences based on weekly periods.
   */
  def withWkSt(w: Weekday) = copy(wkst = Some(w))

  /**
   * The interval between each freq iteration. For example, when using YEARLY, an interval of 2 means once
   * every two years, but with HOURLY, it means once every two hours. The default interval is 1.
   */
  def withInterval(i: Int) = copy(interval = i)

  /**
   * How many occurrences will be generated.
   */
  def withCount(c: Int) = copy(count = Some(c))

  /**
   * A datetime instance that will specify the limit of the recurrence.
   */
  def withUntil(d: DateTime) = copy(until = Some(d))

  /**
   * The bySetPos specifies a value or list of values which correspond to the nth occurrence(s)
   * within the set of events specified by the rule. Valid values are +/-1 to +/-366. It MUST be
   * used in conjunction with another BYxxx rule, and is used to restrict the occurrences valid
   * within the rule.
   */
  def bySetPos(i: Int*) = copy(bysetpos = i.toList)

  /**
   * If given, it must be either an integer, or a sequence of integers, meaning the months valid in the recurrence.
   */
  def byMonth(m: Int*) = copy(bymonth = m.toList)

  /**
   * If given, it must be either an integer, or a sequence of integers, meaning the month days valid in the recurrence.
   */
  def byMonthDay(d: Int*) = copy(bymonthday = d.toList)

  /**
   * If given, it must be either an integer, or a sequence of integers, meaning the year days valid in the recurrence.
   */
  def byYearDay(d: Int*) = copy(byyearday = d.toList)

  /**
   * If given, it must be either an integer, or a sequence of integers, meaning the week numbers valid in the recurrence.
   * Week numbers have the meaning described in ISO8601, that is, the first week of the year is that containing at least
   * four days of the new year.
   */
  def byWeekNo(w: Int*) = copy(byweekno = w.toList)

  /**
   * If given, it must be either a Weekday (eg MO), a or a sequence of these constants. When given, these variables
   * will define the weekdays valid in the recurrence. It's also possible to use an argument n for the weekday instances,
   * which will signify the nth occurrence of this weekday in the period. For example, with MONTHLY, or with YEARLY and
   * BYMONTH, using FR(+1) in byweekday will specify the first friday of the month where the recurrence happens. Notice
   * that in the RFC documentation, this is specified as BYDAY, but was renamed to avoid the ambiguity of that keyword.
   */
  def byWeekDay[T](d: T*)(implicit conv: T => WeekdayNum) = copy(byday = d.map(conv).toList)

  /**
   * If given, it must be either an integer, or a sequence of integers, meaning the hours valid in the recurrence.
   */
  def byHour(h: Int*) = copy(byhour = h.toList)

  /**
   * If given, it must be either an integer, or a sequence of integers, meaning the minutes valid in the recurrence.
   */
  def byMinute(m: Int*) = copy(byminute = m.toList)

  /**
   * If given, it must be either an integer, or a sequence of integers, meaning the minutes valid in the recurrence.
   */
  def bySecond(s: Int*) = copy(bysecond = s.toList)

  /**
   * If given, it determines which time zone the recurrence date/times will be generated in.
   */
  def inZone(tz: DateTimeZone) = copy(inzone = tz)

  /**
   * Syntactic sugar to get the nth occurrence; allows user to write, e.g.
   *
   *   val x = weeklyOn(FR) counting 3 from datetime(2013, 1, 1)
   *
   * to get the third Friday in January 2013. With a negative number, e.g.
   *
   *   val x = weeklyOn(FR) counting -3 from datetime(2013, 1, 1)
   *
   * you would get the third occurrence counting back from Jan 1, 2013.
   *
   * Note that in both cases, if the 'from' date conforms to the recurrence
   * rule, it will be counted.
   */
  def counting(i: Int) = {
    val outer = this
    new {
      def from(dt: DateTime): DateTime = {
        if (i == 0)
          throw new IllegalArgumentException("argument to occurrence must not equal 0")
        else if (i > 0)
          // counting occurrences forward
          outer.from(dt).toStream.drop(i - 1).head
        else {
          // counting occurrences backward
          val iabs = i.abs

          // heuristic: take 4 observations, find the largest daycount between subsequent
          // occurrences, with a day of padding, and with a minimum of 1 day
          val dseq = { outer from dt take 4 }.toSeq
          val ival = { dseq.tail zip dseq }.foldLeft(1) { case (days, (d1, d2)) =>
            days max { Days.daysBetween(d2, d1).getDays + 2 }
          }

          // use this daycount to estimate lower bound from which to start generating dates
          val lbound = dt.minusDays(ival * iabs)
          val ubound = dt.plusDays(ival)

          // create index and count backward from min conforming time >= dt
          val idx = Index.make(outer, lbound, ubound)
          idx.raw(idx.rsearch(dt) - iabs)
        }
      }
    }
  }

  /**
   * Join with another RRule, and optionally specify a start date. If a start date is not specified,
   * it will utilize the start date provided when the function 'from' is applied.
   */
  def join(rrule: RRule, from: Option[DateTime] = None): RRule = copy(joins = rrule -> from :: this.joins)

  /**
   * Exclude another RRule, and optionally specify a start date. If a start date is not specified,
   * it will utilize the start date provided when the function 'from' is applied.
   */
  def except(rrule: RRule, from: Option[DateTime] = None): RRule = copy(excepts = rrule -> from :: this.excepts)

  /**
   * Generate an iterator of DateTime instances based on the current RRule instance starting on or after the
   * provided DateTime instance.
   */
  def from(dt: DateTime): Iterator[DateTime] = {
    val riter = RecurrenceIteratorFactory.createRecurrenceIterator(toICal, dt2dtv(dt), inzone.toTimeZone)

    val iterWithJoins = joins.foldLeft(riter) { case (i1, (rrule, t)) =>
      val tmpfrom = t.map { dt2dtv } getOrElse dt2dtv(dt)
      val tmpiter = RecurrenceIteratorFactory.createRecurrenceIterator(rrule.toICal, tmpfrom, inzone.toTimeZone)
      RecurrenceIteratorFactory.join(i1, tmpiter)
    }

    val iterWithJoinsWithExcepts = excepts.foldLeft(iterWithJoins) { case (i1, (rrule, t)) =>
      val tmpfrom = t.map { dt2dtv } getOrElse dt2dtv(dt)
      val tmpiter = RecurrenceIteratorFactory.createRecurrenceIterator(rrule.toICal, tmpfrom, inzone.toTimeZone)
      RecurrenceIteratorFactory.except(i1, tmpiter)
    }

    DateTimeIteratorFactory.createDateTimeIterator(iterWithJoinsWithExcepts) map { dt => dt.withZone(inzone) }
  }

  override def toString = toICal.toIcal
}

object RRule {
  def apply(f: Frequency): RRule = new RRule(freq = f)
}
