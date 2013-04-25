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

package org.saddle

import org.joda.time.{DateTimeField, DateTimeFieldType, Chronology, DateTime}
import org.joda.time.chrono.ISOChronology
import org.saddle.vec.VecTime
import org.saddle.index.IndexTime

/**
 * Functionality to assist in TimeSeries related operations
 */
package object time {
  val ISO_CHRONO = ISOChronology.getInstance
  val ISO_CHRONO_UTC = ISOChronology.getInstanceUTC

  val TZ_LOCAL = ISO_CHRONO.getZone
  val TZ_UTC = ISO_CHRONO_UTC.getZone

  /**
   * Convenience factory for constructing a DateTime instance
   */
  def datetime(y: Int = 0, m: Int = 0, d: Int = 0, h: Int = 0, t: Int = 0, s: Int = 0, ms: Int = 0,
               chrono: Chronology = ISO_CHRONO): DateTime = {

    val dt = new DateTime(chrono)

    val Y = if (y == 0) dt.getYear else y
    val M = if (m == 0) dt.getMonthOfYear else m
    val D = if (d == 0) dt.getDayOfMonth else d

    new DateTime(Y, M, D, h, t, s, ms, chrono)
  }

  /**
   * Mixin class providing time accessor methods for Vec and Index containing DateTimes
   */
  protected[saddle] class TimeAccessors[T](times: Vec[Long], chrono: Chronology, cast: Vec[Int] => T) {
    def millisOfSecond     = cast(extractor(1L, 1000L))
    def secondOfMinute     = cast(extractor(1000L, 60L))
    def minuteOfHour       = cast(extractor(60000L, 60L))

    private def _millisOfDay = getField(DateTimeFieldType.millisOfDay.getField(chrono), isTime = true)
    private def _secondOfDay = getField(DateTimeFieldType.secondOfDay.getField(chrono), isTime = true)

    def millisOfDay        = cast(_millisOfDay)
    def secondOfDay        = cast(_secondOfDay)
    def minuteOfDay        = cast(getField(DateTimeFieldType.minuteOfDay.getField(chrono), isTime = true))
    def clockhourOfDay     = cast(getField(DateTimeFieldType.clockhourOfDay.getField(chrono), isTime = true))
    def hourOfHalfday      = cast(getField(DateTimeFieldType.hourOfHalfday.getField(chrono), isTime = true))
    def clockhourOfHalfday = cast(getField(DateTimeFieldType.clockhourOfHalfday.getField(chrono), isTime = true))
    def halfdayOfDay       = cast(getField(DateTimeFieldType.halfdayOfDay.getField(chrono), isTime = true))
    def hourOfDay          = cast(getField(DateTimeFieldType.hourOfDay.getField(chrono), isTime = true))

    def dayOfWeek          = cast(getField(DateTimeFieldType.dayOfWeek.getField(chrono)))
    def dayOfMonth         = cast(getField(DateTimeFieldType.dayOfMonth.getField(chrono)))
    def dayOfYear          = cast(getField(DateTimeFieldType.dayOfYear.getField(chrono)))
    def weekOfWeekyear     = cast(getField(DateTimeFieldType.weekOfWeekyear.getField(chrono)))
    def weekyear           = cast(getField(DateTimeFieldType.weekyear.getField(chrono)))
    def weekyearOfCentury  = cast(getField(DateTimeFieldType.weekyearOfCentury.getField(chrono)))
    def monthOfYear        = cast(getField(DateTimeFieldType.monthOfYear.getField(chrono)))
    def year               = cast(getField(DateTimeFieldType.year.getField(chrono)))
    def yearOfEra          = cast(getField(DateTimeFieldType.yearOfEra.getField(chrono)))
    def yearOfCentury      = cast(getField(DateTimeFieldType.yearOfCentury.getField(chrono)))
    def centuryOfEra       = cast(getField(DateTimeFieldType.centuryOfEra.getField(chrono)))
    def era                = cast(getField(DateTimeFieldType.era.getField(chrono)))

    protected def getField(field: DateTimeField, isTime: Boolean = false): Vec[Int] =
      if (chrono != ISO_CHRONO_UTC || !isTime)
        times.map { (ms: Long) => field.get(ms) }
      else
        getFieldFast(field)

    protected def extractor(unit: Long, range: Long): Vec[Int] = times.map { (t: Long) =>
      if (t >= 0L) {
        ((t / unit) % range).toInt
      }
      else {
        (range - 1L + (((t + 1L) / unit) % range)).toInt
      }
    }

    /**
     * Using Joda time's PreciseDateTimeField logic directly allows much faster extraction of the
     * fractional units of each instant when there isn't a need to delegate chronology math.
     *
     * e.g., extract the minute of the current hour.
     */
    protected def getFieldFast(fld: DateTimeField): Vec[Int] = {
      val unit: Long = fld.getDurationField.getUnitMillis
      val range: Long = fld.getRangeDurationField.getUnitMillis / fld.getDurationField.getUnitMillis
      extractor(unit, range)
    }
  }

  /**
   * Enrichment methods for Vec[DateTime]
   */
  implicit def vecTimeAccessors(vec: Vec[DateTime]): TimeAccessors[Vec[Int]] = {
    val (times, chrono: Chronology) = vec match {
      case tv : VecTime => (tv.times, tv.chrono)
      case _            => { val tmp = new VecTime(vec.map(_.getMillis)); (tmp.times, tmp.chrono) }
    }
    new TimeAccessors(times, chrono, identity)
  }

  /**
   * Enrichment methods for Index[DateTime]
   */
  implicit def indexTimeAccessors(ix: Index[DateTime]): TimeAccessors[Index[Int]] = {
    val (times, chrono: Chronology) = ix match {
      case tv : IndexTime => (tv.times.toVec, tv.chrono)
      case _              => { val tmp = new IndexTime(ix.map(_.getMillis)); (tmp.times.toVec, tmp.chrono) }
    }

    new TimeAccessors(times, chrono, Index(_))
  }
}
