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

import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import org.scalacheck.{Gen, Arbitrary}
import org.joda.time.{Days, Months, DateTimeConstants, DateTime, DateTimeZone}
import org.scalacheck.Prop._

class RRulesCheck extends Specification with ScalaCheck {
  import RRules._

  private def genDate: Gen[DateTime] = for {
    year <- Gen.choose(1990, 2020)
    offset <- Gen.choose(-366, 366)
  } yield new DateTime(year, 1, 1, 0, 0, 0, 0).plusDays(offset)

  private def toWeekday(i: Int) = i match {
    case 1 => MO // ISO standard; joda conforms
    case 2 => TU
    case 3 => WE
    case 4 => TH
    case 5 => FR
    case 6 => SA
    case 7 => SU
    case _ => throw new IllegalArgumentException("Bad weekday %d" format i)
  }

  private def genWeekday: Gen[Weekday] = for { i <- Gen.choose(1, 7) } yield toWeekday(i)

  private def isNotWeekend(dt: DateTime) = {
    dt.dayOfWeek().get() must_!= DateTimeConstants.SUNDAY
    dt.dayOfWeek().get() must_!= DateTimeConstants.SATURDAY
  }

  "RRule Tests" in {

    implicit val arbDate = Arbitrary(genDate)
    implicit val arbWkdy = Arbitrary(genWeekday)

    "bizDays must work as expected" in {
      forAll { (dt: DateTime) =>
        val result1 = conform(bizDays, dt, forward = true)

        result1 must be_>=(dt)
        Days.daysBetween(result1, dt).getDays must be_<=(2)
        isNotWeekend(result1)

        val result2 = conform(bizDays, dt, forward = false)

        result2 must be_<=(dt)
        Days.daysBetween(dt, result2).getDays must be_<=(2)
        isNotWeekend(result2)
      }
    }
    
    "bizDays 11/8/2002 test must pass (corner case with DST)" in {
      val zone = DateTimeZone.forID("America/Sao_Paulo")
      
      val dt = new DateTime(2002, 11, 8, 0, 0, 0, 0, zone)
      
      bizDays counting -1 from dt must_== dt
    }

    "bizEoms must work as expected" in {
      forAll { (dt: DateTime) =>
        val result1 = conform(bizEoms, dt, forward = true)

        result1 must be_>=(dt)
        Months.monthsBetween(dt, result1).getMonths must be_<=(1)
        result1.plusDays(3) must_!= result1.getMonthOfYear
        isNotWeekend(result1)

        val result2 = conform(bizEoms, dt, forward = false)

        result2 must be_<=(dt)
        Months.monthsBetween(result2, dt).getMonths must be_<=(1)
        result2.plusDays(3) must_!= result2.getMonthOfYear
        isNotWeekend(result2)
      }
    }

    "bizEoms 10/30/2007 test must pass" in {
      bizEoms counting -1 from datetime(2007,10,30) must_== datetime(2007,9,28)
    }

    "bizBoms must work as expected" in {
      forAll { (dt: DateTime) =>
        val result1 = conform(bizBoms, dt, forward = true)

        result1 must be_>=(dt)
        Months.monthsBetween(dt, result1).getMonths must be_<=(1)
        result1.minusDays(3) must_!= result1.getMonthOfYear
        isNotWeekend(result1)

        val result2 = conform(bizBoms, dt, forward = false)
        result2 must be_<=(dt)
        Months.monthsBetween(result2, dt).getMonths must be_<=(1)
        result2.minusDays(3) must_!= result2.getMonthOfYear
        isNotWeekend(result2)
      }
    }

    "bizEoqs must work as expected" in {
      forAll { (dt: DateTime) =>
        val result1 = conform(bizEoqs, dt, forward = true)

        result1 must be_>=(dt)
        Set(3,6,9,12).contains(result1.getMonthOfYear) must beTrue
        result1.plusDays(3) must_!= result1.getMonthOfYear
        isNotWeekend(result1)

        val result2 = conform(bizEoqs, dt, forward = false)

        result2 must be_<=(dt)
        Set(3,6,9,12).contains(result1.getMonthOfYear) must beTrue
        result2.plusDays(3) must_!= result2.getMonthOfYear
        isNotWeekend(result2)
      }
    }

    "bizEoqs 9/29/2002 test must pass" in {
      bizEoqs counting -1 from datetime(2002,9,29) must_== datetime(2002,6,28)
    }

    "bizBoqs must work as expected" in {
      forAll { (dt: DateTime) =>
        val result1 = conform(bizBoqs, dt, forward = true)

        result1 must be_>=(dt)
        Set(3,6,9,12).contains(result1.getMonthOfYear) must beTrue
        result1.minusDays(3) must_!= result1.getMonthOfYear
        isNotWeekend(result1)

        val result2 = conform(bizBoqs, dt, forward = false)

        result2 must be_<=(dt)
        Set(3,6,9,12).contains(result1.getMonthOfYear) must beTrue
        result2.minusDays(3) must_!= result2.getMonthOfYear
        isNotWeekend(result2)
      }
    }

    "bizBoqs 6/2/19 test must pass" in {
      bizBoqs counting -1 from datetime(2019,6,2) must_== datetime(2019,3,1)
    }

    "eoms must work as expected" in {
      forAll { (dt: DateTime) =>
        val result1 = conform(eoms, dt, forward = true)

        result1 must be_>=(dt)
        Months.monthsBetween(dt, result1).getMonths must be_<=(1)
        result1.plusDays(1).getMonthOfYear must_!= result1.getMonthOfYear

        val result2 = conform(eoms, dt, forward = false)

        result2 must be_<=(dt)
        Months.monthsBetween(result2, dt).getMonths must be_<=(1)
        result2.plusDays(1).getMonthOfYear must_!= result2.getMonthOfYear
      }
    }

    "boms must work as expected" in {
      forAll { (dt: DateTime) =>
        val result1 = conform(boms, dt, forward = true)

        result1 must be_>=(dt)
        Months.monthsBetween(dt, result1).getMonths must be_<=(1)
        result1.minusDays(1).getMonthOfYear must_!= result1.getMonthOfYear

        val result2 = conform(boms, dt, forward = false)

        result2 must be_<=(dt)
        Months.monthsBetween(result2, dt).getMonths must be_<=(1)
        result2.minusDays(1).getMonthOfYear must_!= result2.getMonthOfYear
      }
    }

    "weekly must work as expected" in {
      forAll { (dt: DateTime, wd: Weekday) =>
        val rule = weeklyOn(wd)

        val result1 = conform(rule, dt, forward = true)
        result1 must be_>=(dt)
        toWeekday(result1.dayOfWeek().get()) must_== wd

        val result2 = conform(rule, dt, forward = false)
        result2 must be_<=(dt)
        toWeekday(result2.dayOfWeek().get()) must_== wd
      }
    }
  }
}
