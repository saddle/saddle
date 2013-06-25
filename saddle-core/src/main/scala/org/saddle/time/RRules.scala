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

import org.joda.time.DateTime

/**
 * Helpful prepackaged recurrence rules
 */
object RRules {

  /**
   * Rule representing Monday through Friday
   *
   * Example: bizDays counting 3 from datetime(2013,1,1) ==> Jan 3, 2013
   */
  val bizDays = RRule(DAILY) byWeekDay(MO, TU, WE, TH, FR)

  /**
   * Rule representing business month ends
   *
   * Example: bizEoms counting 2 from datetime(2013,2,28) ==> Mar 29, 2013
   */
  val bizEoms = RRule(MONTHLY) byWeekDay(MO, TU, WE, TH, FR) bySetPos -1

  /**
   * Rule representing business month starts
   *
   * Example: bizBoms counting 5 from datetime(2013,2,1) ==> Jun 3, 2013
   */
  val bizBoms = RRule(MONTHLY) byWeekDay(MO, TU, WE, TH, FR) bySetPos 1

  /**
   * Rule representing business quarter ends
   *
   * Example: bizEoqs counting 5 from datetime(2013,2,1) ==> Mar 31, 2014
   */
  val bizEoqs = RRule(MONTHLY) byMonth(3, 6, 9, 12) byWeekDay(MO, TU, WE, TH, FR) bySetPos -1

  /**
   * Rule representing business quarter starts
   *
   * Example: bizBoqs counting 5 from datetime(2013,2,1) ==> Mar 3, 2014
   */
  val bizBoqs = RRule(MONTHLY) byMonth(3, 6, 9, 12) byWeekDay(MO, TU, WE, TH, FR) bySetPos 1

  /**
   * Rule representing month ends
   *
   * Example: eoms counting 2 from datetime(2013,2,28) ==> March 31, 2013
   */
  val eoms = RRule(MONTHLY) byMonthDay -1

  /**
   * Rule representing month beginnings
   *
   * Example: eoms counting 2 from datetime(2013,2,28) ==> April 1, 2013
   */
  val boms = RRule(MONTHLY) byMonthDay +1

  /**
   * Rule representing weekly on a particular weekday
   *
   * Example: weeklyOn(FR) counting 3 from datetime(2013,1,1) ==> Jan 18, 2013
   */
  def weeklyOn(wd: Weekday) = RRule(WEEKLY) byWeekDay wd

  /**
   * Conforms a datetime to a recurrence rule either forward or backward.
   */
  def conform(rule: RRule, dt: DateTime, forward: Boolean = true): DateTime = {
    forward match {
      case true  => rule counting -1 from { rule counting +1 from dt }
      case false => rule counting +1 from { rule counting -1 from dt }
    }
  }
}
