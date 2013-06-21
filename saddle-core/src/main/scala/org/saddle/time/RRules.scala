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
 * Helpful prepackaged recurrence rules
 */
object RRules {

  /**
   * Rule representing Monday through Friday
   *
   * Example: bizDays count 3 from datetime(2013,1,1) ==> Jan 3, 2013
   */
  val bizDays = RRule(DAILY) byWeekDay(MO, TU, WE, TH, FR)

  /**
   * Rule representing weekly on a particular weekday
   *
   * Example: weeklyOn(FR) count 3 from datetime(2013,1,1) ==> Jan 18, 2013
   */
  def weeklyOn(wd: Weekday) = RRule(WEEKLY) byWeekDay wd

  /**
   * Rule representing business month ends
   *
   * Example: bizEoms count 2 from datetime(2013,2,28) ==> Mar 29, 2013
   */
  val bizEoms = RRule(MONTHLY) byWeekDay(MO, TU, WE, TH, FR) bySetPos -1

  /**
   * Rule representing month ends
   *
   * Example: eoms count 2 from datetime(2013,2,28) ==> March 31, 2013
   */
  val eoms = RRule(MONTHLY) byWeekDay(MO, TU, WE, TH, FR) bySetPos -1
}
