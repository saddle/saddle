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
 */

package org.saddle
import org.joda.time.{DateTime, DateTimeZone}

object DateTimeZoneUtils {
  val zoneNY = DateTimeZone.forID("America/New_York")
  val zoneSP = DateTimeZone.forID("America/Sao_Paulo")

  val defaultZone = DateTimeZone.getDefault
  // Ensures that the time zone of the testing datetime is different than the default one
  val alternateZone = if (defaultZone == zoneNY) zoneSP else zoneNY

  private val now = DateTime.now().getMillis()
  private val defaultOffset = defaultZone.getOffsetFromLocal(now)
  private val hourOffset = 1 * 60 * 60 * 1000
  
  def localPlusHoursZone(hours: Int) = DateTimeZone.forOffsetMillis(defaultOffset + hours * hourOffset)
}
