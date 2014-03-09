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
import org.joda.time.{DateTime, DateTimeZone}

class RRuleCheck extends Specification with ScalaCheck {

  "RRule Tests" in {
   
    "RRule(DAILY).from(dt) must return an iterator that starts with dt" in {
      val zoneNY = DateTimeZone.forID("America/New_York")
      val zoneSP = DateTimeZone.forID("America/Sao_Paulo")
      
      // Ensures that the time zone of the testing datetime is different than the default one
      val zone = if (DateTimeZone.getDefault == zoneNY) zoneSP else zoneNY
      
      val dt = new DateTime(2002, 11, 8, 0, 0, 0, 0, zone)
      
      val it = RRule(DAILY) from dt
      
      it.next must_== dt
    }
  }
}
