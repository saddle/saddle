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

package org.saddle.scalar

import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import org.saddle.DateTimeZoneUtils
import org.saddle.Index
import org.saddle.Vec
import org.saddle.time.dtOrdering
import org.joda.time._

class ScalarTagTimeCheck extends Specification with ScalaCheck {
  
  "ScalarTagTime" in {
    import ScalarTagTime._
    import DateTimeZoneUtils._

    val zone1 = localPlusHoursZone(3)
    val zone2 = localPlusHoursZone(4)

    "should convert Array[DateTime] to Array[Long]" in {
      val dateTimes = Array(
        new DateTime(2014, 3, 31, 0, 0, 0),
        new DateTime(2014, 4, 12, 0, 0, 0)
      )

      val dateTimesNY = dateTimes.map(_.withZoneRetainFields(zoneNY))

      val timestampsNY = Array(
        1396238400000L,
        1397275200000L
      )

      val dateTimesSP = dateTimes.map(_.withZoneRetainFields(zoneSP))

      val timestampsSP = Array(
        1396234800000L,
        1397271600000L
      )

      time2LongArray(dateTimesNY) must_== timestampsNY
      time2LongArray(dateTimesSP) must_== timestampsSP
    }

    "should make Vec[DateTime] from Array[DateTime] with same time zone" in {
      val arr = Array(
        new DateTime(2014, 3, 31, 0, 0, 0, zone1),
        new DateTime(2014, 4, 12, 0, 0, 0, zone1)
      )

      val vec = Vec(
        new DateTime(2014, 3, 31, 0, 0, 0, zone1),
        new DateTime(2014, 4, 12, 0, 0, 0, zone1)
      )

      makeVec(arr) must_== vec
    }

    "should make Vec[DateTime] from Array[DateTime] with different time zone" in {
      val arr = Array(
        new DateTime(2014, 3, 31, 0, 0, 0, zone1),
        new DateTime(2014, 4, 12, 1, 0, 0, zone2)
      )

      val vec = Vec(
        new DateTime(2014, 3, 31, 0, 0, 0, zone1),
        new DateTime(2014, 4, 12, 0, 0, 0, zone1)
      )

      makeVec(arr) must_== vec
    }

    "should make Index[DateTime] from Array[DateTime] with same time zone" in {
      val arr = Array(
        new DateTime(2014, 3, 31, 0, 0, 0, zone1),
        new DateTime(2014, 4, 12, 0, 0, 0, zone1)
      )

      val idx = Index(
        new DateTime(2014, 3, 31, 0, 0, 0, zone1),
        new DateTime(2014, 4, 12, 0, 0, 0, zone1)
      )

      makeIndex(arr) must_== idx
    }

    "should make Index[DateTime] from Array[DateTime] with different time zone" in {
      val arr = Array(
        new DateTime(2014, 3, 31, 0, 0, 0, zone1),
        new DateTime(2014, 4, 12, 1, 0, 0, zone2)
      )

      val idx = Index(
        new DateTime(2014, 3, 31, 0, 0, 0, zone1),
        new DateTime(2014, 4, 12, 0, 0, 0, zone1)
      )

      makeIndex(arr) must_== idx
    }

    "should concat a seq of Vec[DateTime] with same time zone" in {
      val vecs = IndexedSeq(
        Vec(new DateTime(2014, 3, 31, 0, 0, 0, zone1)),
        Vec(new DateTime(2014, 4, 12, 0, 0, 0, zone1))
      )

      val vec = Vec(
        new DateTime(2014, 3, 31, 0, 0, 0, zone1),
        new DateTime(2014, 4, 12, 0, 0, 0, zone1)
      )

      concat(vecs) must_== vec
    }

    "should concat a seq of Vec[DateTime] with different time zone" in {
      val vecs = IndexedSeq(
        Vec(new DateTime(2014, 3, 31, 0, 0, 0, zone1)),
        Vec(new DateTime(2014, 4, 12, 1, 0, 0, zone2))
      )

      val vec = Vec(
        new DateTime(2014, 3, 31, 0, 0, 0, zone1),
        new DateTime(2014, 4, 12, 0, 0, 0, zone1)
      )

      concat(vecs) must_== vec
    }
  }
}
