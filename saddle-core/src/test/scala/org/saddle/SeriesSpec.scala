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

import org.specs2.mutable.Specification
import time._

class SeriesSpec extends Specification {
  "reindex works on dates" in {
    val s1 = Series(Vec(1d, 2, 3), Index(datetime(2005,1,1), datetime(2005,1,2), datetime(2005,1,3)))
    val s2 = Series(Vec(5d, 7), Index(datetime(2005,1,1), datetime(2005,1,3)))

    s2.reindex(s1.index).index must_== s1.index
  }

  "non-spec primitive groupby must work" in {
    val s = Series('a' -> 1, 'b' -> 2, 'b' -> 3)
    s.groupBy.combine(_.first.getOrElse(0)) must_== Series('a' -> 1, 'b' -> 2)
  }

  "map works" in {
    val s = Series('a' -> 1, 'b' -> 2, 'b' -> 3)
    s.map { case (k, v) => (k, v+1) } must_== s + 1
  }

  "flatMap works" in {
    val s = Series('a' -> 1, 'b' -> 2, 'b' -> 3)
    s.flatMap { case (k, v) => Some((k, v+1)) } must_== s + 1
  }
  
  "rolling works" in {
    val s = Series(Vec(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0))
    
    s.rolling(2, _.mean) must_== Series(Vec(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5), s.index.tail(s.length -1))
  }
  
  "rolling without dropping NA works" in {
    val s = Series(Vec(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0))
    
    s.rolling(2, _.mean, dropNA=false) must_== Series(Vec(Double.NaN, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5))
    s.rolling(8, _.mean, dropNA=false) must_== Series(Vec(Double.NaN, Double.NaN, Double.NaN, Double.NaN, Double.NaN, Double.NaN, Double.NaN, 4.5))
    s.rolling(9, _.mean, dropNA=false) must_== Series(Vec(Double.NaN, Double.NaN, Double.NaN, Double.NaN, Double.NaN, Double.NaN, Double.NaN, Double.NaN))
  }
}
