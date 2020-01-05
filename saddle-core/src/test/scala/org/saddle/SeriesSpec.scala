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
import org.saddle.ops.BinOps._
class SeriesSpec extends Specification {
  "table works" in {
    Series.table(Vec(4, 1, 2, 2, 1, 3, 1)) must_== Series(
      4 -> 1,
      1 -> 3,
      2 -> 2,
      3 -> 1
    )
  }
  "table works" in {
    Series.table(Vec[Int]()) must_== Series[Int, Int]()
  }
  "Factory methods work" in {
    Series(Vec(1, 2, 3), Index("a", "b", "c")) must_== Series(
      Index("a", "b", "c"),
      Vec(1, 2, 3)
    )
  }

  "Seq[(A,B)] converts to Series" in {
    Seq(1 -> 2).toSeries must_== Series(1 -> 2)
  }
  "Updated works" in {
    val s = Seq(1 -> 2, 2 -> 3, 3 -> 4).toSeries
    s.updated(5, 0) must_== s
  }
  "Updated works" in {
    val s = Seq(1 -> 2, 2 -> 3, 3 -> 4).toSeries
    s.updated(5, 1) must_== Seq(1 -> 5, 2 -> 3, 3 -> 4).toSeries
  }
  "Updated works" in {
    val s = Seq(1 -> 2, 2 -> 3, 3 -> 4).toSeries
    s.updated(5, Array(2, 1, 0)) must_== Seq(1 -> 5, 2 -> 5, 3 -> 4).toSeries
  }
  "Updated works" in {
    val s = Seq(1 -> 2, 2 -> 3, 3 -> 4).toSeries
    s.updated(5, Array.empty[Int]) must_== s
  }

  "non-spec primitive groupby must work" in {
    val s = Series('a' -> 1, 'b' -> 2, 'b' -> 3)
    s.groupBy.combine(_.first.getOrElse(0)) must_== Series('a' -> 1, 'b' -> 2)
  }

  "map works" in {
    val s = Series('a' -> 1, 'b' -> 2, 'b' -> 3)
    s.map { case (k, v) => (k, v + 1) } must_== s + 1
  }

  "flatMap works" in {
    val s = Series('a' -> 1, 'b' -> 2, 'b' -> 3)
    s.flatMap { case (k, v) => Some((k, v + 1)) } must_== s + 1
  }
}
