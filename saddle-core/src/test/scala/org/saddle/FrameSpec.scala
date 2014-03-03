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

/**
 * Specs for a Frame
 */
class FrameSpec extends Specification {
  "Frame.empty behaves as expected" in {
    Frame("a" -> Vec.empty[Int], "b" -> Vec.empty[Int]).isEmpty must_== true
  }

  "shift-merge must work" in {
    val s1 = org.saddle.Series(Vec(1,2,3), Index("a", "b", "c"))
    val mergeShift = s1.join(s1.shift(1))
    mergeShift.row("b") must_== Frame(0 -> Series("b" -> 2), 1 -> Series("b" -> 1))
  }

  "map works" in {
    val f = Frame("a" -> Series("x" -> 1, "y" -> 2, "z" -> 3), "b" -> Series("x" -> 4, "y" -> 5, "z" -> 6))
    f.map { case (r, c, v) => (r, c, v + 1) } must_== f + 1
  }

  "flatMap works" in {
    val f = Frame("a" -> Series("x" -> 1, "y" -> 2, "z" -> 3), "b" -> Series("x" -> 4, "y" -> 5, "z" -> 6))
    f.flatMap { case (r, c, v) => Some((r, c, v + 1)) } must_== f + 1
  }

  "colType works within rfilter" in {
    val strVec = Vec("string", "another string", "unrelated")
    val intVec = vec.randi(3)
    val df = Panel(strVec, intVec)
    val df2 = df.rfilter(x => x.get(0).map(_.toString).getOrElse("").contains("string"))
    df2.colType[Int] must_!= Frame.empty[Int, Int, Int]
    df2.colType[String] must_!= Frame.empty[Int, Int, String]
  }
  
  "rolling works" in {
    val s = Series(Vec(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0))
    val f = Frame(0 -> s)
    
    f.rolling(2, _.mean) must_== Frame(0 -> s.rolling(2, _.mean))
  }
  
  "rolling without dropping NA works" in {
    val s = Series(Vec(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0))
    val f = Frame(0 -> s)
    
    f.rolling(2, _.mean, dropNA=false) must_== Frame(0 -> s.rolling(2, _.mean, dropNA=false))
    f.rolling(8, _.mean, dropNA=false) must_== Frame(0 -> s.rolling(8, _.mean, dropNA=false))
    f.rolling(9, _.mean, dropNA=false) must_== Frame(0 -> s.rolling(9, _.mean, dropNA=false))
  }
}
