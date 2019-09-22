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
  "Seq[(A,B,C)] converts to a Frame" in {
    Seq((1,2,3)).toFrame must_== Frame(1 -> Series(2->3)).T
  }

  "AddRow works" in {
    Frame(1 -> Series(1 -> 1, 2->2,3->3), 2 -> Series(1->4,2->5,3->6)).addRow(Series(3,4,5),2) must_== 
    Frame(1 -> Series(1 -> 1, 2->2,3->3, 2->4), 2 -> Series(1->4,2->5,3->6, 2->5))
  }
  "AddCols works" in {
    Frame(1 -> Series(1 -> 1, 2->2,3->3), 2 -> Series(1->4,2->5,3->6)).addCol(Series(7,8,9,10),2) must_== 
    Frame(1 -> Series(1 -> 1, 2->2,3->3), 2 -> Series(1->4,2->5,3->6),2 -> Series(1->8,2->9,3->10))
  }

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
}
