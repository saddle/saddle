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
import org.saddle.index.InnerJoin
import org.saddle.ops.BinOps._
/**
  * Specs for a Frame
  */
class FrameSpec extends Specification {

  val testFrame = Frame(
    1 -> Series(1 -> "1,1", 2 -> "2,1", 4 -> "4,1", 5 -> "5,1"),
    2 -> Series(1 -> "1,2", 2 -> "2,2", 4 -> "4,2", 5 -> "5,2"),
    3 -> Series(1 -> "1,3", 2 -> "2,3", 4 -> null, 5 -> "5,3"),
    5 -> Series(1 -> "1,5", 2 -> "2,5", 4 -> "4,5", 5 -> "5,5")
  )
  "join" in {
    val f2 = Frame(
      0 -> Series(
        1 -> "1,0",
        2 -> "2,0",
        4 -> "4,0",
        5 ->
          "5,0"
      )
    )
    val joined = testFrame.cbind(f2)
    joined must_== Frame(testFrame.toColSeq ++ f2.toColSeq: _*)
  }

  "colSplitBy" in {
    val (left, right) = testFrame.colSplitBy(0)
    (right must_== testFrame) and (left must_== Frame.empty[Int, Int, String])
  }
  "colSplitBy" in {
    val (left, right) = testFrame.colSplitBy(1)

    (right must_== testFrame) and (left must_== Frame.empty[Int, Int, String])
  }
  "colSplitBy" in {
    val (left, right) = testFrame.colSplitBy(2)
    (left must_== testFrame.col(1)) and (right must_== testFrame.col(2, 3, 5))
  }
  "row" in {
    val slice = testFrame.row(2 -> 5)
    slice must_== testFrame.row(2, 4, 5)
  }
  "rowSliceBy" in {
    val slice = testFrame.rowSliceBy(-1, 0)
    slice.numRows must_== 0
  }
  "rowSliceBy" in {
    val slice = testFrame.rowSliceBy(3, 5)
    slice must_== testFrame.row(4, 5)
  }
  "rowAt" in {
    testFrame.rowAt(0, 1, 2) must_== testFrame.row(1, 2, 4)
  }
  "rowAt" in {
    testFrame.rowAt(0 -> 2) must_== testFrame.row(1, 2, 4)
  }
  "colSplitBy" in {
    val (left, right) = testFrame.colSplitBy(0)
    (right must_== testFrame) and (left must_== Frame.empty[Int, Int, String])
  }
  "colSplitBy" in {
    val (left, right) = testFrame.colSplitBy(1)

    (right must_== testFrame) and (left must_== Frame.empty[Int, Int, String])
  }
  "colSplitBy" in {
    val (left, right) = testFrame.colSplitBy(2)
    (left must_== testFrame.col(1)) and (right must_== testFrame.col(2, 3, 5))
  }
  "rowSplitBy" in {
    val (left, right) = testFrame.rowSplitBy(0)
    (right must_== testFrame) and (left.numRows must_== 0)
  }
  "rowSplitBy" in {
    val (left, right) = testFrame.rowSplitBy(1)

    (right must_== testFrame) and (left.numRows must_== 0)
  }
  "rowSplitBy" in {
    val (left, right) = testFrame.rowSplitBy(2)
    (left must_== testFrame.row(1)) and (right must_== testFrame.row(2, 4, 5))
  }
  "raw" in {
    testFrame.raw(2, 3) must_== "4,5"
  }
  "reindex" in {
    testFrame.reindex(cix = Index(3, 2, 5, 1), rix = Index(4, 2, 5, 1)) must_==
      Frame(
        3 -> Series(4 -> null, 2 -> "2,3", 5 -> "5,3", 1 -> "1,3"),
        2 -> Series(4 -> "4,2", 2 -> "2,2", 5 -> "5,2", 1 -> "1,2"),
        5 -> Series(4 -> "4,5", 2 -> "2,5", 5 -> "5,5", 1 -> "1,5"),
        1 -> Series(4 -> "4,1", 2 -> "2,1", 5 -> "5,1", 1 -> "1,1")
      )
  }
  "mapRowIndex" in {
    (testFrame.mapRowIndex(_ * 2).rowIx must_== testFrame.rowIx.map(_ * 2)) and
      (testFrame.mapRowIndex(_ * 2).toMat must_== testFrame.toMat)
  }
  "mapColIndex" in {
    (testFrame.mapColIndex(_ * 2).colIx must_== testFrame.colIx.map(_ * 2)) and
      (testFrame.mapColIndex(_ * 2).toMat must_== testFrame.toMat)
  }
  "head" in {
    testFrame.head(2) must_== testFrame.toRowSeq.take(2).toFrame.T
  }
  "headCol" in {
    testFrame.headCol(2) must_== testFrame.toColSeq.take(2).toFrame
  }
  "firstCol" in {
    testFrame.firstCol(3) must_== Series(
      1 -> "1,3",
      2 -> "2,3",
      4 -> null,
      5 -> "5,3"
    )
  }
  "apply" in {
    testFrame.apply(1 -> 4, 2 -> 5) must_==
      Frame(
        2 -> Series(1 -> "1,2", 2 -> "2,2", 4 -> "4,2"),
        3 -> Series(1 -> "1,3", 2 -> "2,3", 4 -> null),
        5 -> Series(1 -> "1,5", 2 -> "2,5", 4 -> "4,5")
      )
  }
  "withRowIndex" in {
    testFrame.withRowIndex(0, 1) must_== {
      val idx = (testFrame.colAt(0).toVec.toSeq zip testFrame
        .colAt(1)
        .toVec
        .toSeq).toIndex
      testFrame.colAt(2, 3).setRowIndex(idx)
    }
  }
  "withColIndex" in {
    testFrame.withColIndex(0, 1) must_== {
      val idx = (testFrame.rowAt(0).toVec.toSeq zip testFrame
        .rowAt(1)
        .toVec
        .toSeq).toIndex
      testFrame.rowAt(2, 3).setColIndex(idx)
    }
  }
  "resetColIndex" in {
    testFrame.resetColIndex must_==
      testFrame.setColIndex(0 until testFrame.numCols toIndex)
  }
  "tail" in {
    testFrame.tail(2) must_== testFrame.toRowSeq.takeRight(2).toFrame.T
  }
  "tailCol" in {
    testFrame.tailCol(2) must_== testFrame.toColSeq.takeRight(2).toFrame
  }
  "first" in {
    testFrame.first(1) must_== testFrame.toRowSeq.find(_._1 == 1).get._2
  }
  "sortedRIx" in {
    testFrame.reindexRow(rix = Index(4, 2, 5, 1)).sortedRIx must_== testFrame
  }
  "sortedCIx" in {
    testFrame.reindexCol(cix = Index(3, 2, 5, 1)).sortedCIx must_== testFrame
  }
  "sortedRows" in {
    Frame(1 -> Series(4, 3, 2, 1)).sortedRows(0) must_== Frame(
      1 -> Series(3 -> 1, 2 -> 2, 1 -> 3, 0 -> 4)
    )
  }
  "sortedCols" in {
    Frame(1 -> Series(4, 3, 2, 1)).T.sortedCols(0) must_== Frame(
      1 -> Series(3 -> 1, 2 -> 2, 1 -> 3, 0 -> 4)
    ).T
  }
  "sortedColsBy" in {
    Frame(1 -> Series(4, 3, 2, 1)).T.sortedColsBy(_.at(0)) must_== Frame(
      1 -> Series(3 -> 1, 2 -> 2, 1 -> 3, 0 -> 4)
    ).T
  }
  "mask" in {
    testFrame.mask(_.startsWith("1")) must_== Frame(
      1 -> Series(1 -> null, 2 -> "2,1", 4 -> "4,1", 5 -> "5,1"),
      2 -> Series(1 -> null, 2 -> "2,2", 4 -> "4,2", 5 -> "5,2"),
      3 -> Series(1 -> null, 2 -> "2,3", 4 -> null, 5 -> "5,3"),
      5 -> Series(1 -> null, 2 -> "2,5", 4 -> "4,5", 5 -> "5,5")
    )
  }
  "mask" in {
    testFrame.mask(Vec(true, false, false, false)) must_== Frame(
      1 -> Series(1 -> null, 2 -> "2,1", 4 -> "4,1", 5 -> "5,1"),
      2 -> Series(1 -> null, 2 -> "2,2", 4 -> "4,2", 5 -> "5,2"),
      3 -> Series(1 -> null, 2 -> "2,3", 4 -> null, 5 -> "5,3"),
      5 -> Series(1 -> null, 2 -> "2,5", 4 -> "4,5", 5 -> "5,5")
    )
  }
  "rmask" in {
    val b = Vec(true, false, false, false)
    testFrame.T.rmask(b).T must_== testFrame.mask(b)
  }
  "joinMap" in {
    val testframe2 = Frame(
      1 -> Series(1 -> "1,1", 2 -> "2,1", 3 -> "3,1", 4 -> "4,1"),
      2 -> Series(1 -> "1,2", 2 -> "2,2", 3 -> "3,2", 4 -> "4,2"),
      3 -> Series(1 -> "1,3", 2 -> "2,3", 3 -> null, 4 -> "4,3"),
      4 -> Series(1 -> "1,4", 2 -> "2,4", 3 -> "3,3", 4 -> "4,4")
    )
    testFrame.joinMap(testframe2)(_ + _) must_==
      Frame(
        1 -> Series(1 -> "1,11,1", 2 -> "2,12,1", 4 -> "4,14,1", 5 -> null),
        2 -> Series(1 -> "1,21,2", 2 -> "2,22,2", 4 -> "4,24,2", 5 -> null),
        3 -> Series(1 -> "1,31,3", 2 -> "2,32,3", 4 -> null, 5 -> null),
        4 -> Series[Int, String](1 -> null, 2 -> null, 4 -> null, 5 -> null)
      )
  }

  "groupBy" in {
    testFrame.groupBy.combine(_.toSeq.reduce(_ + _)) must_== testFrame
  }
  "groupBy" in {
    testFrame.groupBy(_ % 2).combine(_.toSeq.reduce(_ + _)) must_==
      Frame(
        0 -> Series(
          1 -> "2,14,1",
          2 -> "2,24,2",
          3 -> "2,3null",
          5 -> "2,54,5"
        ),
        1 -> Series(1 -> "1,15,1", 2 -> "1,25,2", 3 -> "1,35,3", 5 -> "1,55,5")
      ).T
  }
  "groupBy" in {
    testFrame.groupBy(Index(1, 0, 0, 1)).combine(_.toSeq.reduce(_ + _)) must_==
      Frame(
        0 -> Series(
          1 -> "2,14,1",
          2 -> "2,24,2",
          3 -> "2,3null",
          5 -> "2,54,5"
        ),
        1 -> Series(1 -> "1,15,1", 2 -> "1,25,2", 3 -> "1,35,3", 5 -> "1,55,5")
      ).T
  }

  "shift" in {
    testFrame.shift(2) must_== Frame(
      1 -> Series(1 -> null, 2 -> null, 4 -> "1,1", 5 -> "2,1"),
      2 -> Series(1 -> null, 2 -> null, 4 -> "1,2", 5 -> "2,2"),
      3 -> Series(1 -> null, 2 -> null, 4 -> "1,3", 5 -> "2,3"),
      5 -> Series(1 -> null, 2 -> null, 4 -> "1,5", 5 -> "2,5")
    )
  }

  "filterIx" in {
    testFrame.filterIx(_ % 2 == 0) must_== testFrame.toColSeq
      .filter(_._1 % 2 == 0)
      .toFrame
  }
  "rfilterIx" in {
    testFrame.rfilterIx(_ % 2 == 0) must_== testFrame.toRowSeq
      .filter(_._1 % 2 == 0)
      .toFrame
      .T
  }

  "Seq[(A,B,C)] converts to a Frame" in {
    Seq((1, 2, 3)).toFrame must_== Frame(1 -> Series(2 -> 3)).T
  }

  "AddRow works" in {
    Frame(
      1 -> Series(1 -> 1, 2 -> 2, 3 -> 3),
      2 -> Series(1 -> 4, 2 -> 5, 3 -> 6)
    ).addRow(Series(3, 4, 5), 2, InnerJoin) must_==
      Frame(
        1 -> Series(1 -> 1, 2 -> 2, 3 -> 3, 2 -> 4),
        2 -> Series(1 -> 4, 2 -> 5, 3 -> 6, 2 -> 5)
      )
  }
  "AddCols works" in {
    Frame(
      1 -> Series(1 -> 1, 2 -> 2, 3 -> 3),
      2 -> Series(1 -> 4, 2 -> 5, 3 -> 6)
    ).addCol(Series(7, 8, 9, 10), 2, InnerJoin) must_==
      Frame(
        1 -> Series(1 -> 1, 2 -> 2, 3 -> 3),
        2 -> Series(1 -> 4, 2 -> 5, 3 -> 6),
        2 -> Series(1 -> 8, 2 -> 9, 3 -> 10)
      )
  }

  "Frame.empty behaves as expected" in {
    Frame("a" -> Vec.empty[Int], "b" -> Vec.empty[Int]).isEmpty must_== true
  }

  "shift-merge must work" in {
    val s1 = org.saddle.Series(Vec(1, 2, 3), Index("a", "b", "c"))
    val mergeShift = s1.join(s1.shift(1))
    mergeShift.row("b") must_== Frame(
      0 -> Series("b" -> 2),
      1 -> Series("b" -> 1)
    )
  }

  "map works" in {
    val f = Frame(
      "a" -> Series("x" -> 1, "y" -> 2, "z" -> 3),
      "b" -> Series("x" -> 4, "y" -> 5, "z" -> 6)
    )
    f.map { case (r, c, v) => (r, c, v + 1) } must_== f + 1
  }

  "flatMap works" in {
    val f = Frame(
      "a" -> Series("x" -> 1, "y" -> 2, "z" -> 3),
      "b" -> Series("x" -> 4, "y" -> 5, "z" -> 6)
    )
    f.flatMap { case (r, c, v) => Some((r, c, v + 1)) } must_== f + 1
  }

  "colType works within rfilter" in {
    val strVec = Vec("string", "another string", "unrelated")
    val intVec = vec.randi(3)
    val df = Panel(strVec, intVec)
    val df2 =
      df.rfilter(x => x.get(0).map(_.toString).getOrElse("").contains("string"))
    df2.colType[Int] must_!= Frame.empty[Int, Int, Int]
    df2.colType[String] must_!= Frame.empty[Int, Int, String]
  }
  "table" in {
    Frame.table(Vec(1 -> 'a', 1 -> 'a', 2 -> 'b')) must_== Frame(
      1 -> Series('a' -> 2),
      2 -> Series('b' -> 1)
    ).T
  }
}
