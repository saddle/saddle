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
package org.saddle.csv

import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import org.saddle.{na, Index, Vec, Frame}

class CsvCheck extends Specification with ScalaCheck {
  "csv string parsing and writing works" in {

    val expect = Frame(
      Vec("1", "4", "5", "7"),
      Vec("25", "55", "9", "8"),
      Vec("36", "6", "38", "9")
    ).setColIndex(Index("a", "b,c,d", "e"))

    val parsedBack = (CsvParser
      .parse(
        scala.io.Source
          .fromString(new String(CsvWriter.writeFrameToArray(expect)))
          .getLines
      )
      .withColIndex(0)
      .withRowIndex(0)
      .mapRowIndex(_.toInt))

    parsedBack must_== expect
  }
  "csv string parsing works" in {
    val data =
      """a,"b,c,d",e
        |1,25,36,
        |4,55, "6"
        |5,9,38
        |7, "8",    "9",   """.stripMargin

    val src = scala.io.Source.fromString(data).getLines

    val frame = CsvParser.parse(src).withColIndex(0).resetRowIndex
    val expect = Frame(
      Vec("1", "4", "5", "7"),
      Vec("25", "55", "9", "8"),
      Vec("36", "6", "38", "9")
    ).setColIndex(Index("a", "b,c,d", "e"))

    frame must_== expect
  }

  "csv int parsing works" in {
    val data =
      """a,"b,c,d",e
        |1,2,3
        |4,5,"test",
        |7, "8",    "9",   """.stripMargin

    val src = scala.io.Source.fromString(data).getLines

    val frame = CsvParser
      .parse(src)
      .withColIndex(0)
      .resetRowIndex
      .mapValues(CsvParser.parseInt)
    val expect = Frame(Vec(1, 4, 7), Vec(2, 5, 8), Vec(3, na.to[Int], 9))
      .setColIndex(Index("a", "b,c,d", "e"))

    frame must_== expect
  }

  "csv fails on irregular row" in {
    val data =
      """a,"b,c,d",e
        |1,2,3
        |4,5
        |7, "8",    "9",   """.stripMargin

    val src = scala.io.Source.fromString(data).getLines

    CsvParser.parse(src) must throwAn[ArrayIndexOutOfBoundsException]
  }

  "csv parsing still works when final field is empty" in {
    val data =
      """1,2,3
       |1,2,""".stripMargin

    val src = scala.io.Source.fromString(data).getLines

    CsvParser.parse(src) must throwAn[ArrayIndexOutOfBoundsException].not
  }
}
