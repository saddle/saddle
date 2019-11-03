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
import org.saddle.{Index, Vec, Frame, ST, na}

class CsvCheck extends Specification with ScalaCheck {
  val crlf = "\r\n"
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
      )
      .withColIndex(0)
      .withRowIndex(0)
      .mapRowIndex(_.toInt))

    parsedBack must_== expect
  }
  "csv string parsing works" in {
    val data =
      s"""a,"b,c,d",e${crlf}1,25,36,${crlf}4,55,"6"${crlf}5,9,38${crlf}7,"8","9",   """

    val src = scala.io.Source.fromString(data)
    val frame =
      CsvParser.parse(src, bufferSize = 2).withColIndex(0).resetRowIndex
    val expect = Frame(
      Vec("1", "4", "5", "7"),
      Vec("25", "55", "9", "8"),
      Vec("36", "6", "38", "9")
    ).setColIndex(Index("a", "b,c,d", "e"))
    frame must_== expect
  }
  "csv string parsing works with double quotes and quoted CRLF and unquoted CR" in {
    val data =
      s"""a,"b,""c"",d",e${crlf}1,25${'\r'}1,${'\r'}${'\r'}${'\r'}36,${crlf}4,5${'\r'}${'\r'}5,"6${crlf}1"${crlf}5,  ,38${crlf}7,"8${'\r'}1","9",   """

    val src = scala.io.Source.fromString(data)
    val frame =
      CsvParser.parse(src, bufferSize = 2).withColIndex(0).resetRowIndex
    val expect = Frame(
      Vec("1", "4", "5", "7"),
      Vec(s"25${'\r'}1", s"5${'\r'}${'\r'}5", "  ", s"8${'\r'}1"),
      Vec(s"${'\r'}${'\r'}${'\r'}36", s"6${crlf}1", "38", "9")
    ).setColIndex(Index("a", """b,""c"",d""", "e"))
    frame must_== expect
  }
  "quoted empty string" in {
    val data =
      s"""""${crlf}1"""

    val src = scala.io.Source.fromString(data)
    val frame =
      CsvParser.parse(src, bufferSize = 2).withColIndex(0).resetRowIndex
    val expect = Frame(
      Vec("1")
    ).setColIndex(Index(""))
    frame must_== expect
  }
  "quoted empty string 2" in {
    val data =
      s"""1${crlf}"""""

    val src = scala.io.Source.fromString(data)
    val frame =
      CsvParser.parse(src, bufferSize = 2).withColIndex(0).resetRowIndex
    val expect = Frame(
      Vec("")
    ).setColIndex(Index("1"))
    frame must_== expect
  }

  "csv int parsing works" in {
    val data =
      s"""a,"b,c,d",e${crlf}1,2,3${crlf}4,5,"test"${crlf}7,"8","9",""".stripMargin

    val src = scala.io.Source.fromString(data)

    val frame = CsvParser
      .parse(src)
      .withColIndex(0)
      .resetRowIndex
      .mapValues(implicitly[ST[Int]].parse)
    val expect = Frame(Vec(1, 4, 7), Vec(2, 5, 8), Vec(3, na.to[Int], 9))
      .setColIndex(Index("a", "b,c,d", "e"))
    frame must_== expect
  }

  "csv fails on irregular row" in {
    val data =
      s"""a,"b,c,d",e${"\r\n"}1,2,3${"\r\n"}4,5${"\r\n"}7,"8","9",   """.stripMargin

    val src = scala.io.Source.fromString(data)

    CsvParser.parse(src) must throwAn[RuntimeException]
  }

  "csv parsing still works when final field is empty" in {
    val data =
      """1,2,3
       |1,2,""".stripMargin

    val src = scala.io.Source.fromString(data)

    CsvParser.parse(src) must throwAn[ArrayIndexOutOfBoundsException].not
  }
}
