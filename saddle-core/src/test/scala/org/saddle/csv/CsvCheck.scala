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
import org.saddle.{Index, Vec, Frame, na, ST}
import java.nio.charset.Charset

class CsvCheck extends Specification with ScalaCheck {
  val crlf = "\r\n"
  val lf = "\n"
  "file reading works" in {
    val tmp = java.io.File.createTempFile("test", "test")
    val os = new java.io.FileOutputStream(tmp)
    val text = "abcdefgh"
    os.write(text.getBytes("US-ASCII"))
    os.close
    val text2 = CsvParser
      .readFile(tmp, bufferSize = 2)
      .map(_.toString)
      .mkString
    text must_== text2
  }
  "file reading works, utf16" in {
    val tmp = java.io.File.createTempFile("test", "test")
    val os = new java.io.FileOutputStream(tmp)
    val text = "abcdefgh…πœ"
    os.write(text.getBytes("UTF-16"))
    os.close
    val text2 = CsvParser
      .readFile(tmp, bufferSize = 2, Charset.forName("UTF-16"))
      .map(_.toString)
      .mkString
    text must_== text2
  }
  "csv string parsing and writing works" in {

    val expect = Frame(
      Vec("1", "4", "5", "7"),
      Vec("25", "55", "9", "8"),
      Vec("36", "6", "38", "9")
    ).setColIndex(Index("a", "b,c,d", "e"))

    val parsedBack = (CsvParser
      .parseSource[String](
        scala.io.Source
          .fromString(new String(CsvWriter.writeFrameToArray(expect)))
      )
      .right
      .get
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
      CsvParser
        .parseSource[String](src, bufferSize = 2)
        .right
        .get
        .withColIndex(0)
        .resetRowIndex
    val expect = Frame(
      Vec("1", "4", "5", "7"),
      Vec("25", "55", "9", "8"),
      Vec("36", "6", "38", "9")
    ).setColIndex(Index("a", "b,c,d", "e"))
    frame must_== expect
  }
  "csv int parsing works" in {
    val data =
      s"""a,"b,c,d",e${crlf}1,25,36,${crlf}4,55,"6"${crlf}5,9,38${crlf}7,"8","9",   """

    val src = scala.io.Source.fromString(data)
    val frame =
      CsvParser
        .parseSourceWithHeader[Int](src, bufferSize = 2)
        .right
        .get
        .resetRowIndex
    val expect = Frame(
      Vec("1", "4", "5", "7"),
      Vec("25", "55", "9", "8"),
      Vec("36", "6", "38", "9")
    ).setColIndex(Index("a", "b,c,d", "e")).mapValues(_.toInt)
    frame must_== expect
  }
  "csv string parsing works, two lines" in {
    val data =
      s"""a,"b,c,d",e${crlf}1,25,36,${crlf}4,55,"6"${crlf}5,9,38${crlf}7,"8","9",   """

    val src = scala.io.Source.fromString(data)
    val frame =
      CsvParser
        .parseSource[String](src, bufferSize = 2, maxLines = 2)
        .right
        .get
        .withColIndex(0)
        .resetRowIndex
    val expect = Frame(
      Vec("1"),
      Vec("25"),
      Vec("36")
    ).setColIndex(Index("a", "b,c,d", "e"))
    frame must_== expect
  }
  "csv string parsing works, take first line" in {
    val data =
      s"""a,"b,c,d",e${crlf}1,25,36,${crlf}4,55,"6"${crlf}5,9,38${crlf}7,"8","9",   """

    val src = scala.io.Source.fromString(data)
    val frame =
      CsvParser
        .parseSource[String](src, bufferSize = 2, maxLines = 1)
        .right
        .get
        .withColIndex(0)
        .resetRowIndex
    val expect = Frame(
      Vec.empty[Double],
      Vec.empty[Double],
      Vec.empty[Double]
    ).setColIndex(Index("a", "b,c,d", "e"))
    frame must_== expect
  }
  "csv string parsing works with LF line endings" in {
    val data =
      s"""a,"b,c,d",e${lf}1,25,36,${lf}4,55,"6"${lf}5,9,38${lf}7,"8","9",   """

    val src = scala.io.Source.fromString(data)
    val frame =
      CsvParser
        .parseSource[String](src, bufferSize = 2, recordSeparator = lf)
        .right
        .get
        .withColIndex(0)
        .resetRowIndex
    val expect = Frame(
      Vec("1", "4", "5", "7"),
      Vec("25", "55", "9", "8"),
      Vec("36", "6", "38", "9")
    ).setColIndex(Index("a", "b,c,d", "e"))
    frame must_== expect
  }
  "csv string parsing works with double quotes and quoted CRLF and unquoted CR" in {
    val data =
      s""","b,""c"",d",e${crlf}1,25${'\r'}1,${'\r'}${'\r'}${'\r'}36,${crlf}4,5${'\r'}${'\r'}5,"6${crlf}1"${crlf}5,  ,38${crlf}7,"8${'\r'}1","",   """

    val src = scala.io.Source.fromString(data)
    val frame =
      CsvParser
        .parseSource[String](src, bufferSize = 2)
        .right
        .get
        .withColIndex(0)
        .resetRowIndex
    val expect = Frame(
      Vec("1", "4", "5", "7"),
      Vec(s"25${'\r'}1", s"5${'\r'}${'\r'}5", "  ", s"8${'\r'}1"),
      Vec(s"${'\r'}${'\r'}${'\r'}36", s"6${crlf}1", "38", "")
    ).setColIndex(Index("", """b,""c"",d""", "e"))
    frame must_== expect
  }
  "csv string parsing works with double quotes and quoted CRLF and unquoted CR with LF line endings" in {
    val data =
      s""","b,""c"",d",e${lf}1,25${'\r'}1,${'\r'}${'\r'}${'\r'}36,${lf}4,5${'\r'}${'\r'}5,"6${lf}1"${lf}5,  ,38${lf}7,"8${'\r'}1","9",   """

    val src = scala.io.Source.fromString(data)
    val frame =
      CsvParser
        .parseSource[String](src, bufferSize = 2, recordSeparator = lf)
        .right
        .get
        .withColIndex(0)
        .resetRowIndex

    val expect = Frame(
      Vec("1", "4", "5", "7"),
      Vec(s"25${'\r'}1", s"5${'\r'}${'\r'}5", "  ", s"8${'\r'}1"),
      Vec(s"${'\r'}${'\r'}${'\r'}36", s"6${lf}1", "38", "9")
    ).setColIndex(Index("", """b,""c"",d""", "e"))

    frame must_== expect
  }
  "quoted empty string" in {
    val data =
      s"""""${crlf}1"""

    val src = scala.io.Source.fromString(data)
    val frame =
      CsvParser
        .parseSource[String](src, bufferSize = 2)
        .right
        .get
        .withColIndex(0)
        .resetRowIndex
    val expect = Frame(
      Vec("1")
    ).setColIndex(Index(""))
    frame must_== expect
  }
  "quoted empty string with LF line ending" in {
    val data =
      s"""""${lf}1"""

    val src = scala.io.Source.fromString(data)
    val frame =
      CsvParser
        .parseSource[String](src, bufferSize = 2, recordSeparator = lf)
        .right
        .get
        .withColIndex(0)
        .resetRowIndex
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
      CsvParser
        .parseSource[String](src, bufferSize = 2)
        .right
        .get
        .withColIndex(0)
        .resetRowIndex
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
      .parseSource[String](src)
      .right
      .get
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

    CsvParser.parseSource[String](src) must_== Left(
      "Incomplete line 1. Expected 3 fields, got 2.. line=1, field=2"
    )
  }
  "csv fails on bad quoting" in {
    val data =
      s""""a"a""".stripMargin

    val src = scala.io.Source.fromString(data)

    CsvParser.parseSource[String](src) must_== Left(
      "quotes in quoted field must be escaped by doubling them.. line=0, field=0"
    )
  }
  "csv fails on unclosed quoting" in {
    val data =
      s""""aa""".stripMargin

    val src = scala.io.Source.fromString(data)

    CsvParser.parseSource[String](src) must_== Left(
      "Unclosed quote.. line=0, field=0"
    )
  }

  "csv parsing still works when final field is empty" in {
    val data =
      """1,2,3
       |1,2,""".stripMargin

    val src = scala.io.Source.fromString(data)

    CsvParser.parseSource[String](src) must throwAn[
      ArrayIndexOutOfBoundsException
    ].not
  }
}
