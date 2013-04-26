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

package org.saddle.io

import org.saddle._
import collection.mutable.ArrayBuffer
import java.util.concurrent.{Executors, Callable}
import org.saddle.vec.VecString

/**
 * Holds parameters to customize CSV parsing
 *
 * @param separChar The separator; default is comma
 * @param quoteChar Within matching quotes, treat separChar as normal char;
 *                  default is double-quote
 * @param withQuote If true, do not strip quote character from quoted fields
 * @param hasHeader Whether the CSV file has a header row, default true
 * @param skipLines Whether to skip some integer number of lines, default 0
 */
case class CsvParams(separChar: Char    = ',',
                     quoteChar: Char    = '"',
                     withQuote: Boolean = false,
                     hasHeader: Boolean = true,
                     skipLines: Int     = 0)

/**
 * Csv parsing utilities
 */
object CsvParser {
  private val nProcs = java.lang.Runtime.getRuntime.availableProcessors()

  /**
   * Extract data from a CSV data source for populating a Frame.
   *
   * For example,
   *
   * {{{
   *   val src = < some CsvSource >
   *   val data = CsvParser.parse(CsvParser.parseInt)(src)
   *   ...
   *   data.toFrame
   *   data.toFrameNoHeader
   * }}}
   *
   * @param source The csv data source to operate on
   */
  def parse(source: CsvSource): Frame[Int, Int, String] = parse()(source)

  /**
   * Another parse function.
   *
   * @param cols The column offsets to parse (if empty, parse everything)
   * @param params The CsvParams to utilize in parsing
   * @param source The csv data source to operate on
   */
  def parse(cols: Seq[Int] = List(), params: CsvParams = CsvParams())(source: CsvSource): Frame[Int, Int, String] = {

    require(params.separChar != params.quoteChar,
            "Separator character and quote character cannot be the same")

    source.reset()

    // sorted, unique column locations to parse
    var locs = Set(cols : _*).toArray[Int].sorted

    // parse first line
    val firstLine = {
      val line = source.readLine
      if (line == null) sys.error("No data to parse")
      extractAllFields(line, params)
    }

    // what column locations to extract
    if (locs.length == 0) locs = (0 until firstLine.length).toArray

    // set up buffers to store parsed data
    val bufdata = for { c <- locs } yield Buffer[Byte](1024)
    val offsets = for { c <- locs } yield Buffer[Int](1024)
    val lengths = for { c <- locs } yield Buffer[Int](1024)
    val curOffs = for { c <- locs } yield 0

    def addToBuffer(s: String, buf: Int) {
      val bytes = s.getBytes(UTF8)
      val len = bytes.length
      var i = 0
      while(i < len) {
        bufdata(buf).add(bytes(i))
        i += 1
      }
      offsets(buf).add(curOffs(buf))
      lengths(buf).add(len)
      curOffs(buf) += len
    }

    // first line is either header, or needs to be processed
    val fields = Vec(firstLine).take(locs)
    fields.toSeq.zipWithIndex.map { case (s, i) => addToBuffer(s, i) }

    // parse remaining rows
    var str: String = null
    var nln: Int = 0
    while ( { str = source.readLine; str != null } ) {
      extractFields(str, addToBuffer, locs, params)
      nln += 1
    }

    val zipped = { bufdata zip offsets zip lengths } map { case ((d, o), l) => new VecString(d, o, l) }
    Frame(zipped : _*).row(params.skipLines -> *)
  }

  /**
   * Extract data from a CSV data source in parallel, producing data for creating a
   * Frame.
   *
   * For example,
   *
   * {{{
   *   val f = CsvFile("tmp.csv")
   *   val data = CsvParser.parsePar(CsvParser.parseInt)(f)
   *   ...
   *   data.toFrame
   * }}}
   *
   * @param cols The column offsets to parse (if empty, parse everything)
   * @param params The CsvParams to utilize in parsing
   */
  def parsePar(cols: Seq[Int] = List(), params: CsvParams = CsvParams())(src: CsvSourcePar): Frame[Int, Int, String] = {
    val xserv = Executors.newFixedThreadPool(nProcs)           // create thread pool for N CPU bound threads

    var results = IndexedSeq.empty[Vec[String]]

    try {
      // submit chunks to parse
      val parseTasks = for ((chunk, i) <- src.getChunks(nProcs).zipWithIndex) yield {
        xserv submit parseTask(chunk, cols, params.copy(hasHeader = i == 0 && params.hasHeader))
      }

      val chunks = parseTasks.map(_.get())                     // wait on, retrieve parse results

      if (chunks(0).numCols > 0) {
        val nRows = chunks.map(_.numRows).sum                  // total rows across all chunks

        // submit chunks for combination
        val combineTasks = for (i <- 0 until chunks(0).numCols) yield {
          xserv submit combineTask(chunks, i, nRows)
        }

        results = combineTasks.map(_.get())                    // wait on, retrieve combine results
      }
    }
    finally {
      xserv.shutdown()
    }

    Frame(results : _*).row(params.skipLines -> *)
  }

  private def extractFields(line: String, callback: (String, Int) => Unit,
                            locs: Array[Int], params: CsvParams) {

    val quote = params.quoteChar
    val sep = params.separChar
    val stripQuote = !params.withQuote

    var inQ = false                     // whether our scan is between quotes

    var curFld = 0                      // current field of parse
    var curBeg = 0                      // offset of start of current field in line
    var curEnd = 0                      // current character we're on in line
    var locIdx = 0                      // current location within locs array
    var inQoff = 0                      // offset if there is a quote character to strip

    val carr = line.toCharArray         // line as character array
    val slen = carr.length              // length of line

    while (curEnd < slen && locIdx < locs.length) {
      val chr = carr(curEnd)            // get current character

      if (chr == quote) {               // handle a quote
        if (stripQuote) {
          if (inQ)
            inQoff = 1                  // we're exiting a quoted field
          else
            curBeg = curEnd + 1         // we're starting a quoted field
        }
        inQ = !inQ
      }

      if (!inQ && chr == sep) {         // we're not in quoted field & we hit a separator
        if (curFld == locs(locIdx)) {   // we want this field
          callback(String.valueOf(carr, curBeg, curEnd - curBeg - inQoff), locIdx)
          locIdx += 1
        }
        inQoff = 0
        curBeg = curEnd + 1             // start a new field
        curFld += 1
      }

      curEnd += 1                       // move forward a character
    }

    // handle final field, may/not be terminated with separChar
    if (locIdx < locs.length && curFld == locs(locIdx) && curBeg < slen) {
      inQoff = if (carr(curEnd - 1) == quote && stripQuote) 1 else 0
      callback(String.valueOf(carr, curBeg, curEnd - curBeg - inQoff), locIdx)
      locIdx += 1
    }

    // if we didn't scan a field for all requested locations, throw an error
    if (locIdx < locs.length) {
      throw new ArrayIndexOutOfBoundsException(
        """Unable to read column %d in line:
          | ------------
          | %s
          | ------------""".stripMargin.format(locs(locIdx), line))
    }
  }

  private def extractAllFields(line: String, params: CsvParams): Array[String] = {
    val quote = params.quoteChar
    val sep = params.separChar
    val stripQuote = !params.withQuote

    val result = ArrayBuffer[String]()

    var inQ = false                     // whether our scan is between quotes

    var curFld = 0                      // current field of parse
    var curBeg = 0                      // offset of start of current field in line
    var curEnd = 0                      // current character we're on in line
    var inQoff = 0                      // offset if there is a quote character to strip

    val carr = line.toCharArray         // line as character array
    val slen = carr.length              // length of line

    while (curEnd < slen) {
      val chr = carr(curEnd)            // get current character

      if (chr == quote) {               // handle a quote
        if (stripQuote) {
          if (inQ)
            inQoff = 1                  // we're exiting a quoted field
          else
            curBeg = curEnd + 1         // we're starting a quoted field
        }
        inQ = !inQ
      }

      if (!inQ && chr == sep) {         // we're not in quoted field & we hit a separator
        result += String.valueOf(carr, curBeg, curEnd - curBeg - inQoff)
        inQoff = 0
        curBeg = curEnd + 1             // start a new field
        curFld += 1
      }

      curEnd += 1                       // move forward a character
    }

    // handle final field, may/not be terminated with separChar
    if (curBeg < slen) {
      inQoff = if (carr(curEnd - 1) == quote && stripQuote) 1 else 0
      result += String.valueOf(carr, curBeg, curEnd - curBeg - inQoff)
    }

    result.toArray
  }

  // a task that parses a chunk, returns some ParseData
  private def parseTask(chunk: CsvSource, cols: Seq[Int], params: CsvParams) =
    new Callable[Frame[Int, Int, String]] {
      val csvpar = params.copy(skipLines = 0)
      def call() = parse(cols, csvpar)(chunk)
    }

  // concatenates a single column within the ParseData chunks, returns joined Vec
  private def combineTask(chunks: IndexedSeq[Frame[Int, Int, String]], col: Int, sz: Int) =
    new Callable[Vec[String]] {
      def call() = {
        val vecs = for (i <- 0 until chunks.length) yield chunks(i).colAt(col).toVec
        VecString.concat(vecs)
      }
    }

  def parseInt(s: String) =
    try { java.lang.Integer.parseInt(s) } catch { case _ : NumberFormatException => Int.MinValue }

  def parseLong(s: String) =
    try { java.lang.Long.parseLong(s) } catch { case _ : NumberFormatException => Long.MinValue }

  def parseFloat(s: String) =
    try { java.lang.Float.parseFloat(s) } catch { case _ : NumberFormatException => Float.NaN }

  def parseDouble(s: String) =
    try { java.lang.Double.parseDouble(s) } catch { case _ : NumberFormatException => Double.NaN }
}