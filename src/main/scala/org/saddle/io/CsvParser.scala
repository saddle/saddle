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
   * @param convert A callback for token conversion; convert from String => T
   * @param cols The column offsets to parse (if empty, parse everything)
   * @param params The CsvParams to utilize in parsing
   * @param source The csv data source to operate on
   * @tparam T The result type of the parsing function
   */
  def parse[T: CLM](convert: String => T, cols: Seq[Int] = List(),
                    params: CsvParams = CsvParams())(source: CsvSource): ParsedData[T] = {

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

    // set up buffers to store parsed column data
    val buffers = { for (c <- locs) yield Buffer[T](1024) }.toIndexedSeq

    def addToBuffer(i: Int, s: String) {
      buffers(i).add(convert(s))
    }

    // first line is either header, or needs to be processed
    val fields = Vec(firstLine).take(locs)
    val headers = if (params.hasHeader) fields else {
      fields.toSeq.zipWithIndex.map { case (s, i) => addToBuffer(i, s) }
      Vec.empty[String]
    }

    // parse remaining rows
    var str: String = null
    while ( { str = source.readLine; str != null } ) {
      extractFields(str, addToBuffer, locs, params)
    }

    val vecs = buffers.map(b => Vec(b.toArray))

    ParsedData(headers, vecs)
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
   * @param convert A callback for token conversion; convert from String => T
   * @param cols The column offsets to parse (if empty, parse everything)
   * @param params The CsvParams to utilize in parsing
   * @tparam T The result type of the parsing function
   */
  def parsePar[T: CLM](convert: String => T, cols: Seq[Int] = List(),
                       params: CsvParams = CsvParams())(src: CsvSourcePar): ParsedData[T] = {
    val xserv = Executors.newFixedThreadPool(nProcs)           // create thread pool for N CPU bound threads

    var results = Seq.empty[Vec[T]]
    var header = Vec.empty[String]

    try {
      // submit chunks to parse
      val parseTasks = for ((chunk, i) <- src.getChunks(nProcs).zipWithIndex) yield {
        xserv submit parseTask(convert, chunk, cols, params.copy(hasHeader = i == 0 && params.hasHeader))
      }

      val chunks = parseTasks.map(_.get())                     // wait on, retrieve parse results

      header = chunks(0).headers

      if (chunks(0).columns.length > 0) {
        val nRows = chunks.map(_.columns(0).length).sum        // total rows across all chunks
        val nCols = header.length                              // number of parsed out columns

        // submit chunks for combination
        val combineTasks = for (i <- 0 until nCols) yield {
          xserv submit combineTask(chunks, i, nRows)
        }

        results = combineTasks.map(_.get())                    // wait on, retrieve combine results
      }
    }
    finally {
      xserv.shutdown()
    }

    ParsedData(header, results.map(v => v.slice(params.skipLines, v.length)))
  }

  private def extractFields(line: String, callback: (Int, String) => Unit,
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
          callback(locIdx, String.valueOf(carr, curBeg, curEnd - curBeg - inQoff))
          locIdx += 1
          inQoff = 0
        }
        curBeg = curEnd + 1             // start a new field
        curFld += 1
      }

      curEnd += 1                       // move forward a character
    }

    // handle final field, may/not be terminated with separChar
    if (locIdx < locs.length && curFld == locs(locIdx) && curBeg < slen) {
      inQoff = if (carr(curEnd - 1) == quote && stripQuote) 1 else 0
      callback(locIdx, String.valueOf(carr, curBeg, curEnd - curBeg - inQoff))
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
  private def parseTask[T: CLM](convert: String => T, chunk: CsvSource, cols: Seq[Int], params: CsvParams) =
    new Callable[ParsedData[T]] {
      val csvpar = params.copy(skipLines = 0)
      def call() = parse[T](convert, cols, csvpar)(chunk)
    }

  // concatenates a single column within the ParseData chunks, returns joined Vec
  private def combineTask[T: CLM](chunks: IndexedSeq[ParsedData[T]], col: Int, sz: Int) =
    new Callable[Vec[T]] {
      def call() = {
        val arr = Array.ofDim[T](sz)
        var c = 0
        for (i <- 0 until chunks.length) {
          val src = chunks(i).columns(col)
          val sz = src.length
          var j = 0
          while (j < sz) {
            arr(c) = src(j)
            j += 1
            c += 1
          }
        }
        Vec(arr)
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