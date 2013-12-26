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
import java.util.concurrent.Callable
import it.unimi.dsi.fastutil.objects.ObjectLinkedOpenHashSet

/**
 * Holds parameters to customize CSV parsing
 *
 * @param separChar The separator; default is comma
 * @param quoteChar Within matching quotes, treat separChar as normal char;
 *                  default is double-quote
 * @param withQuote If true, do not strip quote character from quoted fields
 * @param skipLines Whether to skip some integer number of lines, default 0
 */
case class CsvParams(separChar: Char    = ',',
                     quoteChar: Char    = '"',
                     withQuote: Boolean = false,
                     skipLines: Int     = 0)

/**
 * Csv parsing utilities
 */
object CsvParser {
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
    val bufdata = for { c <- locs } yield Buffer[String](1024)

    // this seriously helps reduce memory footprint w/o major perf. impact
    val interner = new ObjectLinkedOpenHashSet[String]()
    def addToBuffer(s: String, buf: Int) {
      if (!interner.contains(s)) {
        interner.add(s)
        bufdata(buf).add(s)
      }
      else
        bufdata(buf).add(interner.get(s))
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

    val columns = bufdata map { b => Vec(b.toArray) }

    Frame(columns : _*).row(params.skipLines -> *)
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

    // handle a final field which may/not be terminated with separChar
    if (locIdx < locs.length && curFld == locs(locIdx) && curBeg < slen) {
      inQoff = if (carr(curEnd - 1) == quote && stripQuote) 1 else 0
      callback(String.valueOf(carr, curBeg, curEnd - curBeg - inQoff), locIdx)
      locIdx += 1
    }

    // handle a missing value following final separator
    if (locIdx == locs.length - 1 && curBeg == slen) {
      callback("", locIdx)
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

  def parseInt(s: String) =
    try { java.lang.Integer.parseInt(s) } catch { case _ : NumberFormatException => Int.MinValue }

  def parseLong(s: String) =
    try { java.lang.Long.parseLong(s) } catch { case _ : NumberFormatException => Long.MinValue }

  def parseFloat(s: String) =
    try { java.lang.Float.parseFloat(s) } catch { case _ : NumberFormatException => Float.NaN }

  def parseDouble(s: String) =
    try { java.lang.Double.parseDouble(s) } catch { case _ : NumberFormatException => Double.NaN }
}