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

import org.saddle.{Frame, Vec}
import collection.mutable.ArrayBuffer
import scala.annotation.tailrec
import scala.io.Source
import org.saddle.Buffer

/**
  * Csv parsing utilities
  */
object CsvParser {

  private class DataBuffer(
      data: Iterator[Array[Char]],
      var buffer: Array[Char],
      var position: Int,
      var save: Boolean
  ) {
    @tailrec
    private def fillBuffer: Boolean = {
      if (!data.hasNext) false
      else {
        if (!save) {
          buffer = data.next
          position = 0
        } else {
          buffer = buffer ++ data.next
        }
        if (buffer.length > position) true
        else fillBuffer
      }
    }
    @inline def hasNext = position < buffer.length || fillBuffer

    @inline def next =
      if (position < buffer.length) {
        val c = buffer(position)
        position += 1
        c
      } else {
        fillBuffer
        val c = buffer(position)
        position += 1
        c
      }
  }

  def parse(
      source: Source,
      cols: Seq[Int] = Nil,
      fieldSeparator: Char = ',',
      quoteChar: Char = '"',
      recordSeparator: String = "\r\n",
      bufferSize: Int = 8192
  ): Frame[Int, Int, String] =
    parseFromIterator(
      source.grouped(bufferSize).map(_.toArray),
      cols,
      fieldSeparator,
      quoteChar,
      recordSeparator
    )

  /**
    * Parse CSV files according to RFC 4180
    *
    * @param cols The column offsets to parse (if empty, parse everything)
    * @param separChar The separator; default is comma
    * @param quoteChar Within matching quotes, treat separChar as normal char;
    *                  default is double-quote
    * @param withQuote If true, do not strip quote character from quoted fields
    * @param source The csv data source to operate on
    */
  def parseFromIterator(
      source: Iterator[Array[Char]],
      cols: Seq[Int] = Nil,
      fieldSeparator: Char = ',',
      quoteChar: Char = '"',
      recordSeparator: String = "\r\n"
  ): Frame[Int, Int, String] = {

    require(
      fieldSeparator != quoteChar,
      "Separator character and quote character cannot be the same"
    )

    require(
      recordSeparator.size == 1 || recordSeparator.size == 2,
      s"Record separator must have 1 or 2 characters. ${recordSeparator.toCharArray.map(_.toByte).deep}"
    )

    if (source.isEmpty) {
      sys.error("No data to parse")
    }

    val data = new DataBuffer(source, Array.empty, 0, false)

    // sorted, unique column locations to parse
    var locs = Set(cols: _*).toArray[Int].sorted

    // parse first line
    val firstLine = {
      val buffer = new ArrayBuffer[String](1024)
      val callback = (s: String, _: Int) => {
        buffer.append(s)
      }
      extractFields(
        data,
        callback,
        Array.empty,
        quoteChar,
        fieldSeparator,
        recordSeparator.toCharArray,
        1
      )
      buffer.toArray
    }
    // what column locations to extract
    if (locs.length == 0) locs = (0 until firstLine.length).toArray

    // set up buffers to store parsed data
    val bufdata = for { _ <- locs } yield new Buffer[String](
      Array.ofDim[String](1024),
      0
    )

    def addToBuffer(s: String, buf: Int) = {
      import scala.Predef.{wrapRefArray => _}
      bufdata(buf).+=(s)
    }

    // first line is either header, or needs to be processed
    val fields = Vec(firstLine).take(locs)
    fields.toSeq.zipWithIndex.map { case (s, i) => addToBuffer(s, i) }

    // parse remaining rows
    extractFields(
      data,
      addToBuffer,
      locs,
      quoteChar,
      fieldSeparator,
      recordSeparator.toCharArray,
      Long.MaxValue
    )

    val columns = bufdata map { b =>
      Vec(b.toArray)
    }

    Frame(columns: _*)
  }

  private def extractFields(
      data: DataBuffer,
      callback: (String, Int) => Unit,
      locs: Array[Int],
      quoteChar: Char,
      separChar: Char,
      recordSeparator: Array[Char],
      maxLines: Long
  ) = {

    // 0 - init
    // 1 - non-escaped data
    // 2 - quoted data
    // 3 - quote in quoted data
    // 4 - CR in init
    // 5 - CR in data
    // 6 - CR in quote
    var state = 0

    var curField = 0 // current field of parse
    var curBegin = 0 // offset of start of current field in line
    var locIdx = 0 // current location within locs array
    var lineIdx = 0L
    val CR = recordSeparator.head
    val LF =
      if (recordSeparator.size == 2) recordSeparator.last
      else recordSeparator.head
    val singleRecordSeparator = recordSeparator.size == 1

    val allFields = locs.isEmpty

    @inline def emit(offset: Int) = {
      if (allFields || (locs.size > locIdx && curField == locs(locIdx))) {
        callback(
          String
            .valueOf(data.buffer, curBegin, data.position - offset - curBegin),
          locIdx
        )
        locIdx += 1
      }
    }

    @inline def close() = {
      curField += 1
      data.save = false
    }
    @inline def open(offset: Int) = {
      curBegin = data.position - offset
      data.save = true
    }

    @inline def newline() = {
      if (locIdx < locs.size) {
        throw new RuntimeException(
          s"Incomplete line $lineIdx. Expected ${locs.size} fields, got $locIdx"
        )
      }
      lineIdx += 1
      locIdx = 0
      curField = 0
    }

    while (data.hasNext && lineIdx < maxLines) {
      val chr = data.next
      // println(s"$chr $state ${data.save}")
      if (state == 0) { // init
        if (chr == separChar) {
          emit(1)
          close()
        } else if (chr == quoteChar) {
          state = 2
          open(0)
        } else if (chr == CR) {
          if (singleRecordSeparator) {
            emit(1)
            close()
            newline()
          } else {
            state = 4
            open(1)
          }
        } else {
          state = 1
          open(1)
        }
      } else if (state == 1) { // data
        if (chr == separChar) {
          emit(1)
          close()
          state = 0
        } else if (chr == quoteChar)
          throw new RuntimeException("quote must not occur in unquoted fiedl")
        else if (chr == CR) {
          if (singleRecordSeparator) {
            emit(1)
            close()
            state = 0
            newline()
          } else {
            state = 5
          }
        }
      } else if (state == 2) { //quoted data
        if (chr == quoteChar) {
          state = 3
        }
      } else if (state == 3) { // quote in quoted data
        if (chr == quoteChar) {
          state = 2
        } else if (chr == separChar) {
          emit(2)
          close()
          state = 0
        } else if (chr == CR) {
          if (singleRecordSeparator) {
            emit(2)
            close()
            state = 0
            newline()
          } else {
            state = 6
          }
        } else
          throw new RuntimeException(
            "quotes in quoted field must be escaped by doubling them"
          )
      } else if (state == 4) { // CR in init
        if (chr == LF) {
          emit(2)
          close()
          state = 0
          newline()
        } else if (chr == separChar) {
          callback(s"$CR", locIdx)
          locIdx += 1
          curField += 1
          state = 0
        } else if (chr == quoteChar) {
          throw new RuntimeException("invalid quote")
        } else if (chr == CR) {
          state = 5
        } else {
          state = 1
        }
      } else if (state == 5) { // CR in data
        if (chr == LF) {
          emit(2)
          close()
          state = 0
          newline()
        } else if (chr == separChar) {
          emit(1)
          close()
          state = 0
        } else if (chr == quoteChar) {
          throw new RuntimeException("invalid quote")
        } else if (chr == CR) {
          state = 5
        } else {
          state = 1
        }
      } else if (state == 6) { // CR in quote
        if (chr == LF) {
          emit(3)
          close()
          state = 0
          newline()
        } else {
          throw new RuntimeException("Closing quote followed by data")
        }
      }
    }

    if (state == 1) {
      emit(0)
    } else if (state == 3) {
      emit(1)
    } else if (state == 2) {
      throw new RuntimeException("Unclosed quote")
    }

  }

}
