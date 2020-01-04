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

import org.saddle.{Frame, Vec, ST}
import org.saddle.Index
import collection.mutable.ArrayBuffer
import scala.annotation.tailrec
import scala.io.Source
import org.saddle.Buffer
import scala.{specialized => spec}
import java.nio.CharBuffer
import java.io.File
import java.nio.charset.Charset

/**
  * Csv parsing utilities
  */
object CsvParser {

  def readFile(
      file: File,
      bufferSize: Int = 8192,
      charset: Charset = Charset.forName("US-ASCII")
  ): Iterator[CharBuffer] = {
    val buffer = java.nio.ByteBuffer.allocate(bufferSize)
    val is = new java.io.FileInputStream(file)
    val channel = is.getChannel
    var eof = false
    def fillBuffer() = {
      buffer.clear()
      var count = channel.read(buffer)
      while (count >= 0 && buffer.remaining > 0) {
        count = channel.read(buffer)
      }
      if (count < 0) {
        eof = true
      }
      buffer.flip
    }
    new Iterator[CharBuffer] {
      def hasNext = !eof
      def next = {
        fillBuffer()
        charset.decode(buffer)
      }
    }
  }

  private class DataBuffer(
      data: Iterator[CharBuffer],
      var buffer: CharBuffer,
      var position: Int,
      var save: Boolean
  ) {
    def concat(buffer1: CharBuffer, buffer2: CharBuffer) = {
      val b = CharBuffer.allocate(buffer1.remaining + buffer.remaining)
      b.put(buffer1)
      b.put(buffer2)
      b.flip
      b
    }
    @tailrec
    private def fillBuffer: Boolean = {
      if (!data.hasNext) false
      else {
        if (!save) {
          buffer = data.next
          position = 0
        } else {
          buffer = concat(buffer, data.next)
        }
        if (buffer.length > position) true
        else fillBuffer
      }
    }
    @inline final def hasNext = position < buffer.length || fillBuffer

    @inline final def next =
      if (position < buffer.length) {
        val c = buffer.get(position)
        position += 1
        c
      } else {
        fillBuffer
        val c = buffer.get(position)
        position += 1
        c
      }
  }

  def parseFile[@spec(Int, Double, Long, Float) T](
      file: File,
      cols: Seq[Int] = Nil,
      fieldSeparator: Char = ',',
      quoteChar: Char = '"',
      recordSeparator: String = "\r\n",
      bufferSize: Int = 8192,
      maxLines: Long = Long.MaxValue
  )(implicit st: ST[T]): Either[String, Frame[Int, Int, T]] =
    parseFromIterator(
      readFile(file, bufferSize),
      cols,
      fieldSeparator,
      quoteChar,
      recordSeparator,
      maxLines
    ).map(_._1)

  def parseFileWithHeader[@spec(Int, Double, Long, Float) T](
      file: File,
      cols: Seq[Int] = Nil,
      fieldSeparator: Char = ',',
      quoteChar: Char = '"',
      recordSeparator: String = "\r\n",
      bufferSize: Int = 8192,
      maxLines: Long = Long.MaxValue
  )(implicit st: ST[T]): Either[String, Frame[Int, String, T]] =
    parseFromIterator(
      readFile(file, bufferSize),
      cols,
      fieldSeparator,
      quoteChar,
      recordSeparator,
      maxLines,
      header = true
    ).map { case (frame, colIndex) => frame.setColIndex(colIndex.get) }

  def parseSource[@spec(Int, Double, Long, Float) T](
      source: Source,
      cols: Seq[Int] = Nil,
      fieldSeparator: Char = ',',
      quoteChar: Char = '"',
      recordSeparator: String = "\r\n",
      bufferSize: Int = 8192,
      maxLines: Long = Long.MaxValue
  )(implicit st: ST[T]): Either[String, Frame[Int, Int, T]] =
    parseFromIterator(
      source.grouped(bufferSize).map(v => CharBuffer.wrap(v.toArray)),
      cols,
      fieldSeparator,
      quoteChar,
      recordSeparator,
      maxLines
    ).map(_._1)

  def parseSourceWithHeader[@spec(Int, Double, Long, Float) T](
      source: Source,
      cols: Seq[Int] = Nil,
      fieldSeparator: Char = ',',
      quoteChar: Char = '"',
      recordSeparator: String = "\r\n",
      bufferSize: Int = 8192,
      maxLines: Long = Long.MaxValue
  )(implicit st: ST[T]): Either[String, Frame[Int, String, T]] =
    parseFromIterator(
      source.grouped(bufferSize).map(v => CharBuffer.wrap(v.toArray)),
      cols,
      fieldSeparator,
      quoteChar,
      recordSeparator,
      maxLines,
      header = true
    ).map { case (frame, colIndex) => frame.setColIndex(colIndex.get) }

  /**
    * Parse CSV files according to RFC 4180
    *
    * @param cols The column offsets to parse (if empty, parse everything)
    * @param fieldSeparator The separator; default is comma
    * @param quoteChar Within matching quotes, treat separChar as normal char;
    *                  default is double-quote
    * @param recordSeparator Record separator (line ending)
    * @param source The csv data source to operate on
    * @param maxLines The maximum number of records that will be read from the file. Includes header.
    * @param header indicates whether the first line should be set aside
    */
  def parseFromIterator[@spec(Int, Double, Long, Float) T](
      source: Iterator[CharBuffer],
      cols: Seq[Int] = Nil,
      fieldSeparator: Char = ',',
      quoteChar: Char = '"',
      recordSeparator: String = "\r\n",
      maxLines: Long = Long.MaxValue,
      header: Boolean = false
  )(
      implicit st: ST[T]
  ): Either[String, (Frame[Int, Int, T], Option[Index[String]])] =
    if (fieldSeparator == quoteChar)
      Left("Separator character and quote character cannot be the same")
    else if (recordSeparator.size != 1 && recordSeparator.size != 2)
      Left(
        s"Record separator must have 1 or 2 characters. ${recordSeparator.toCharArray.map(_.toByte).deep}"
      )
    else if (source.isEmpty || maxLines == 0)
      Right((Frame.empty[Int, Int, T], None))
    else {

      val data = new DataBuffer(source, CharBuffer.allocate(0), 0, false)

      // sorted, unique column locations to parse
      var locs = Set(cols: _*).toArray[Int].sorted

      // parse first line
      val (firstLine, errorMessage) = {
        val buffer = new ArrayBuffer[String](1024)
        val callback = (s: String, _: Int) => {
          buffer.append(s)
        }
        val errorMessage = extractFields(
          data,
          callback,
          Array.empty,
          quoteChar,
          fieldSeparator,
          recordSeparator.toCharArray,
          1
        )
        (buffer.toArray, errorMessage)
      }

      if (errorMessage.nonEmpty) Left(errorMessage)
      else {
        // what column locations to extract
        if (locs.length == 0) locs = (0 until firstLine.length).toArray

        // set up buffers to store parsed data
        val bufdata = for { _ <- locs } yield new Buffer[T](
          Array.ofDim[T](1024),
          0
        )

        def addToBuffer(s: String, buf: Int) = {
          import scala.Predef.{wrapRefArray => _}
          bufdata(buf).+=(st.parse(s))
        }

        val fields = Vec(firstLine).take(locs)
        val colIndex = if (header) {
          Some(Index(fields))
        } else {
          fields.toSeq.zipWithIndex.map { case (s, i) => addToBuffer(s, i) }
          None
        }

        // parse remaining rows
        val errorMessage = extractFields(
          data,
          addToBuffer,
          locs,
          quoteChar,
          fieldSeparator,
          recordSeparator.toCharArray,
          maxLines - 1
        )

        if (errorMessage.nonEmpty) Left(errorMessage)
        else {
          val columns = bufdata map { b =>
            Vec(b.toArray)
          }

          Right((Frame(columns: _*), colIndex))
        }
      }
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
    var curBegin = 0 // offset of start of current field in buffer
    var locIdx = 0 // current location within locs array
    var lineIdx = 0L
    var error = false
    var errorMessage = ""

    val CR = recordSeparator.head
    val LF =
      if (recordSeparator.size == 2) recordSeparator.last
      else recordSeparator.head
    val singleRecordSeparator = recordSeparator.size == 1

    val allFields = locs.isEmpty

    def fail(str: String) = {
      error = true
      errorMessage = str + s".. line=$lineIdx, field=$curField"
    }

    def emit(offset: Int) = {
      if (allFields || (locs.size > locIdx && curField == locs(locIdx))) {
        callback(
          data.buffer.subSequence(curBegin, data.position - offset).toString,
          locIdx
        )
        locIdx += 1
      }
    }

    def close() = {
      curField += 1
      data.save = false
    }
    def open(offset: Int) = {
      curBegin = data.position - offset
      data.save = true
    }

    def newline() = {
      if (locIdx < locs.size) {
        fail(
          s"Incomplete line $lineIdx. Expected ${locs.size} fields, got $locIdx"
        )
      }
      lineIdx += 1
      locIdx = 0
      curField = 0
    }

    while (data.hasNext && lineIdx < maxLines && !error) {
      val chr = data.next
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
        } else if (chr == quoteChar) {
          fail("quote must not occur in unquoted field")
        } else if (chr == CR) {
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
        } else {
          fail(
            "quotes in quoted field must be escaped by doubling them"
          )
        }
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
          fail("invalid quote")
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
          fail("invalid quote")
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
          fail("Closing quote followed by data")
        }
      }
    }

    if (state == 1) {
      emit(0)
    } else if (state == 3) {
      emit(1)
    } else if (state == 2) {
      fail("Unclosed quote")
    }

    errorMessage

  }

}
