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

import java.io.{InputStreamReader, BufferedReader, RandomAccessFile}
import java.nio.channels.FileChannel
import it.unimi.dsi.io.ByteBufferInputStream

/**
 * CsvFile provides an implementation of a [[org.saddle.io.CsvSourcePar]] for
 * parsing a CSV file.
 *
 * For example,
 *
 * {{{
 *   val fs = CsvFile("tmp.csv")
 *   val data = CsvParser.parsePar(CsvParser.parseInt)(fs)
 *   ...
 *   data.toFrame
 * }}}
 *
 * @param path Path to file
 * @param encoding Encoding of text file
 */
class CsvFile(path: String, encoding: String = "UTF-8") extends CsvSourcePar {
  private val file = new RandomAccessFile(path, "r")
  private val chan = file.getChannel
  private val sz = chan.size()

  // split a file into at most N chunks to be parsed
  def getChunks(n: Int): IndexedSeq[CsvSource] = {
    var start = 0L
    var end = 0L
    val chunk = sz / n

    val chunks = for { i <- 0 until n if end < sz } yield {
      // align to a natural breakpoint
      end = alignToLineBreak(start + chunk)

      // create csv source over this chunk
      val source = makeChunk(start, end)

      // move forward a chunk
      start = end
      source
    }

    chunks
  }

  /**
   * Creates a new CsvSource from a chunk of a file between two byte offsets
   */
  private def makeChunk(start: Long, end: Long) = new CsvSource {
    val s = start   // start of chunk
    val e = end     // end of chunk
    var l = 0L      // location within chunk

    val stream = ByteBufferInputStream.map(chan, FileChannel.MapMode.READ_ONLY)
    val reader = new BufferedReader(new InputStreamReader(stream, encoding))

    def reset() {
      stream.position(s)
      l = s
    }

    def readLine = if (l >= e) null else {
      val str = reader.readLine()
      if (str != null) l += str.getBytes.length + 1
      str
    }
  }

  /**
   * Moves offset to byte just following a line break
   */
  private def alignToLineBreak(offset: Long): Long = {
    val stream = ByteBufferInputStream.map(chan, FileChannel.MapMode.READ_ONLY)
    val reader = new BufferedReader(new InputStreamReader(stream, encoding))

    val byte = math.min(offset, chan.size())
    stream.position(byte)

    val readLn = reader.readLine()
    if (readLn != null) offset + readLn.getBytes.length + 1 else byte
  }

  override def toString = "CsvFile(%s, encoding: %s)".format(path, encoding)
}

object CsvFile {
  def apply(path: String, encoding: String = "UTF-8") = new CsvFile(path, encoding)

  implicit def convertToCsvSource(f: CsvFile): CsvSource = f.makeChunk(0, f.chan.size())
}