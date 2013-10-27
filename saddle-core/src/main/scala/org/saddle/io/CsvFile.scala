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
import org.saddle.UTF8

/**
 * CsvFile provides an implementation of a [[org.saddle.io.CsvSource]] for
 * parsing a CSV file.
 *
 * For example,
 *
 * {{{
 *   val fs = CsvFile("tmp.csv")
 *   val data = CsvParser.parse(CsvParser.parseInt)(fs)
 *   ...
 *   data.toFrame
 * }}}
 *
 * @param path Path to file
 * @param encoding Encoding of text file
 */
class CsvFile(path: String, encoding: String = UTF8) extends CsvSource {
  private val file = new RandomAccessFile(path, "r")
  private val chan = file.getChannel

  private val stream = ByteBufferInputStream.map(chan, FileChannel.MapMode.READ_ONLY)
  private val reader = new BufferedReader(new InputStreamReader(stream, encoding))

  def readLine = {
    val line = reader.readLine()
    if (line == null) file.close()
    line
  }

  override def toString = "CsvFile(%s, encoding: %s)".format(path, encoding)
}

object CsvFile {
  def apply(path: String, encoding: String = UTF8) = new CsvFile(path, encoding)
}