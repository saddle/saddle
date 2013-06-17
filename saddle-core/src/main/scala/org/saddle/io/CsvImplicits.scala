/*
 * Copyright 2012, 2013 Novus Partners, Inc., & Adam D Klein
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
 */

package org.saddle.io

import java.io.{OutputStream, BufferedOutputStream, FileOutputStream}

import org.saddle._
import scalar.ScalarTag

/**
 * Settings for writing a CsvFile
 *
 * @param separChar Separator; default is comma
 * @param useQuote If true, fields containing separChar will be wrapped in quotes
 * @param quoteChar Quote character; default double-quote
 */
case class CsvSettings(separChar: Char = ',',
                       quoteChar: Char = '"',
                       useQuote: Boolean = true,
                       encoding: String = UTF8)

/**
 * Provides implicit functionality for writing data to CSV file format. Usage:
 *
 * {{{
 *   import CsvImplicits._
 *
 *   val f = Frame(...)
 *   f.writeCsvFile("tmp.csv")
 * }}}
 */
object CsvImplicits {
  /**
   * Provides enrichment on Series object for writing to a Csv file.
   */
  implicit def series2CsvWriter[X: ST: ORD, T: ST](series: Series[X, T]) = new {
    def writeCsvFile(path: String,
                     withColIx: Boolean = false,
                     withRowIx: Boolean = true,
                     settings: CsvSettings = new CsvSettings()) {
      frame2CsvWriter(Frame(series)).writeCsvFile(path, withColIx, withRowIx, settings)
    }

    def writeCsvStream(stream: OutputStream,
                       withColIx: Boolean = false,
                       withRowIx: Boolean = true,
                       settings: CsvSettings = new CsvSettings()) {
      frame2CsvWriter(Frame(series)).writeCsvStream(stream, withColIx, withRowIx, settings)
    }
  }     // end new

  /**
   * Provides enrichment on Frame object for writing to a Csv file.
   */
  implicit def frame2CsvWriter[RX: ST: ORD, CX: ST: ORD, T: ST](frame: Frame[RX, CX, T]) = new {
    /**
     * Write a frame in CSV format to a file at the path provided
     *
     * @param path File to write
     * @param withColIx If true, print out headers as first row
     * @param withRowIx If true, print out index value as first column
     * @param settings Settings to use in formatting
     */
    def writeCsvFile(path: String,
                     withColIx: Boolean = true,
                     withRowIx: Boolean = true,
                     settings: CsvSettings = new CsvSettings()) {

      val file = new FileOutputStream(path)
      val stream = new BufferedOutputStream(file, 4 * 1024)

      try {
        writeCsvStream(stream, withColIx, withRowIx, settings)
      }
      finally {
        stream.close()
        file.close()
      }
    }

    /**
     * Write a frame in CSV format to the stream provided
     *
     * @param stream Stream to write on
     * @param withColIx If true, print out headers as first row
     * @param withRowIx If true, print out index value as first column
     * @param settings Settings to use in formatting
     */
    def writeCsvStream(stream: OutputStream,
                       withColIx: Boolean = true,
                       withRowIx: Boolean = true,
                       settings: CsvSettings = new CsvSettings()) {

      val newLine = "\n".getBytes(settings.encoding)

      val separ = settings.separChar.toString
      val quote = settings.quoteChar.toString

      def quotify(seq: Seq[String]): Seq[String] = {
        if (settings.useQuote)
          seq.map { s => if (s.contains(separ)) "%s%s%s".format(quote, s, quote) else s }
        else
          seq
      }

      def writeHeader(rsm: ScalarTag[RX], csm: ScalarTag[CX]) {
        // get depth (number of levels) of row and column indexes
        val cDepth = csm.strList(frame.colIx.raw(0)).length
        val rDepth = rsm.strList(frame.rowIx.raw(0)).length

        // space holder according to number of row index levels
        val lead = Seq.fill(rDepth)("")

        // write out column index headers
        if (withColIx) {
          val colIxSeq = frame.colIx.toSeq

          // for each depth of the column index, write a row
          for (i <- 0 until cDepth) {
            stream write {
              val seq = if (!withRowIx)
                colIxSeq.map(csm.strList(_)(i))
              else
                lead ++: frame.colIx.toSeq.map(csm.strList(_)(i))

              quotify(seq).mkString(separ).getBytes(settings.encoding)
            }

            stream.write(newLine)
          }
        }
      }

      def writeRows(rsm: ScalarTag[RX]) {
        // now write each row of the frame
        frame.toRowSeq.foreach { case (ridx, row) =>
          stream write {
            val seq = if (!withRowIx)
              row.values.toSeq.map(_.toString)
            else
              (rsm.strList(ridx) ++: row.values.toSeq.map(_.toString))

            quotify(seq).mkString(separ).getBytes(settings.encoding)
          }

          stream.write(newLine)
        }
      }

      if (!frame.isEmpty) {
        val rsm = frame.rowIx.scalarTag
        val csm = frame.colIx.scalarTag

        writeHeader(rsm, csm)
        writeRows(rsm)
      }
    }

  }   // end new

}
