package org.saddle.io

import java.io.BufferedReader
import java.io.InputStreamReader
import java.net.URL

import org.saddle.UTF8

/**
 * CsvUrl provides an implementation of a [[org.saddle.io.CsvSource]] for
 * parsing CSV data consumed from a URL.
 *
 * For example,
 *
 * {{{
 *   val fs = CsvURL("http://ichart.finance.yahoo.com/table.csv?s=AAPL&a=2&b=1&c=2015")
 *   val data = CsvParser.parse(fs).withRowIndex(0).withColIndex(0).col("Adj Close")
 * //> data  : org.saddle.Frame[String,String,String] = [296 x 1]
 * //|                Adj Close
 * //|               ----------
 * //| 2016-05-02 ->  93.639999
 * //| 2016-04-29 ->  93.739998
 * //| 2016-04-28 ->  94.830002
 * //| 2016-04-27 ->      97.82
 * //| 2016-04-26 -> 104.349998
 * //| ...
 * }}}
 *
 * @param url URL String of the CSV data source
 * @param encoding Encoding of CSV data source
 */

class CsvUrl(url: String, encoding: String = UTF8) extends CsvSource {
  private val urlHandle = new URL(url);
  private val reader = new BufferedReader(new InputStreamReader(urlHandle.openStream()));

  def readLine = {
    val line = reader.readLine()
    if (line == null) reader.close()
    line
  }

  override def toString = "CsvUrl(%s, encoding: %s)".format(url, encoding)
}

object CsvUrl {
  def apply(url: String, encoding: String = UTF8) = new CsvUrl(url, encoding)
}