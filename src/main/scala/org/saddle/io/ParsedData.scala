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

/**
 * Container to hold data that comes back from parsing a csv
 *
 * @param columns A sequence of Vec instances containing data
 */
case class ParsedData(headers: Vec[String], columns: IndexedSeq[Vec[String]]) {
  def toFrame: Frame[Int, String, String] = Frame(columns : _*).setColIndex(Index(headers))
  def toFrameNoHeader: Frame[Int, Int, String] = Frame(columns : _*)
}

/**
 * Allows implicit conversion from ParseData to Frame
 */
object ParsedData {
  implicit def tf1(fd: ParsedData): Frame[Int, String, String] = fd.toFrame
  implicit def tf2(fd: ParsedData): Frame[Int, Int, String] = fd.toFrameNoHeader
}
