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

package org.saddle

import scala.{ specialized => spec }

/**
 * Buffer provides a mutable data structure specialized on several primitive types.
 * Appending an element takes amortized constant time, and the buffer instance can
 * be converted to an array either implicitly or explicitly via `toArray`.
 */
trait Buffer[@spec(Boolean, Int, Long, Double) T] {
  /**
   * Access an element of the buffer
   * @param loc Offset to access
   */
  def apply(loc: Int): T

  /**
   * Add an element to the buffer
   * @param key value to add
   */
  def add(key: T)

  /**
   * Return number of elements in buffer
   */
  def count(): Int

  /**
   * Convert buffer to an array of the same type
   */
  def toArray: Array[T]

  override def toString = "Buffer [" + util.buildStr(8, count(), (i: Int) => " " + apply(i).toString, " ... ") + " ]"
}

object Buffer {
  val INIT_CAPACITY = 16

  /**
   * Construct a new buffer with size pre-allocation (default size 16)
   *
   * @param sz Initial size of buffer
   * @tparam C Type of elements to be stored in buffer
   */
  def apply[C](sz: Int = INIT_CAPACITY)(implicit st: ST[C]): Buffer[C] = st.makeBuf(sz)

  /**
   * Convert buffer to array implicitly
   * @param buf Buffer to convert
   * @tparam T Type of elements in buffer
   */
  implicit def bufToArr[T](buf: Buffer[T]): Array[T] = buf.toArray
}

