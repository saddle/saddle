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

/**
 * Additional utilities that need a home
 */
package object util {
  /**
   * Creates a string out of ''count'' number of elements extracted from ''total'' elements
   * between offsets [0 ... count / 2) and (total - count / 2 ... total), using a callback
   * that generates a string at each offset, and inserting a break string if count > total.
   * @param count Number of elements to print
   * @param total Total number of elements in sequence
   * @param callback Generates a string at each offset
   * @param break Produces a string to insert as a break
   */
  def buildStr(count: Int, total: Int, callback: Int => String, break: => String = " ... "): String = {
    var i = 0
    val buf = new StringBuilder()
    if (total <= count) {
      while (i < total) {
        buf.append(callback(i))
        i += 1
      }
    }
    else {
      while (i < count / 2) {
        buf.append(callback(i))
        i += 1
      }
      buf append break
      i = total - count / 2
      while (i < total) {
        buf.append(callback(i))
        i += 1
      }
    }
    buf.toString()
  }

  /**
   * Takes n elements from the front and from the back of array
   * @param arr Array
   * @param n Number of elements to take
   */
  def grab[T](arr: Seq[T], n: Int): Seq[T] = arr.take(n) ++ arr.takeRight(n)
}
