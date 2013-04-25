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

package org.saddle.buffer

import org.saddle._
import org.saddle.Buffer

/**
 * Buffer instance for Any type
 */
class BufferAny[T: ST](sz: Int = 16) extends Buffer[T] {
  var list = Array.ofDim[T](sz)
  var count = 0
  var remain = sz

  def apply(loc: Int) = list(loc)

  def add(i: T) {
    if (remain == 0) {
      remain = list.length
      val newList = Array.ofDim[T](remain * 2)
      Array.copy(list, 0, newList, 0, list.length)
      list = newList
    }

    list(count) = i
    count += 1
    remain -= 1
  }

  def toArray: Array[T] = {
    val newList = Array.ofDim[T](count)
    Array.copy(list, 0, newList, 0, count)
    newList
  }
}

object BufferAny {
  def apply[T: ST](sz: Int) = new BufferAny[T](sz)
  def apply[T: ST]() = new BufferAny[T]()
}