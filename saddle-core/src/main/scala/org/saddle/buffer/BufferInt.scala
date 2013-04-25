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

import it.unimi.dsi.fastutil.ints.IntArrays
import org.saddle.Buffer

class BufferInt(sz: Int = Buffer.INIT_CAPACITY) extends Buffer[Int] {
  var list = Array.ofDim[Int](sz)
  var count = 0
  var remain = sz

  def apply(loc: Int) = list(loc)

  def add(i: Int) {
    if (remain == 0) {
      remain = list.length
      list = IntArrays.grow(list, remain * 2)
    }

    list(count) = i
    count += 1
    remain -= 1
  }

  def toArray: Array[Int] = IntArrays.copy(list, 0, count)
}

object BufferInt {
  def apply(sz: Int) = new BufferInt(sz)
  def apply() = new BufferInt()
}