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

package org.saddle.vec

import org.saddle.array

private[saddle] object VecBool {
  // indirect counting sort of boolean vector
  def argSort(arr: Array[Boolean]): Array[Int] = {
    val newArr = array.range(0, arr.length)
    var c = 0

    // first pass for false
    var i = 0
    while(i < arr.length) {
      if (!arr(i)) {
        newArr(i) = c
        c += 1
      }
      i += 1
    }

    // second pass for true
    i = 0
    while(c < arr.length) {
      if (arr(i)) {
        newArr(i) = c
        c += 1
      }
      i += 1
    }

    newArr
  }

  // direct sort of boolean vector
  def sort(arr: Array[Boolean]): Array[Boolean] = {
    val newArr = array.empty[Boolean](arr.length)
    var c = 0
    var i = 0

    // count # false
    while(i < arr.length) {
      if (!arr(i)) c += 1
      i += 1
    }

    // populate true
    while(c < arr.length) {
      newArr(c) = true
      c += 1
    }

    newArr
  }
}