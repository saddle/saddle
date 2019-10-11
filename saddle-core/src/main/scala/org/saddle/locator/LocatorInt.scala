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
package org.saddle.locator

import org.saddle.Buffer
import org.saddle.util.IntMap

class LocatorInt(sz: Int = Locator.INIT_CAPACITY) extends Locator[Int] {
  var keyOrder = new Buffer(new Array[Int](sz), 0)
  val map = new IntMap
  val cts = new IntMap

  def contains(key: Int): Boolean = map.contains(key)
  def get(key: Int): Int = map.get(key).getOrElse(-1)
  def put(key: Int, value: Int) = if (!contains(key)) {
    map.update(key, value)
    keyOrder.+=(key)
  }
  def count(key: Int): Int = cts.get(key).getOrElse(0)
  def inc(key: Int): Int = {
    val u = count(key)
    cts.update(key, u + 1)
    u
  }
  def keys(): Array[Int] = keyOrder.toArray
  def counts(): Array[Int] = {
    val res = Array.ofDim[Int](size)
    var i = 0
    map.foreachKey { key =>
      res(i) = count(key)
      i += 1
    }
    res
  }
  def size: Int = keyOrder.length
}
