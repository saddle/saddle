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

import it.unimi.dsi.fastutil.doubles.{Double2IntOpenHashMap, Double2IntLinkedOpenHashMap}

/**
 * A double-to-integer hash map, backed by fastutil implementation
 */
class LocatorDouble(sz: Int = Locator.INIT_CAPACITY) extends Locator[Double] {
  val map = new Double2IntLinkedOpenHashMap(sz)
  val cts = new Double2IntOpenHashMap(sz)

  map.defaultReturnValue(-1)
  cts.defaultReturnValue(0)

  def get(key: Double): Int = map.get(key)

  def put(key: Double, value: Int) {
    // prevents unboxing!
    val tmp = map.put(key, value)
  }

  def contains(key: Double) = map.containsKey(key)

  def size = map.size()

  def keys() = map.keySet().toDoubleArray

  def inc(key: Double): Int = cts.addTo(key, 1)

  def count(key: Double) = cts.get(key)

  def counts() = {
    val iter = map.keySet().iterator()
    val res  = Array.ofDim[Int](size)
    var i = 0
    while(iter.hasNext) {
      res(i) = cts.get(iter.nextDouble())
      i += 1
    }
    res
  }
}
