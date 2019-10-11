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
import org.saddle.util.DoubleMap
class LocatorDouble(sz: Int = Locator.INIT_CAPACITY) extends Locator[Double] {
  var keyOrder = new Buffer(new Array[Double](sz), 0)
  val map = new DoubleMap
  val cts = new DoubleMap

  def contains(key: Double): Boolean = map.get(key).isDefined
  def get(key: Double): Int = map.get(key).getOrElse(-1)
  def put(key: Double, value: Int) = if (!contains(key)) {
    map.update(key, value)
    keyOrder.+=(key)
  }
  def count(key: Double): Int = cts.get(key).getOrElse(0)
  def inc(key: Double): Int = {
    val u = count(key)
    cts.update(key, u + 1)
    u
  }
  def keys(): Array[Double] = keyOrder.toArray
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
