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

import org.saddle.ST
import org.saddle.Buffer

class LocatorAny[T: ST](sz: Int = Locator.INIT_CAPACITY) extends Locator[T] {
  val keyOrder = new Buffer(new Array[T](sz), 0)
  val map = new scala.collection.mutable.HashMap[T, Int]
  val cts = new scala.collection.mutable.HashMap[T, Int]

  def contains(key: T): Boolean = map.contains(key)
  def get(key: T): Int = map.get(key).getOrElse(-1)
  def put(key: T, value: Int) = if (!contains(key)) {
    map.update(key, value)
    keyOrder.+=(key)
  }
  def count(key: T): Int = cts.get(key).getOrElse(0)
  def inc(key: T): Int = {
    val u = count(key)
    cts.update(key, u + 1)
    u
  }
  def keys(): Array[T] = keyOrder.toArray
  def counts(): Array[Int] = {
    val iter = map.keys.iterator
    val res = Array.ofDim[Int](size)
    var i = 0
    while (iter.hasNext) {
      res(i) = count(iter.next)
      i += 1
    }
    res
  }
  def size: Int = keyOrder.length
}
