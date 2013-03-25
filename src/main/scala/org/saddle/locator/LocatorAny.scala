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

import it.unimi.dsi.fastutil.objects.{Object2IntLinkedOpenHashMap, Object2IntOpenHashMap}

/**
 * An object-to-integer hash map, backed by fastutil implementation
 */
private[saddle] class LocatorAny[T](sz: Int = 16) extends Locator[T] {
  val map = new Object2IntLinkedOpenHashMap[T](sz)
  val cts = new Object2IntOpenHashMap[T](sz)

  map.defaultReturnValue(-1)
  cts.defaultReturnValue(0)

  def get(key: T): Int = map.getInt(key)

  def put(key: T, value: Int) {
    // prevents unboxing!
    val tmp: Int = map.put(key, value)
  }

  def contains(key: T) = map.containsKey(key)

  def size = map.size()

  def inc(key: T) = cts.addTo(key, 1)

  def count(key: T) = cts.getInt(key)

  def counts() = {
    val iter = map.keySet().iterator()
    val res  = Array.ofDim[Int](size)
    var i = 0
    while(iter.hasNext) {
      res(i) = cts.getInt(iter.next())
      i += 1
    }
    res
  }

  def keys() = map.keySet().toArray.asInstanceOf[Array[T]]
}

object LocatorAny {
  def apply[T](sz: Int) = new LocatorAny[T](sz)
  def apply[T]() = new LocatorAny[T]()
}
