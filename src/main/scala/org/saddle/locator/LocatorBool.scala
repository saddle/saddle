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

/**
 * A bool-to-integer hash map
 */
class LocatorBool extends Locator[Boolean] {
  val map = Array.fill[Int](2)(-1)
  val cts = Array.ofDim[Int](2)

  def get(key: Boolean): Int = if (key) map(1) else map(0)

  def put(key: Boolean, value: Int) {
    val idx = if (key) 1 else 0
    map(idx) = value
  }

  def contains(key: Boolean) = get(key) != -1

  def size =
    if (contains(false) && contains(true)) 2
    else if (contains(true)) 1
    else if (contains(false)) 1
    else 0

  def keys() =
    if (contains(false) && contains(true)) Array(false, true)
    else if (contains(true)) Array(true)
    else if (contains(false)) Array(false)
    else Array.empty[Boolean]

  def counts() = cts

  def inc(key: Boolean): Int = {
    val idx = if (key) 1 else 0
    val tmp = cts(idx)
    cts(idx) += 1
    tmp
  }

  def count(key: Boolean) = cts(if (key) 1 else 0)
}

