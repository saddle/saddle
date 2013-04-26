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

import org.saddle._
import scala.{ specialized => spec }

/**
 * The Locator class provides a mapping from a key to the first location of that key
 * within a corresponding [[org.saddle.Index]] containing at least one of that key.
 *
 * As it provides a representative for a subset of keys in the index, in the language
 * of category theory, this mapping is a ''section'':

 * Given an array x of type T, which represents a mapping
 *
 * {{{
 *            f
 *        {i} => {T}
 * }}}
 * for i in [0, x.length), Locator provides the associated section
 *
 * {{{
 *            s
 *        {T} => {i}
 *
 * }}}
 * where s(t) = min(i) for any i such that f(i) = t.
 */
trait Locator[@spec(Boolean, Int, Long, Double) T] {
  /**
   * Whether the instance contains the key
   * @param key The key to query
   */
  def contains(key: T): Boolean

  /**
   * Should return the first offset corresponding to the provided key,
   * or -1 if none was found.
   * @param key The key to query
   */
  def get(key: T): Int

  /**
   * Put a new key into the map along with an associated integer
   * value.
   * @param key Key to put into map
   * @param value Value to associate with key
   */
  def put(key: T, value: Int)

  /**
   * Return the number of times the key was entered into the map
   * @param key The key to query
   */
  def count(key: T): Int

  /**
   * Increment the count for a key
   * @param key The key whose count should increment
   */
  def inc(key: T): Int

  /**
   * Return the keys contained in the map in original insertion order
   */
  def keys(): Array[T]

  /**
   * Returns counts associated with the keys in the same order as keys()
   */
  def counts(): Array[Int]

  /**
   * Number of entries in the Locator map
   */
  def size: Int
}

object Locator {
  val INIT_CAPACITY = 16

  /**
   * Factory method to create a new Locator instance.
   * @param sz Backing hashmap size (default 16)
   * @tparam C Type of elements to be stored in Locator
   */
  def apply[C](sz: Int = 16)(implicit st: ST[C]): Locator[C] = st.makeLoc(sz)
}