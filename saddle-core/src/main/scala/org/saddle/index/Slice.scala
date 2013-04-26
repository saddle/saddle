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

package org.saddle.index

import org.saddle.Index

/**
 * Slice provides a methodology so that when it is applied to an index,
 * it produces an upper and lower integer offset at which to slice.
 */
trait Slice[+T] {
  def apply[U >: T](idx: Index[U]): (Int, Int)
}

// implementations

/**
 * Represent a slice from one key to another, inclusive.
 * @param k1 First key
 * @param k2 Second key
 * @tparam T Type of Key
 */
class SliceDefault[T](k1: T, k2: T) extends Slice[T] {
  def apply[U >: T](idx: Index[U]): (Int, Int) = (idx.lsearch(k1), idx.rsearch(k2))
}

/**
 * Represent a slice from zero to a key.
 * @param k Key to slice to
 * @tparam T Type of Key
 */
class SliceTo[T](k: T) extends Slice[T] {
  def apply[U >: T](idx: Index[U]): (Int, Int) = (0, idx.rsearch(k))
}

/**
 * Represent a slice from key to end of index
 * @param k Key to slice from
 * @tparam T Type of Key
 */
class SliceFrom[T](k: T) extends Slice[T] {
  def apply[U >: T](idx: Index[U]): (Int, Int) = (idx.lsearch(k), idx.length)
}

/**
 * Represent a slice over the entire index
 */
class SliceAll extends Slice[Nothing] {
  def apply[U](idx: Index[U]): (Int, Int) = (0, idx.length)
}

// companion objects

object Slice {
  def apply[T](k1: T, k2: T) = new SliceDefault(k1, k2)
}

object SliceFrom {
  def apply[T](k: T) = new SliceFrom(k)
}

object SliceTo {
  def apply[T](k: T) = new SliceTo(k)
}

object SliceAll {
  def apply[T](k: T) = new SliceAll()
}