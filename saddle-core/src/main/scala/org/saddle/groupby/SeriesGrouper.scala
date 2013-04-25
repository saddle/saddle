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

package org.saddle.groupby

import org.saddle._

/**
 * Helper class to do combine or transform after a groupBy
 */
class SeriesGrouper[Y: ST: ORD, X: ST: ORD, T: ST](
  ix: Index[Y], series: Series[X, T], sorted: Boolean = true) extends IndexGrouper[Y](ix, sorted) {

  def combine[U: ST: ORD](fn: (Y, Vec[T]) => U): Series[Y, U] =
    Series(SeriesGrouper.combine(ix, keys, series.values, fn), Index(keys))

  // less powerful combine, ignores group key
  def combine[U: ST: ORD](fn: Vec[T] => U): Series[Y, U] =
    combine( (k, v) => fn(v) )

  def transform[U: ST](fn: (Y, Vec[T]) => Vec[U]): Series[X, U] =
    Series(SeriesGrouper.transform(series.values, groups, fn), series.index)

  // less powerful transform, ignores group key
  def transform[U: ST](fn: Vec[T] => Vec[U]): Series[X, U] =
    transform( (k, v) => fn(v) )
}

object SeriesGrouper {
  // Collapses each group vector to a single value
  private[saddle] def combine[Y: ST: ORD, T: ST, U: ST](
    ix: Index[Y], uniq: Array[Y], vec: Vec[T], fn: (Y, Vec[T]) => U): Vec[U] = {
    val sz = uniq.length

    val res = Array.ofDim[U](sz)
    var i = 0
    while(i < sz) {
      val v = uniq(i)
      res(i) = fn(v, vec.take(ix(v)))
      i += 1
    }

    Vec(res)
  }

  // Transforms each group vector into a new vector
  private[saddle] def transform[Y: ST: ORD, T: ST, U: ST](
    vec: Vec[T], groups: Array[(Y, Array[Int])], fn: (Y, Vec[T]) => Vec[U]): Vec[U] = {
    val iter = for ( (k, i) <- groups) yield (fn(k, vec(i)), i)
    val res = Array.ofDim[U](vec.length)
    for ((v, i) <- iter) {
      val sz = v.length
      var k = 0
      while (k < sz) {
        // put each value back into original location
        res(i(k)) = v(k)
        k += 1
      }
    }
    Vec(res)
  }

  def apply[Y: ST: ORD, X: ST: ORD, T: ST](ix: Index[Y], ser: Series[X, T]) =
    new SeriesGrouper(ix, ser)

  def apply[Y: ST: ORD, T: ST](series: Series[Y, T]) =
    new SeriesGrouper(series.index, series)
}