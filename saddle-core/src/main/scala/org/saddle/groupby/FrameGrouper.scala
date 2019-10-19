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

import org.saddle.{ST, ORD, Frame, Index, array, Vec}

/**
  * Helper class to do combine or transform after a groupBy
  */
class FrameGrouper[Z: ST: ORD, X: ST: ORD, Y: ST: ORD, T: ST](
    ix: Index[Z],
    frame: Frame[X, Y, T],
    sorted: Boolean = true
) {

  private lazy val uniq: Array[Z] = {
    val arr = ix.uniques.toArray
    if (sorted && !ix.isMonotonic)
      array.take(
        arr,
        array.argsort(arr),
        sys.error("Logic error in sorting group index")
      )
    else
      arr
  }

  def keys: Array[Z] = uniq

  def groups: Array[(Z, Array[Int])] = for (k <- keys) yield (k, ix.get(k))

  def combine[U: ST](fn: (Z, Vec[T]) => U): Frame[Z, Y, U] =
    Frame(frame.values.map(SeriesGrouper.combine(ix, keys, _, fn)): _*)
      .setRowIndex(keys)
      .setColIndex(frame.colIx)

  // less powerful combine, ignores group key
  def combine[U: ST: ORD](fn: Vec[T] => U): Frame[Z, Y, U] =
    combine((_, v) => fn(v))

  def transform[U: ST](fn: (Z, Vec[T]) => Vec[U]): Frame[X, Y, U] =
    Frame(
      frame.values.map(SeriesGrouper.transform(_, groups, fn)),
      frame.rowIx,
      frame.colIx
    )

  // less powerful transform, ignores group key
  def transform[U: ST](fn: Vec[T] => Vec[U]): Frame[X, Y, U] =
    transform((_, v) => fn(v))
}

object FrameGrouper {
  def apply[Z: ST: ORD, Y: ST: ORD, T: ST](frame: Frame[Z, Y, T]) =
    new FrameGrouper(frame.rowIx, frame)

  def apply[Z: ST: ORD, X: ST: ORD, Y: ST: ORD, T: ST](
      ix: Index[Z],
      frame: Frame[X, Y, T]
  ) = new FrameGrouper(ix, frame)
}
