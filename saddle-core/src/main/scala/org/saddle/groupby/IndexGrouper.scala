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

import org.saddle.{ST, ORD, Index, array}

/**
  * Creates groups for each unique key in an index
  */
class IndexGrouper[Y: ST: ORD](ix: Index[Y], sorted: Boolean = true) {
  private lazy val uniq: Array[Y] = {
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

  def keys: Array[Y] = uniq

  def groups: Array[(Y, Array[Int])] = for (k <- keys) yield (k, ix.get(k))
}

object IndexGrouper {
  def apply[Y: ST: ORD](ix: Index[Y]) = new IndexGrouper(ix)
  def apply[Y: ST: ORD](ix: Index[Y], sorted: Boolean = true) =
    new IndexGrouper(ix, sorted)
}
