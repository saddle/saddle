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

import scala.{specialized => spec}
import org.saddle._
import vec.{VecImpl, VecBool}
import util.Concat.Promoter
import index.IndexImpl.IndexProperties
import locator.Locator
import org.saddle.scalar._

/**
 * Index with integer keys
 */
class IndexBool(keys: Vec[Boolean]) extends Index[Boolean] {
  val scalarTag = ScalarTagBool

  private lazy val (kmap, IndexProperties(contiguous, monotonic)) = IndexImpl.keys2map(this)

  protected def locator: Locator[Boolean] = kmap

  def length: Int = keys.length

  def toVec: Vec[Boolean] = keys

  // get the key at the position specified
  def raw(idx: Int): Boolean = keys(idx)

  def take(locs: Array[Int]): Index[Boolean] = Index(array.take(keys, locs, IndexImpl.sentinelErr))

  def without(locs: Array[Int]): Index[Boolean] = Index(array.remove(keys, locs))

  def concat[B, C](x: Index[B])(implicit wd: Promoter[Boolean, B, C], mc: ST[C], oc: ORD[C]): Index[C] =
    Index(util.Concat.append[Boolean, B, C](toArray, x.toArray))

  def isMonotonic: Boolean = monotonic

  def isContiguous: Boolean = isUnique || contiguous

  def argSort: Array[Int] = VecBool.argSort(keys)

  def reversed: Index[Boolean] = new IndexBool(toVec.reversed)

  def join(other: Index[Boolean], how: JoinType = LeftJoin): ReIndexer[Boolean] =
    JoinerImpl.join(this, other, how)

  // Intersects two indices if both have set semantics
  def intersect(other: Index[Boolean]): ReIndexer[Boolean] = {
    if (!this.isUnique || !other.isUnique)
      throw Index.IndexException("Cannot intersect non-unique indexes")
    JoinerImpl.join(this, other, InnerJoin)
  }

  // Unions two indices if both have set semantics
  def union(other: Index[Boolean]): ReIndexer[Boolean] = {
    if (!this.isUnique || !other.isUnique)
      throw Index.IndexException("Cannot union non-unique indexes")
    JoinerImpl.join(this, other, OuterJoin)
  }

  def slice(from: Int, until: Int, stride: Int): Index[Boolean] = {
    new IndexBool(keys.slice(from, until, stride))
  }

  // find the first location whereby an insertion would maintain a sorted index
  def lsearch(t: Boolean): Int = {
    require(isMonotonic, "Index must be sorted")
    locator.get(t)
  }

  // find the last location whereby an insertion would maintain a sorted index
  def rsearch(t: Boolean): Int = {
    require(isMonotonic, "Index must be sorted")
    locator.get(t) + locator.count(t)
  }

  def map[@spec(Boolean, Int, Long, Double) B:ST: ORD](f: Boolean => B): Index[B] =
    Index(VecImpl.map(keys)(f).toArray)

  def toArray: Array[Boolean] = keys.toArray

  /**Default equality does an iterative, element-wise equality check of all values. */
  override def equals(o: Any): Boolean = {
    o match {
      case rv: IndexBool => (this eq rv) || (this.length == rv.length) && {
        var i = 0
        var eq = true
        while(eq && i < this.length) {
          eq &&= raw(i) == rv.raw(i)
          i += 1
        }
        eq
      }
      case _ => super.equals(o)
    }
  }
}