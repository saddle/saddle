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
import annotation.tailrec
import org.saddle.{ST, ORD, Vec, Index, array, util}
import org.saddle.util.Concat.Promoter
import org.saddle.index.IndexImpl.IndexProperties
import org.saddle.vec.VecImpl
import org.saddle.locator.Locator

/**
  * An implementation of [[org.saddle.Index]] generic in type T for which there is an Ordering[T]
  * and a ST[T] available in the implicit context.
  */
class IndexAny[T: ST: ORD](keys: Vec[T]) extends Index[T] {
  val scalarTag = keys.scalarTag

  private lazy val (lmap, IndexProperties(contiguous, monotonic)) =
    IndexImpl.keys2map(this)

  protected def locator: Locator[T] = lmap

  def length: Int = keys.length

  def toVec: Vec[T] = keys

  // get the key at the position specified
  def raw(idx: Int): T = keys.raw(idx)

  def take(locs: Array[Int]): Index[T] =
    Index(array.take(keys.toArray, locs, IndexImpl.sentinelErr))

  def without(locs: Array[Int]): Index[T] =
    Index(array.remove(keys.toArray, locs))

  def concat[B, C](
      x: Index[B]
  )(implicit wd: Promoter[T, B, C], mc: ST[C], oc: ORD[C]): Index[C] =
    Index(util.Concat.append[T, B, C](toArray, x.toArray))

  def isMonotonic: Boolean = monotonic

  def isContiguous: Boolean = isUnique || contiguous

  /** Returns offsets into index that would result in sorted index */
  def argSort: Array[Int] = array.argsort(keys.toArray)

  def reversed: Index[T] = new IndexAny[T](toVec.reversed)

  def join(other: Index[T], how: JoinType = LeftJoin): ReIndexer[T] =
    JoinerImpl.join(this, other, how)

  // Intersects two indices if both have set semantics
  def intersect(other: Index[T]): ReIndexer[T] = {
    if (!this.isUnique || !other.isUnique)
      throw Index.IndexException("Cannot intersect non-unique indexes")
    JoinerImpl.join(this, other, InnerJoin)
  }

  // Unions two indices if both have set semantics
  def union(other: Index[T]): ReIndexer[T] = {
    if (!this.isUnique || !other.isUnique)
      throw Index.IndexException("Cannot union non-unique indexes")
    JoinerImpl.join(this, other, OuterJoin)
  }

  def slice(from: Int, until: Int, stride: Int): Index[T] = {
    Index[T](keys.slice(from, until, stride))
  }

  // find the first location whereby an insertion would maintain a sorted index
  def lsearch(t: T): Int = {
    require(isMonotonic, "Index must be sorted")

    val fnd = locator.count(t)

    if (fnd > 0)
      locator.get(t)
    else
      -(binarySearch(keys.toArray, t) + 1)
  }

  // find the last location whereby an insertion would maintain a sorted index
  def rsearch(t: T): Int = {
    require(isMonotonic, "Index must be sorted")

    val fnd = locator.count(t)

    if (fnd > 0)
      fnd + locator.get(t)
    else
      -(binarySearch(keys.toArray, t) + 1)
  }

  // adapted from java source
  private def binarySearch(a: Array[T], key: T): Int = {
    @tailrec def bSearch(lo: Int, hi: Int): Int = {
      if (lo > hi) -(lo + 1)
      else {
        val mid: Int = (lo + hi) >>> 1
        val midVal: T = a(mid)
        if (scalarTag.lt(midVal, key)) bSearch(mid + 1, hi)
        else if (scalarTag.gt(midVal, key)) bSearch(lo, mid - 1)
        else mid
      }
    }
    bSearch(0, a.length - 1)
  }

  def map[@spec(Boolean, Int, Long, Double) B: ST: ORD](f: T => B): Index[B] =
    Index(VecImpl.map(keys)(f).toArray)

  def toArray: Array[T] = keys.toArray

  /**Default equality does an iterative, element-wise equality check of all values. */
  override def equals(o: Any): Boolean = {
    o match {
      case rv: IndexInt =>
        (this eq rv) || (this.length == rv.length) && {
          var i = 0
          var eq = true
          while (eq && i < this.length) {
            eq &&= raw(i) == rv.raw(i)
            i += 1
          }
          eq
        }
      case _ => super.equals(o)
    }
  }
}
