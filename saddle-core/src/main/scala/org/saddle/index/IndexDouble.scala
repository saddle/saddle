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
import java.util.Arrays._
import org.saddle._
import util.Concat.Promoter
import index.IndexImpl.IndexProperties
import vec.VecImpl
import locator.Locator
import org.saddle.scalar.ScalarTagDouble

/**
 * Index with double keys
 */
class IndexDouble(keys: Vec[Double]) extends Index[Double] {
  val scalarTag = ScalarTagDouble

  private lazy val (kmap, IndexProperties(contiguous, monotonic)) = IndexImpl.keys2map(this)

  protected def locator: Locator[Double] = kmap

  def length: Int = keys.length

  def toVec: Vec[Double] = keys

  // get the key at the position specified
  def raw(idx: Int): Double = keys(idx)

  def take(locs: Array[Int]): Index[Double] = Index(array.take(keys, locs, IndexImpl.sentinelErr))

  def without(locs: Array[Int]): Index[Double] = Index(array.remove(keys, locs))

  def concat[B, C](x: Index[B])(implicit wd: Promoter[Double, B, C], mc: ST[C], oc: ORD[C]): Index[C] =
    Index(util.Concat.append[Double, B, C](toArray, x.toArray))

  def isMonotonic: Boolean = monotonic

  def isContiguous: Boolean = isUnique || contiguous

  def argSort: Array[Int] = array.argsort(keys.toArray)

  def reversed: Index[Double] = new IndexDouble(toVec.reversed)

  def join(other: Index[Double], how: JoinType = LeftJoin): ReIndexer[Double] =
    JoinerImpl.join(this, other, how)

  // Intersects two indices if both have set semantics
  def intersect(other: Index[Double]): ReIndexer[Double] = {
    if (!this.isUnique || !other.isUnique)
      throw Index.IndexException("Cannot intersect non-unique indexes")
    JoinerImpl.join(this, other, InnerJoin)
  }

  // Unions two indices if both have set semantics
  def union(other: Index[Double]): ReIndexer[Double] = {
    if (!this.isUnique || !other.isUnique)
      throw Index.IndexException("Cannot union non-unique indexes")
    JoinerImpl.join(this, other, OuterJoin)
  }

  def slice(from: Int, until: Int, stride: Int): Index[Double] = {
    new IndexDouble(keys.slice(from, until, stride))
  }

  // find the first location whereby an insertion would maintain a sorted index
  def lsearch(t: Double): Int = {
    require(isMonotonic, "Index must be sorted")

    val fnd = locator.count(t)

    if (fnd > 0)
      locator.get(t)
    else
      -(binarySearch(keys, t) + 1)
  }

  // find the last location whereby an insertion would maintain a sorted index
  def rsearch(t: Double): Int = {
    require(isMonotonic, "Index must be sorted")

    val fnd = locator.count(t)

    if (fnd > 0)
      fnd + locator.get(t)
    else
      -(binarySearch(keys, t) + 1)
  }

  def map[@spec(Boolean, Int, Long, Double) B: ST: ORD](f: Double => B): Index[B] =
    Index(VecImpl.map(keys)(f).toArray)

  def toArray: Array[Double] = keys.toArray

  /** Default equality does an iterative, element-wise equality check of all values. */
  override def equals(o: Any): Boolean = {
    o match {
      case rv: IndexDouble => (this eq rv) || (this.length == rv.length) && {
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