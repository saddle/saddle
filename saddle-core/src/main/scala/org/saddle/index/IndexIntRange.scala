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
import org.saddle.{array, Index, ST, ORD, util, Vec}
import org.saddle.scalar.ScalarTagInt
import org.saddle.locator.Locator

/**
  * An implementation of an Index[Int] which implicitly represents a bound of integers,
  * which lazily generates its elements as an array when needed. This compact representation
  * is the default when creating a Saddle object such as [[org.saddle.Series]] which
  * requires and index and one is not supplied.
  */
class IndexIntRange(val length: Int, val from: Int = 0) extends Index[Int] {
  require(length >= 0, "Length must be non-negative!")

  @transient lazy val scalarTag = ScalarTagInt

  @transient private lazy val asArr = array.range(from, from + length)
  @transient private lazy val genIdx = Index(asArr)

  /**
    * Custom implementation of a Locator to serve as the backing map in a
    * more space-efficient manner than the full blown LocatorInt implementation.
    */
  protected def locator = new Locator[Int] {
    def size = length

    lazy val cts = {
      val res = Array.ofDim[Int](length)
      var i = 0
      while (i < length) {
        res(i) = 1
        i += 1
      }
      res
    }

    def contains(key: Int) = key >= from && key < from + length
    def get(key: Int) = if (contains(key)) key - from else -1
    def count(key: Int) = if (contains(key)) 1 else 0

    def put(key: Int, value: Int) = { sys.error("Not supported") }
    def inc(key: Int) = sys.error("Not supported")
    def keys() = asArr
    def counts() = cts
  }

  private def guardLoc(loc: Int): Int =
    if (loc < 0 || loc >= length)
      throw new ArrayIndexOutOfBoundsException(
        "Location %d is out of bounds" format loc
      )
    else
      loc

  def raw(loc: Int) = from + guardLoc(loc)

  def toVec: Vec[Int] = Vec(asArr)

  // take values of index at certain locations
  def take(locs: Array[Int]) =
    Index(
      Vec(locs)
        .map(i => if (i == -1) IndexImpl.sentinelErr else guardLoc(i) + from)
    )

  def without(locs: Array[Int]): Index[Int] =
    Index(array.remove(asArr, locs))

  def concat(
      x: Index[Int]
  ): Index[Int] =
    Index(util.Concat.append(toArray, x.toArray))

  // find the first location whereby an insertion would maintain a sorted index
  def lsearch(t: Int) = math.min(math.max(0, from + t), from + length)

  // find the last location whereby an insertion would maintain a sorted index
  def rsearch(t: Int) = math.min(math.max(0, from + t + 1), from + length)

  // slice at array locations, [from, until)
  def slice(from: Int, until: Int, stride: Int) =
    if (stride == 1)
      new IndexIntRange(
        math.min(length, until - from),
        math.max(this.from + math.max(from, 0), 0)
      )
    else
      genIdx.slice(from, until, stride)

  def getAll(keys: Array[Int]) =
    Vec(keys).filter(locator.contains _).map(_ - from).toArray

  def isMonotonic = true

  def isContiguous = true

  def argSort = asArr

  def reversed = Index(asArr).reversed

  def intersect(other: Index[Int]) = JoinerImpl.join(this, other, InnerJoin)

  def union(other: Index[Int]) = JoinerImpl.join(this, other, OuterJoin)

  def join(other: Index[Int], how: JoinType = LeftJoin): ReIndexer[Int] =
    JoinerImpl.join(this, other, how)

  def map[@spec(Boolean, Int, Long, Double) B: ST: ORD](
      f: (Int) => B
  ): Index[B] =
    genIdx map f

  private[saddle] def toArray = asArr
}

object IndexIntRange {
  def apply(length: Int, from: Int = 0) = new IndexIntRange(length, from)
}
