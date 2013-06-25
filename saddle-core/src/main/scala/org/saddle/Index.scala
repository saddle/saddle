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

package org.saddle

import scala.{specialized => spec, Array}
import index._
import scalar._
import locator.Locator
import util.Concat.Promoter
import vec.VecImpl
import java.io.OutputStream
import org.joda.time.DateTime
import org.saddle.time.RRule

/**
 * Index provides a constant-time look-up of a value within array-backed storage,
 * as well as operations to support joining and slicing.
 */
trait Index[@spec(Boolean, Int, Long, Double) T] {
  protected def locator: Locator[T]

  /**
   * Number of elements in the index
   */
  def length: Int

  /**
   * A [[org.saddle.scalar.ScalarTag]] representing the kind of Scalar
   * found in this index.
   */
  def scalarTag: ScalarTag[T]

  /**
   * Convert Index to a [[org.saddle.Vec]]
   */
  def toVec: Vec[T]

  /**
   * Access an element directly within the index, without wrapping in a Scalar
   * box.
   * @param loc Offset into the index
   */
  def raw(loc: Int): T

  // at method, gets index key(s) by location

  /**
   * Retrieve an element of the index at a particular offset
   * @param loc Offset into index
   */
  def at(loc: Int): Scalar[T] = {
    implicit val tag = scalarTag
    raw(loc)
  }

  /**
   * Retrieve several elements from the index at provided offets
   * @param locs An array of integer offsets
   */
  def at(locs: Array[Int]): Index[T] = take(locs)

  /**
   * Retrieve several elements from the index at provided offsets
   * @param locs A sequence of integer offsets
   */
  def at(locs: Int*): Index[T] = take(locs.toArray)

  /**
   * Given a sequence of keys, return the sequence of locations in the index
   * at which those keys correspondingly occur, ignoring keys which do not
   * exist.
   * @param keys Sequence of keys to find
   */
  def apply(keys: T*): Array[Int] = {
    val szhint = keys.length
    val result = Buffer[Int](szhint)
    var i = 0
    while(i < szhint) {
      val elems = get(keys(i))
      var k = 0
      while(k < elems.length) {
        result.add(elems(k))
        k += 1
      }
      i += 1
    }
    result
  }

  /**
   * Given an array of keys, return the sequence of locations in the index
   * at which those keys correspondingly occur, ignoring keys which do not
   * exist.
   * @param keys Sequence of keys to find
   */
  def apply(keys: Array[T]): Array[Int] = apply(keys : _*)

  /**
   * Take values of the index at certain locations, returning a new Index
   * consisting of those values.
   *
   * See also [[org.saddle.array.take]]
   *
   * @param locs Locations to take
   */
  def take(locs: Array[Int]): Index[T]

  /**
   * Complement of the take method; return a new Index whose values are those
   * which do not occur at the specified locations.
   *
   * @param locs Locations to omit
   */
  def without(locs: Array[Int]): Index[T]

  /**
   * Concatenate two Index objects together
   *
   * @param other Other index to concatenate
   * @param p Implicit evidence of a Promoter which can send both T and B to C
   * @param mc Implicit evidence of ST[C]
   * @param oc Implicit evidence of ORD[C]
   * @tparam B Type of other index
   * @tparam C Result of promoting types A, B
   */
  def concat[B, C](other: Index[B])(implicit p: Promoter[T, B, C], mc: ST[C], oc: ORD[C]): Index[C]

  /**
   * Find the first location whereby inserting a key would maintain a sorted index. Index
   * must already be sorted.
   * @param t Key that would be inserted
   */
  def lsearch(t: T): Int

  /**
   * Find the last location whereby inserting a key would maintain a sorted index. Index
   * must already be sorted.
   * @param t Key that would be inserted
   */
  def rsearch(t: T): Int

  /**
   * Returns a slice of an index between two keys; if inclusive is false, then exclude
   * the upper bound. Index must be sorted, as this method relies on lsearch and rsearch.
   * @param from Key lower bound
   * @param to Key upper bound
   * @param inclusive If true (default), include upper bound in slice
   */
  def sliceBy(from: T, to: T, inclusive: Boolean = true): Index[T] =
    if (inclusive) slice(lsearch(from), rsearch(to))
    else slice(lsearch(from), lsearch(to))

  /**
   * Returns a slice of Index between two keys, including both the lower and
   * upper keys.
   * @param rng An instance of
   */
  def sliceBy(rng: Slice[T]): Index[T] = {
    val (a, b) = rng(this)
    slice(a, b)
  }

  /**
   * Returns a slice of Index between two integers, including the `from` bound,
   * and excluding the `until` bound.
   * @param from Int, lower bound
   * @param until Int, one past upper bound
   * @param stride Default is 1, the step with which to advance over bound
   */
  def slice(from: Int, until: Int, stride: Int = 1): Index[T]

  /**
   * Returns true if there are no duplicate keys in the Index
   */
  def isUnique: Boolean = (locator.size == length)

  /**
   * Returns an array of unique keys in the Index, in the order in which they
   * originally appeared in the backing Vec.
   * @param ord Implicit ORD for instances of type T
   * @param tag Implicit ST for instances of type T
   */
  def uniques(implicit ord: ORD[T], tag: ST[T]): Index[T] = Index(Vec(locator.keys()))

  /**
   * Returns an array whose entries represent the number of times the corresponding
   * entry in `uniques` occurs within the index.
   */
  def counts: Array[Int] = locator.counts()

  /**
   * Return the number of times the key occurs in the index
   * @param key The key to query
   */
  def count(key: T): Int = locator.count(key)

  /**
   * Get first integer offset of a key
   * @param key Key to find in index
   */
  def getFirst(key: T): Int = locator.get(key)

  /**
   * Get last integer offset of a key
   * @param key Key to find in index
   */
  def getLast(key: T): Int = {
    val loc = getFirst(key)
    if (loc == -1)
      -1
    else if (isContiguous) {
      loc + locator.count(key) - 1
    }
    else {
      var i = loc + 1
      var c = locator.count(key)
      while(c > 1 && i < length) {
        if (raw(i) == key) c -= 1
        i += 1
      }
      i - 1
    }
  }

  /**
   * Get location offsets within Index given a particular key
   * @param key Key with which to search
   */
  def get(key: T): Array[Int] = {
    val firstLoc = locator.get(key)
    var count = 0
    if (firstLoc == -1)
      Array[Int]()
    else if (isUnique || { count = locator.count(key); 1 == count }) {
      Array(locator.get(key))
    }
    else if (isContiguous) {
      array.range(firstLoc, firstLoc + count)
    }
    else {
      val result = Array.ofDim[Int](count)
      var loc = firstLoc
      var i = 0
      while(loc < length && count != 0) {
        if( raw(loc) == key ) {
          result(i) = loc
          i += 1
          count -= 1
        }
        loc += 1
      }
      result
    }
  }

  /**
   * Returns a slice comprised of at most the first n elements of the Index
   * @param n Number of elements to slice
   */
  def head(n: Int): Index[T] = slice(0, math.min(n, length))

  /**
   * Returns a slice comprised of at most the last n elements of the Index
   * @param n Number of elements to slice
   */
  def tail(n: Int): Index[T] = slice(math.max(length - n, 0), length)

  /**
   * Returns the first element of the Index, or NA if there is none
   */
  def first: Scalar[T] = if (length > 0) at(0) else NA

  /**
   * Returns the last element of the Index, or NA if there is none
   */
  def last: Scalar[T] = if (length > 0) at(length - 1) else NA

  /**
   * Returns the index in sorted (ascending) order
   */
  def sorted: Index[T] = take(argSort)

  /**
   * Returns the index in reversed order
   */
  def reversed: Index[T]

  /**
   * Returns the int location of the first element of the index to satisfy the predicate function,
   * or -1 if no element satisfies the function.
   * @param pred Function from T => Boolean
   */
  def findOne(pred: T => Boolean): Int = VecImpl.findOne(toVec)(pred)(scalarTag)

  /**
   * Returns true if there is an element which satisfies the predicate function,
   * @param pred Function from T => Boolean
   */
  def exists(pred: T => Boolean): Boolean = findOne(pred) != -1

  /**
   * For an index which contains Tuples, drop the right-most element of each tuple, resulting
   * in a new index.
   * @param ev Implicit evidence of a Splitter instance that takes T (of arity N) to U (of arity N-1)
   * @tparam U Type of elements of result index
   */
  def dropLevel[U, _](implicit ev: Splitter[T, U, _]): Index[U] = ev(this)._1

  /**
   * Given this index whose elements have arity N and another index of arity 1, form a result
   * index whose entries are tuples of arity N+1 reflecting the Cartesian product of the two,
   * in the provided order. See [[org.saddle.index.Stacker]] for more details.
   * @param other Another Index
   * @param ev Implicit evidence of a Stacker
   * @tparam U The input type, of arity 1
   * @tparam V The result type, of arity N+1
   */
  def stack[U, V](other: Index[U])(implicit ev: Stacker[T, U, V]): Index[V] = ev(this, other)

  /**
   * Given this index contains tuples of arity N > 1, split will result in a pair of index
   * instances; the left will have elements of arity N-1, and the right arity 1.
   * @param ev Implicit evidence of an instance of Splitter
   * @tparam O1 Left index type (of arity N-1)
   * @tparam O2 Right index type (of arity 1)
   */
  def split[O1, O2](implicit ev: Splitter[T, O1, O2]): (Index[O1], Index[O2]) = ev(this)

  /**
   * Generates offsets into current index given another index for the purposes of
   * re-indexing. For more on reindexing, see [[org.saddle.index.ReIndexer]]. If
   * the current and other indexes are equal, a value of None is returned.
   *
   * @param other The other index with which to generate offsets
   */
  def getIndexer(other: Index[T]): Option[Array[Int]] = {
    val ixer = this.join(other, index.RightJoin)
    require(ixer.index.length == other.length, "Could not reindex unambiguously")
    ixer.lTake
  }

  /**
   * Returns true if the index contains at least one entry equal to the provided key
   * @param key Key to query
   */
  def contains(key: T): Boolean = locator.contains(key)

  /**
   * Produces a [[org.saddle.index.ReIndexer]] corresponding to the intersection of
   * this Index with another. Both indexes must have set semantics - ie, have no
   * duplicates.
   *
   * @param other The other index
   */
  def intersect(other: Index[T]): ReIndexer[T]

  /**
   * Produces a [[org.saddle.index.ReIndexer]] corresponding to the union of
   * this Index with another. Both indexes must have set semantics - ie, have no
   * duplicates.
   *
   * @param other The other index
   */
  def union(other: Index[T]): ReIndexer[T]

  // default implementation, could be sped up in specialized instances
  /**
   * Returns true if the ordering of the elements of the Index is non-decreasing.
   */
  def isMonotonic: Boolean

  /**
   * Returns true if the index is either unique, or any two or more duplicate keys
   * occur in consecutive locations in the index.
   */
  def isContiguous: Boolean

  /**
   * Returns offsets into index that would result in sorted index
   */
  def argSort: Array[Int]

  // sql-style joins

  /**
   * Allows for the following SQL-style joins between this index and another:
   *
   *   - [[org.saddle.index.LeftJoin]]
   *   - [[org.saddle.index.RightJoin]]
   *   - [[org.saddle.index.InnerJoin]]
   *   - [[org.saddle.index.OuterJoin]]
   *
   * @param other Another index
   * @param how join type, see [[org.saddle.index.JoinType]]
   */
  def join(other: Index[T], how: JoinType = LeftJoin): ReIndexer[T]

  /**
   * Given a key, return the previous value in the Index (in the natural, ie supplied,
   * order). The Index must at least be contiguous, if not unique.
   *
   * @param current Key value to find
   */
  def prev(current: Scalar[T]): Scalar[T] = {
    implicit val tag = scalarTag

    if (!isContiguous)
      throw Index.IndexException("Cannot traverse index that is not contiguous in its values")

    val prevSpot = locator.get(current.get) - 1
    prevSpot match {
      case x if x >= 0 => raw(x)
      case _           => current
    }
  }

  /**
   * Given a key, return the next value in the Index (in the natural, ie supplied,
   * order). The Index must at least be contiguous, if not unique.
   *
   * @param current Key value to find
   */
  def next(current: Scalar[T]): Scalar[T] = {
    implicit val tag = scalarTag

    if (!isContiguous)
      throw Index.IndexException("Cannot traverse index that is not contiguous in its values")

    val nextSpot = locator.get(current.get) + locator.count(current.get)
    nextSpot match {
      case x if x < length => raw(x)
      case _               => current
    }
  }

  /**
   * Map over the elements in the Index, producing a new Index, similar to Map in the
   * Scala collections.
   *
   * @param f Function to map with
   * @tparam B Type of resulting elements
   */
  def map[@spec(Boolean, Int, Long, Double) B: ST: ORD](f: T => B): Index[B]

  /**
   * Convert Index elements to an IndexedSeq.
   *
   */
  def toSeq: IndexedSeq[T] = toArray.toIndexedSeq

  private[saddle] def toArray: Array[T]

  /** Default hashcode is simple rolling prime multiplication of sums of hashcodes for all values. */
  override def hashCode(): Int = toVec.foldLeft(1)(_ * 31 + _.hashCode())

  /** Default equality does an iterative, element-wise equality check of all values. */
  override def equals(o: Any): Boolean = {
    o match {
      case rv: Index[_] => (this eq rv) || (this.length == rv.length) && {
        var i = 0
        var eq = true
        while(eq && i < this.length) {
          eq &&= raw(i) == rv.raw(i)
          i += 1
        }
        eq
      }
      case _ => false
    }
  }

  /**
   * Creates a string representation of Index
   * @param len Max number of elements to include
   */
  def stringify(len: Int = 10): String = {
    val half = len / 2

    val buf = new StringBuilder()

    val maxf = (a: List[Int], b: List[String]) => (a zip b).map(v => v._1.max(v._2.length))

    val varr = toArray
    val sm = scalarTag

    if (varr.length == 0)
      buf append "Empty Index"
    else {
      val vlens = util.grab(varr, half).map(sm.strList(_)).foldLeft(sm.strList(varr(0)).map(_.length))(maxf)

      buf.append("[Index %d x 1]\n" format (length))

      def createRow(r: Int) = {
        val lst = for ( (l, v) <- (vlens zip sm.strList(raw(r)))) yield v.formatted("%" + l + "s")
        lst.mkString(" ") + "\n"
      }

      buf append util.buildStr(len, length, createRow, " ... \n")
    }

    buf.toString()
  }

  /**
   * Pretty-printer for Index, which simply outputs the result of stringify.
   * @param len Number of elements to display
   */
  def print(len: Int = 10, stream: OutputStream = System.out) {
    stream.write(stringify(len).getBytes)
  }

  override def toString = stringify()
}

object Index {
  /**
   * Factory method to create an index from a Vec of elements
   * @param values Vec
   * @tparam C Type of elements in Vec
   */
  def apply[C: ST: ORD](values: Vec[C]): Index[C] = implicitly[ST[C]].makeIndex(values)

  /**
   * Factory method to create an index from an array of elements
   * @param arr Array
   * @tparam C Type of elements in array
   */
  def apply[C: ST: ORD](arr: Array[C]): Index[C] = apply(Vec(arr))

  /**
   * Factory method to create an index from a sequence of elements, eg
   *
   * {{{
   *   Index(1,2,3)
   *   Index(IndexedSeq(1,2,3) : _*)
   * }}}
   *
   * @param values Seq[C]
   * @tparam C Type of elements in Seq
   */
  def apply[C: ST: ORD](values: C*): Index[C] = apply(values.toArray)

  /**
   * Factory method to create an Index; the basic use case is to construct
   * a multi-level index (i.e., an Index of Tuples) via a Tuple of Vecs.
   *
   * For instance:
   *
   * {{{
   *   Index.make(vec.rand(10), vec.rand(10))
   * }}}
   *
   * @param values Values from which to construct the index
   * @param ev Implicit evidence of an IndexMaker that can utilize values
   * @tparam I The type of the values input
   * @tparam O The type of the elements of the result index
   */
  def make[I, O](values: I)(implicit ev: IndexMaker[I, O]): Index[O] = ev(values)

  /**
   * Factory method to create an Index from a recurrence rule between two
   * dates.
   *
   * For instance:
   *
   * {{{
   *   Index.make(RRules.bizEoms, datetime(2005,1,1), datetime(2005,12,31))
   * }}}
   *
   * @param rrule Recurrence rule to use
   * @param start The earliest datetime on or after which to being the recurrence
   * @param end   The latest datetime on or before which to end the recurrence
   */
  def make(rrule: RRule, start: DateTime, end: DateTime): Index[DateTime] = {
    import time._
    Index((rrule.copy(count = None) withUntil end from start).toSeq : _*)
  }

  /**
   * Factor method to create an empty Index
   * @tparam C type of Index
   */
  def empty[C: ST: ORD]: Index[C] = Index(Array.empty[C])

  // (safe) conversions

  /**
   * An array may be implicitly converted to an Index
   * @param arr Array
   * @tparam C Type of elements in array
   */
  implicit def arrayToIndex[C: ST: ORD](arr: Array[C]) = Index(arr)

  /**
   * A Vec may be implicitly converted to an Index
   * @param s Vec
   * @tparam C Type of elements in Vec
   */
  implicit def vecToIndex[C: ST: ORD](s: Vec[C]) = Index(s.toArray)

  /**
   * Provides an index-specific exception
   * @param err Error message
   */
  case class IndexException(err: String) extends RuntimeException(err)
}
