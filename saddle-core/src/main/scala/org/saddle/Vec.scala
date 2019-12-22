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

import scala.{specialized => spec}

import vec.VecDefault
import index.Slice
import ops.{BinOpVec, NumericOps}
import scalar.{Scalar, ScalarTag}

import java.io.OutputStream
import org.saddle.ops.BinOpVecInPlace

object Vec extends BinOpVec with BinOpVecInPlace {

  /**
    * Factory method to create a Vec from an array of elements
    *
    * @param arr Array
    * @tparam T Type of elements in array
    */
  def apply[@spec(Boolean, Int, Long, Double) T](arr: Array[T])(
      implicit st: ST[T]
  ): Vec[T] = new VecDefault(arr, st)

  /**
    * Factory method to create a Vec from a sequence of elements. For example,
    *
    * {{{
    *   Vec(1,2,3)
    *   Vec(Seq(1,2,3) : _*)
    * }}}
    *
    * @param values Sequence
    * @tparam T Type of elements in Vec
    */
  def apply[@spec(Boolean, Int, Long, Double) T: ST](values: T*): Vec[T] =
    Vec(values.toArray)

  /**
    * Creates an empty Vec of type T.
    *
    * @tparam T Vec type parameter
    */
  def empty[T: ST]: Vec[T] = Vec(Array.empty[T])

  implicit class VecToBoolLogic(v: Vec[Boolean]) {

    /**
      * True if all elements are true
      */
    def all: Boolean = -1 == v.findOne(_ == false)

    /**
      * True if some elements are true
      */
    def some: Boolean = -1 != v.findOne(_ == true)

    /**
      * True if no elements are true
      */
    def none: Boolean = !some

    /**
      * Number of elements which are true
      */
    def countT: Int = v.foldLeft(0)((a, b) => a + (if (b) 1 else 0))

    /**
      * Number of elements which are false
      */
    def countF: Int = v.foldLeft(0)((a, b) => a + (if (b) 0 else 1))
  }
}

/**
  * `Vec` is an immutable container for 1D homogeneous data (a "vector"). It is
  * backed by an array and indexed from 0 to length - 1.
  *
  * Several element access methods are provided.
  *
  * The `apply()` method returns a slice of the original vector:
  *
  * {{{
  *   val v = Vec(1,2,3,4)
  *   v(0) == Vec(1)
  *   v(1, 2) == Vec(2,3)
  * }}}
  *
  * The `at` method returns an instance of a [[org.saddle.scalar.Scalar]], which behaves
  * much like an `Option` in that it can be either an instance of [[org.saddle.scalar.NA]]
  * or a [[org.saddle.scalar.Value]] case class:
  *
  * {{{
  *   Vec[Int](1,2,3,na).at(0) == Scalar(1)
  *   Vec[Int](1,2,3,na).at(3) == NA
  * }}}
  *
  *
  * The method `raw` accesses the underlying value directly.
  *
  * {{{
  *   Vec(1d,2,3).raw(0) == 1d
  * }}}
  *
  * `Vec` may be used in arithemetic expressions which operate on two `Vec`s or on a
  * `Vec` and a scalar value. A few examples:
  *
  * {{{
  *   Vec(1,2,3,4) + Vec(2,3,4,5) == Vec(3,5,7,9)
  *   Vec(1,2,3,4) * 2 == Vec(2,4,6,8)
  * }}}
  *
  * Note, Vec is implicitly convertible to an array for convenience; this could be
  * abused to mutate the contents of the Vec. Try to avoid this!
  *
  * @tparam T Type of elements within the Vec
  */
trait Vec[@spec(Boolean, Int, Long, Double) T] extends NumericOps[Vec[T]] {

  /**
    * The number of elements in the container                                                  F
    */
  def length: Int

  /**
    * A ScalarTag in the type of the elements of the Vec
    */
  def scalarTag: ScalarTag[T]

  /**
    * Set to true when the vec is shifted over the backing array
    * false iff the backing array is a contiguous sequence of the elements of this Vec
    * false iff 0 until length map (raw) == the backing array
    */
  def needsCopy: Boolean

  // ----------
  // get values

  /**
    * Access a boxed element of a Vec[A] at a single location
    * @param loc offset into Vec
    */
  def at(loc: Int): Scalar[T]

  /**
    * Access an unboxed element of a Vec[A] at a single location
    * @param loc offset into Vec
    */
  def raw(loc: Int): T

  /**
    * Slice a Vec at a sequence of locations, e.g.
    *
    * val v = Vec(1,2,3,4,5)
    * v(1,3) == Vec(2,4)
    *
    * @param locs locations at which to slice
    */
  def apply(locs: Int*): Vec[T]

  /**
    * Slice a Vec at a sequence of locations, e.g.
    *
    * val v = Vec(1,2,3,4,5)
    * v(Array(1,3)) == Vec(2,4)
    *
    * @param locs locations at which to slice
    */
  def apply(locs: Array[Int]): Vec[T]

  /**
    * Slice a Vec at a sequence of locations, e.g.
    *
    * val v = Vec(1,2,3,4,5)
    * v(Vec(1,3)) == Vec(2,4)
    *
    * @param locs locations at which to slice
    */
  def apply(locs: Vec[Int]): Vec[T]

  /**
    * Slice a Vec at a bound of locations, e.g.
    *
    * val v = Vec(1,2,3,4,5)
    * v(1->3) == Vec(2,3,4)
    *
    * @param rng evaluates to IRange
    */
  def apply(rng: Slice[Int]): Vec[T]

  /**
    * Access the first element of a Vec[A], or NA if length is zero
    */
  def first: Scalar[T]

  /**
    * Access the last element of a Vec[A], or NA if length is zero
    */
  def last: Scalar[T]

  // ----------

  /**
    * Return copy of backing array
    */
  def contents: Array[T]

  /**
    * Return first n elements
    * @param n Number of elements to access
    */
  def head(n: Int): Vec[T]

  /**
    * Return last n elements
    * @param n Number of elements to access
    */
  def tail(n: Int): Vec[T]

  /**
    * True if and only if number of elements is zero
    */
  def isEmpty: Boolean

  /**
    * Equivalent to slicing operation; e.g.
    *
    * {{{
    *   val v = Vec(1,2,3)
    *   v.take(0,1) == v(0,1)
    * }}}
    *
    * @param locs Location of elements to take
    */
  def take(locs: Array[Int]): Vec[T]

  /**
    * Returns a Vec with the first `i` elements
    */
  def takeLeft(i: Int): Vec[T]

  /**
    * Returns a Vec with the last `i` elements
    */
  def takeRight(i: Int): Vec[T]

  /**
    * Returns a Vec with the first `i` elements removed
    */
  def dropLeft(i: Int): Vec[T]

  /**
    * Returns a Vec with the last `i` elements removed
    */
  def dropRight(i: Int): Vec[T]

  /**
    * The complement of the take operation; slice out
    * elements NOT specified in list.
    *
    * @param locs Location of elements not to take
    */
  def without(locs: Array[Int]): Vec[T]

  /**
    * Returns Vec whose locations corresponding to true entries in the
    * boolean input mask vector are set to NA
    *
    * @param m Mask vector of Vec[Boolean]
    */
  def mask(m: Vec[Boolean]): Vec[T]

  /**
    * Returns Vec whose locations are NA where the result of the
    * provided function evaluates to true
    *
    * @param f A function taking an element and returning a Boolean
    */
  def mask(f: T => Boolean): Vec[T]

  /**
    * Concatenate two Vec instances together, where there exists some way to
    * join the type of elements. For instance, Vec[Double] concat Vec[Int]
    * will promote Int to Double as a result of the implicit existence of an
    * instance of Promoter[Double, Int, Double]
    *
    * @param v  Vec[B] to concat
    * @param wd Implicit evidence of Promoter[A, B, C]
    * @param mc Implicit evidence of ST[C]
    * @tparam B type of other Vec elements
    * @tparam C type of resulting Vec elements
    */
  def concat(v: Vec[T]): Vec[T]

  /**
    * Additive inverse of Vec with numeric elements
    *
    */
  def unary_-()(implicit num: NUM[T]): Vec[T]

  // Must implement specialized methods independently of specialized class, workaround to
  // https://issues.scala-lang.org/browse/SI-5281

  /**
    * Map a function over the elements of the Vec, as in scala collections library
    */
  def map[@spec(Boolean, Int, Long, Double) B: ST](f: T => B): Vec[B]

  /**
    * Maps a function over elements of the Vec and flattens the result.
    *
    * NAs are ignored and `f` is never called on a NA
    */
  def flatMap[@spec(Boolean, Int, Long, Double) B: ST](f: T => Vec[B]): Vec[B]

  /**
    * Left fold over the elements of the Vec, as in scala collections library
    */
  def foldLeft[@spec(Boolean, Int, Long, Double) B: ST](init: B)(
      f: (B, T) => B
  ): B

  /**
    * Left scan over the elements of the Vec, as in scala collections library
    */
  def scanLeft[@spec(Boolean, Int, Long, Double) B: ST](init: B)(
      f: (B, T) => B
  ): Vec[B]

  /**
    * Filtered left fold over the elements of the Vec, as in scala collections library
    */
  def filterFoldLeft[@spec(Boolean, Int, Long, Double) B: ST](
      pred: T => Boolean
  )(init: B)(f: (B, T) => B): B

  /**
    * Filtered left scan over elements of the Vec, as in scala collections library
    */
  def filterScanLeft[@spec(Boolean, Int, Long, Double) B: ST](
      pred: T => Boolean
  )(init: B)(f: (B, T) => B): Vec[B]

  /**
    * Left fold that folds only while the test condition holds true. As soon as the condition function yields
    * false, the fold returns.
    *
    * @param cond Function whose signature is the same as the fold function, except that it evaluates to Boolean
    */
  def foldLeftWhile[@spec(Boolean, Int, Long, Double) B: ST](init: B)(
      f: (B, T) => B
  )(cond: (B, T) => Boolean): B

  /**
    * Zips Vec with another Vec and applies a function to the paired elements. If either of the pair is NA, the
    * result is forced to NA.
    * @param other Vec[B]
    * @param f Function (A, B) => C
    * @tparam B Parameter of other Vec
    * @tparam C Result of function
    */
  def zipMap[
      @spec(Int, Long, Double) B: ST,
      @spec(Boolean, Int, Long, Double) C: ST
  ](other: Vec[B])(f: (T, B) => C): Vec[C]

  /**
    * Drop the elements of the Vec which are NA
    */
  def dropNA: Vec[T]

  /**
    * Return true if there is an NA value in the Vec
    */
  def hasNA: Boolean

  /**
    * Execute a (side-effecting) operation on each (non-NA) element in the vec
    * @param op operation to execute
    */
  def foreach(op: T => Unit): Unit

  /**
    * Execute a (side-effecting) operation on each (non-NA) element in vec which satisfies
    * some predicate.
    * @param pred Function A => Boolean
    * @param op Side-effecting function
    */
  def forall(pred: T => Boolean)(op: T => Unit): Unit

  /**
    * Return Vec of integer locations (offsets) which satisfy some predicate
    * @param pred Predicate function from A => Boolean
    */
  def find(pred: T => Boolean): Vec[Int]

  /**
    * Return first integer location which satisfies some predicate, or -1 if there is none
    * @param pred Predicate function from A => Boolean
    */
  def findOne(pred: T => Boolean): Int

  /**
    * Return true if there exists some element of the Vec which satisfies the predicate function
    * @param pred Predicate function from A => Boolean
    */
  def exists(pred: T => Boolean): Boolean

  /**
    * Return Vec whose elements satisfy a predicate function
    * @param pred Predicate function from A => Boolean
    */
  def filter(pred: T => Boolean): Vec[T]

  /**
    * Return vec whose offets satisfy a predicate function
    * @param pred Predicate function from Int => Boolean
    */
  def filterAt(pred: Int => Boolean): Vec[T]

  /**
    * Return Vec whose elements are selected via a Vec of booleans (where that Vec holds the value true)
    * @param pred Predicate vector: Vec[Boolean]
    */
  def where(pred: Vec[Boolean]): Vec[T]

  /**
    * Produce a Vec whose entries are the result of executing a function on a sliding window of the
    * data.
    * @param winSz Window size
    * @param f Function Vec[A] => B to operate on sliding window
    * @tparam B Result type of function
    */
  def rolling[@spec(Boolean, Int, Long, Double) B: ST](
      winSz: Int,
      f: Vec[T] => B
  ): Vec[B]

  /**
    * Yield a Vec whose elements have been sorted (in ascending order)
    * @param ev evidence of Ordering[A]
    */
  def sorted(implicit ev: ORD[T], st: ST[T]): Vec[T]

  /**
    * Yield a Vec whose elements have been reversed from their original order
    */
  def reversed: Vec[T]

  /**
    * Creates a view into original vector from an offset up to, but excluding,
    * another offset. Data is not copied.
    *
    * @param from Beginning offset
    * @param until One past ending offset
    * @param stride Increment within slice
    */
  def slice(from: Int, until: Int, stride: Int = 1): Vec[T]

  /**
    * Creates a view into original vector from an offset up to, and including,
    * another offset. Data is not copied.
    *
    * @param from Beginning offset
    * @param to Ending offset
    * @param stride Increment within slice
    */
  def sliceBy(from: Int, to: Int, stride: Int = 1): Vec[T]

  /**
    * Split Vec into two Vecs at position i
    * @param i Position at which to split Vec
    */
  def splitAt(i: Int): (Vec[T], Vec[T])

  /**
    * Creates a view into original Vec, but shifted so that n
    * values at the beginning or end of the Vec are NA's. Data
    * is not copied.
    *
    * @param n Number of offsets to shift
    */
  def shift(n: Int): Vec[T]

  /**
    * Fills NA values in vector with result of a function which acts on the index of
    * the particular NA value found
    *
    * @param f A function from Int => A; yields value for NA value at ith position
    */
  def fillNA(f: Int => T): Vec[T]

  /**
    * Converts Vec to an indexed sequence (default implementation is immutable.Vector)
    *
    */
  def toSeq: IndexedSeq[T]

  /* Sum of numeric elements, ignoring NAs */
  def sum(implicit na: NUM[T], st: ST[T]): T

  /* Min of numeric elements, ignoring NAs */
  def min(implicit na: NUM[T], st: ST[T]): Scalar[T]

  /* Max of numeric elements, ignoring NAs */
  def max(implicit na: NUM[T], st: ST[T]): Scalar[T]

  /* Count of elements, ignoring NAs */
  def count: Int

  /* Min of elements passing the predicate, ignoring NAs */
  def countif(test: T => Boolean): Int

  /**
    * Product of all the values in the Vec, ignoring NA values
    */
  def prod(implicit na: NUM[T], st: ST[T]): T

  /**
    * Integer offset of the minimum element of the Vec, if one exists, or else -1
    */
  def argmin(implicit na: NUM[T], st: ST[T], ord: ORD[T]): Int

  /**
    * Integer offset of the minimum element of the Vec, if one exists, or else -1
    */
  def argmax(implicit na: NUM[T], st: ST[T], ord: ORD[T]): Int

  /**
    * Return the median of the values in the Vec, ignoring NA
    */
  def median(implicit na: NUM[T]): Double

  /**
    * Return the mean of the values in the Vec, ignoring NA
    */
  def mean(implicit na: NUM[T]): Double

  /**
    * Return the percentile of the values at a particular threshold, ignoring NA
    * @param tile The percentile in [0, 100] at which to compute the threshold
    * @param method The percentile method; one of [[org.saddle.PctMethod]]
    */
  def percentile(tile: Double, method: PctMethod = PctMethod.NIST)(
      implicit na: NUM[T]
  ): Double

  /**
    * Return a Vec of ranks corresponding to a Vec of numeric values.
    * @param tie Method with which to break ties; a [[org.saddle.RankTie]]
    * @param ascending Boolean, default true, whether to give lower values larger rank
    */
  def rank(tie: RankTie = RankTie.Avg, ascending: Boolean = true)(
      implicit na: NUM[T]
  ): Vec[Double]

  /**
    * Returns a Vec whose backing array has been copied
    */
  def copy: Vec[T]

  /* Returns an array containing the elements of the Vec
   *
   * The returned array may be shared with the original Vec
   */
  def toArray: Array[T]

  /* Not necessarily copies */
  private[saddle] def toDoubleArray(implicit na: NUM[T]): Array[Double]

  /**
    * Creates a string representation of Vec
    * @param len Max number of elements to include
    */
  def stringify(len: Int): String

  /**
    * Pretty-printer for Vec, which simply outputs the result of stringify.
    * @param len Number of elements to display
    */
  def print(len: Int, stream: OutputStream): Unit

  /**
    * Rounds elements in the vec (which must be numeric) to
    * a significance level
    *
    * @param sig Significance level to round to (e.g., 2 decimal places)
    */
  def roundTo(sig: Int = 2)(implicit ev: NUM[T]): Vec[Double]

  /** Returns a new Vec with the value at `offset` set to `value
    *
    * Copies before mutating.
    */
  def updated(offset: Int, value: T): Vec[T]

  /** Updates (overwrites) a location in the Vec
    *
    * Mutates the underlying array
    */
  def update(offset: Int, value: T): Unit

  /** Updates (overwrites) a range in the Vec
    *
    * Mutates the underlying array
    */
  def update(slice: Slice[Int], value: T): Unit

  /** Updates (overwrites) a range in the Vec
    *
    * Mutates the underlying array
    */
  def update(slice: Slice[Int], value: Vec[T]): Unit

  /** Returns a new Vec with the values at `offset` set to `value
    *
    * Copies before mutating.
    * Ignores invalid offsets in the array
    */
  def updated(offsets: Array[Int], value: T): Vec[T]

  /** Reshapes the Vec into a Mat
    *
    * May not copy. The new Mat instance may share data
    * with this Vec.
    */
  def reshape(rows: Int, cols: Int): Mat[T]

}
