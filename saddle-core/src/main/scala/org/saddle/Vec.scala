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

import vec._
import index._
import ops._
import scalar._
import util.Concat.Promoter

import java.io.OutputStream


object Vec extends BinOpVec with VecBoolEnricher {
  import scala.language.implicitConversions
  /**
   * Factory method to create a Vec from an array of elements
   *
   * @param arr Array
   * @tparam T Type of elements in array
   */
  def apply[@spec(Boolean, Int, Long, Double) T](arr: Array[T])(implicit st: ST[T]): Vec[T] = new VecDefault(arr, st)

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
  def apply[@spec(Boolean, Int, Long, Double) T: ST](values: T*): Vec[T] = Vec(values.toArray)

  /**
   * Creates an empty Vec of type T.
   *
   * @tparam T Vec type parameter
   */
  def empty[T: ST]: Vec[T] = Vec(Array.empty[T])

  // **** conversions

  // Vec is isomorphic to array

  /**
   * A Vec may be implicitly converted to an array. Use responsibly;
   * please do not circumvent immutability of Vec class!
   * @param s Vec
   * @tparam T Type parameter of Vec
   */
  implicit def vecToArray[T](s: Vec[T]) = s.toArray

  /**
   * An array may be implicitly converted to a Vec.
   * @param arr Array
   * @tparam T Type parameter of Array
   */
  implicit def arrayToVec[T: ST](arr: Array[T]) = Vec(arr)

  /**
   * A Vec may be implicitly ''widened'' to a Series.
   *
   * @param s Vec to widen to Series
   * @tparam A Type of elements in Vec
   */
  implicit def vecToSeries[A: ST](s: Vec[A]) = Series(s)

  /**
   * A Vec may be implicitly converted to a single column Mat
   */
  implicit def vecToMat[A: ST](s: Vec[A]): Mat[A] = Mat(s)
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
trait Vec[@spec(Boolean,Int,Long,Double) T] extends NumericOps[Vec[T]] {
  /**
   * The number of elements in the container                                                  F
   */
  def length: Int

  /**
   * A ScalarTag in the type of the elements of the Vec
   */
  def scalarTag: ScalarTag[T]

  /**
   * Danger - could expose internal NA's
   *
   * Access an element by location. This is made private because the internal
   * representation might contain primitive NA's that need to be boxed so that
   * they aren't utilized unknowingly in calculations.
   */
  def apply(loc: Int): T

  /**
   * Set to true when the vec is shifted over the backing array
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
  def concat[B, C](v: Vec[B])(implicit wd: Promoter[T, B, C], mc: ST[C]): Vec[C]

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
   */
  def flatMap[@spec(Boolean, Int, Long, Double) B : ST](f: T => Vec[B]): Vec[B]

  /**
   * Left fold over the elements of the Vec, as in scala collections library
   */
  def foldLeft[@spec(Boolean, Int, Long, Double) B: ST](init: B)(f: (B, T) => B): B

  /**
   * Left scan over the elements of the Vec, as in scala collections library
   */
  def scanLeft[@spec(Boolean, Int, Long, Double) B: ST](init: B)(f: (B, T) => B): Vec[B]

  /**
   * Filtered left fold over the elements of the Vec, as in scala collections library
   */
  def filterFoldLeft[@spec(Boolean, Int, Long, Double) B: ST](pred: T => Boolean)(init: B)(f: (B, T) => B): B

  /**
   * Filtered left scan over elements of the Vec, as in scala collections library
   */
  def filterScanLeft[@spec(Boolean, Int, Long, Double) B: ST](pred: T => Boolean)(init: B)(f: (B, T) => B): Vec[B]

  /**
   * Left fold that folds only while the test condition holds true. As soon as the condition function yields
   * false, the fold returns.
   *
   * @param cond Function whose signature is the same as the fold function, except that it evaluates to Boolean
   */
  def foldLeftWhile[@spec(Boolean, Int, Long, Double) B: ST](init: B)(f: (B, T) => B)(cond: (B, T) => Boolean): B

  /**
   * Zips Vec with another Vec and applies a function to the paired elements. If either of the pair is NA, the
   * result is forced to NA.
   * @param other Vec[B]
   * @param f Function (A, B) => C
   * @tparam B Parameter of other Vec
   * @tparam C Result of function
   */
  def zipMap[@spec(Int, Long, Double) B: ST,
             @spec(Boolean, Int, Long, Double) C: ST](other: Vec[B])(f: (T, B) => C): Vec[C]

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
  def foreach(op: T => Unit) : Unit

  /**
   * Execute a (side-effecting) operation on each (non-NA) element in vec which satisfies
   * some predicate.
   * @param pred Function A => Boolean
   * @param op Side-effecting function
   */
  def forall(pred: T => Boolean)(op: T => Unit) : Unit 

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
  def rolling[@spec(Boolean, Int, Long, Double) B: ST](winSz: Int, f: Vec[T] => B): Vec[B]

  /**
   * Yield a Vec whose elements have been sorted (in ascending order)
   * @param ev evidence of Ordering[A]
   */
  def sorted(implicit ev: ORD[T], st: ST[T]) : Vec[T]

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
   * Replaces all NA values for which there is a non-NA value at a lower offset
   * with the corresponding highest-offset, non-NA value. E.g,
   *
   * {{{
   *   Vec(1, 2, NA, 3, NA).pad == Vec(1, 2, 2, 3, 3)
   *   Vec(NA, 1, 2, NA).pad == Vec(NA, 1, 2, 2)
   * }}}
   *
   */
  def pad: Vec[T] 

  /**
   * Replaces all NA values for which there is a non-NA value at a lower offset
   * with the corresponding highest-offset, non-NA value; but looking back only
   * at most N positions.
   *
   * {{{
   *   Vec(1, 2, NA, 3, NA).padAtMost(1) == Vec(1, 2, 2, 3, 3)
   *   Vec(NA, 1, 2, NA).padAtMost(1) == Vec(NA, 1, 2, 2)
   *   Vec(1, NA, NA, 3, NA).padAtMost(1) == Vec(1, 1, NA, 3, 3)
   * }}}
   *
   */
  def padAtMost(n: Int): Vec[T] 

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

  def sum(implicit na: NUM[T], st: ST[T]): T 
  def count: Int 
  def countif(test: T => Boolean): Int 

  /**
   * Returns a Vec whose backing array has been copied
   */
  protected def copy: Vec[T]

  private[saddle] def toArray: Array[T]

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
  def print(len: Int, stream: OutputStream) 

}
