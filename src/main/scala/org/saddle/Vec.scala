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

import ops.{BinOpVec, NumericOps}
import scalar._
import util.Concat.Promoter
import vec._
import stats._
import index._
import java.io.OutputStream
import org.joda.time.DateTime

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
 * @tparam A Type of elements within the Vec
 */
trait Vec[@spec(Boolean, Int, Long, Double) A] extends NumericOps[Vec[A]] {
  /**
   * The number of elements in the container
   */
  def length: Int

  /**
   * A ScalarTag in the type of the elements of the Vec
   */
  def scalarTag: ScalarTag[A]

  /**
   * Danger - could expose internal NA's
   *
   * Access an element by location. This is made private because the internal
   * representation might contain primitive NA's that need to be boxed so that
   * they aren't utilized unknowingly in calculations.
   */
  private[saddle] def apply(loc: Int): A

  /**
   * Set to true when the vec is shifted over the backing array
   */
  protected def needsCopy: Boolean = false

  // ----------
  // get values

  /**
   * Access a boxed element of a Vec[A] at a single location
   * @param loc offset into Vec
   */
  def at(loc: Int): Scalar[A] = {
    implicit val clm: ST[A] = scalarTag
    apply(loc)
  }

  /**
   * Access an unboxed element of a Vec[A] at a single location
   * @param loc offset into Vec
   */
  def raw(loc: Int): A = {
    implicit val clm = scalarTag
    apply(loc)
  }

  /**
   * Slice a Vec at a sequence of locations, e.g.
   *
   * val v = Vec(1,2,3,4,5)
   * v(1,3) == Vec(2,4)
   *
   * @param locs locations at which to slice
   */
  def apply(locs: Int*): Vec[A] = take(locs.toArray)

  /**
   * Slice a Vec at a sequence of locations, e.g.
   *
   * val v = Vec(1,2,3,4,5)
   * v(Array(1,3)) == Vec(2,4)
   *
   * @param locs locations at which to slice
   */
  def apply(locs: Array[Int]): Vec[A] = take(locs)

  /**
   * Slice a Vec at a bound of locations, e.g.
   *
   * val v = Vec(1,2,3,4,5)
   * v(1->3) == Vec(2,3,4)
   *
   * @param rng evaluates to IRange
   */
  def apply(rng: Slice[Int]): Vec[A] = {
    val idx  = new IndexIntRange(length)
    val pair = rng(idx)
    slice(pair._1, pair._2)
  }

  /**
   * Access the first element of a Vec[A], or NA if length is zero
   */
  def first: Scalar[A] = {
    implicit val clm = scalarTag
    if (length > 0) apply(0) else NA
  }

  /**
   * Access the last element of a Vec[A], or NA if length is zero
   */
  def last: Scalar[A] = {
    implicit val clm = scalarTag
    if (length > 0) apply(length - 1) else NA
  }

  // ----------

  /**
   * Return copy of backing array
   */
  def contents: Array[A] = copy.toArray

  /**
   * Return first n elements
   * @param n Number of elements to access
   */
  def head(n: Int): Vec[A] = slice(0, n)

  /**
   * Return last n elements
   * @param n Number of elements to access
   */
  def tail(n: Int): Vec[A] = slice(length - n, length)

  /**
   * True if and only if number of elements is zero
   */
  def isEmpty: Boolean = length == 0

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
  def take(locs: Array[Int]): Vec[A]

  /**
   * The complement of the take operation; slice out
   * elements NOT specified in list.
   *
   * @param locs Location of elements not to take
   */
  def without(locs: Array[Int]): Vec[A]

  /**
   * Returns Vec whose locations corresponding to true entries in the
   * boolean input mask vector are set to NA
   *
   * @param m Mask vector of Vec[Boolean]
   */
  def mask(m: Vec[Boolean]): Vec[A] = VecImpl.mask(this, m, scalarTag.missing)(scalarTag)

  /**
   * Returns Vec whose locations are NA where the result of the
   * provided function evaluates to true
   *
   * @param f A function taking an element and returning a Boolean
   */
  def mask(f: A => Boolean): Vec[A] = VecImpl.mask(this, f, scalarTag.missing)(scalarTag)

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
  def concat[B, C](v: Vec[B])(implicit wd: Promoter[A, B, C], mc: ST[C]): Vec[C]

  /**
   * Additive inverse of Vec with numeric elements
   *
   */
  def unary_-(): Vec[A]

  // Must implement specialized methods independently of specialized class, workaround to
  // https://issues.scala-lang.org/browse/SI-5281

  /**
   * Map a function over the elements of the Vec, as in scala collections library
   */
  def map[@spec(Boolean, Int, Long, Double) B: ST](f: A => B): Vec[B]

  /**
   * Left fold over the elements of the Vec, as in scala collections library
   */
  def foldLeft[@spec(Boolean, Int, Long, Double) B: ST](init: B)(f: (B, A) => B): B

  /**
   * Left scan over the elements of the Vec, as in scala collections library
   */
  def scanLeft[@spec(Boolean, Int, Long, Double) B: ST](init: B)(f: (B, A) => B): Vec[B]

  /**
   * Filtered left fold over the elements of the Vec, as in scala collections library
   */
  def filterFoldLeft[@spec(Boolean, Int, Long, Double) B: ST](pred: A => Boolean)(init: B)(f: (B, A) => B): B

  /**
   * Filtered left scan over elements of the Vec, as in scala collections library
   */
  def filterScanLeft[@spec(Boolean, Int, Long, Double) B: ST](pred: A => Boolean)(init: B)(f: (B, A) => B): Vec[B]

  /**
   * Left fold that folds only while the test condition holds true. As soon as the condition function yields
   * false, the fold returns.
   *
   * @param cond Function whose signature is the same as the fold function, except that it evaluates to Boolean
   */
  def foldLeftWhile[@spec(Boolean, Int, Long, Double) B: ST](init: B)(f: (B, A) => B)(cond: (B, A) => Boolean): B

  /**
   * Zips Vec with another Vec and applies a function to the paired elements. If either of the pair is NA, the
   * result is forced to NA.
   * @param other Vec[B]
   * @param f Function (A, B) => C
   * @tparam B Parameter of other Vec
   * @tparam C Result of function
   */
  def zipMap[@spec(Int, Long, Double) B: ST, @spec(Boolean, Int, Long, Double) C: ST](other: Vec[B])(f: (A, B) => C): Vec[C]

  /**
   * Drop the elements of the Vec which are NA
   */
  def dropNA: Vec[A]

  /**
   * Return true if there is an NA value in the Vec
   */
  def hasNA: Boolean

  /**
   * Execute a (side-effecting) operation on each (non-NA) element in the vec
   * @param op operation to execute
   */
  def foreach(op: A => Unit) { VecImpl.foreach(this)(op)(scalarTag) }

  /**
   * Execute a (side-effecting) operation on each (non-NA) element in vec which satisfies
   * some predicate.
   * @param pred Function A => Boolean
   * @param op Side-effecting function
   */
  def forall(pred: A => Boolean)(op: A => Unit) { VecImpl.forall(this)(pred)(op)(scalarTag) }

  /**
   * Return Vec of integer locations (offsets) which satisfy some predicate
   * @param pred Predicate function from A => Boolean
   */
  def find(pred: A => Boolean): Vec[Int] = VecImpl.find(this)(pred)(scalarTag)

  /**
   * Return first integer location which satisfies some predicate, or -1 if there is none
   * @param pred Predicate function from A => Boolean
   */
  def findOne(pred: A => Boolean): Int = VecImpl.findOne(this)(pred)(scalarTag)

  /**
   * Return true if there exists some element of the Vec which satisfies the predicate function
   * @param pred Predicate function from A => Boolean
   */
  def exists(pred: A => Boolean): Boolean = findOne(pred) != -1

  /**
   * Return Vec whose elements satisfy a predicate function
   * @param pred Predicate function from A => Boolean
   */
  def filter(pred: A => Boolean): Vec[A] = VecImpl.filter(this)(pred)(scalarTag)

  /**
   * Return Vec whose elements are selected via a Vec of booleans (where that Vec holds the value true)
   * @param pred Predicate vector: Vec[Boolean]
   */
  def where(pred: Vec[Boolean]): Vec[A] = VecImpl.where(this)(pred.toArray)(scalarTag)

  /**
   * Produce a Vec whose entries are the result of executing a function on a sliding window of the
   * data.
   * @param winSz Window size
   * @param f Function Vec[A] => B to operate on sliding window
   * @tparam B Result type of function
   */
  def rolling[@spec(Boolean, Int, Long, Double) B: ST](winSz: Int, f: Vec[A] => B): Vec[B]

  /**
   * Yield a Vec whose elements have been sorted (in ascending order)
   * @param ev evidence of Ordering[A]
   */
  def sorted(implicit ev: ORD[A]) = take(array.argsort(toArray)(scalarTag, ev))

  /**
   * Yield a Vec whose elements have been reversed from their original order
   */
  def reversed: Vec[A] = {
    implicit val clm = scalarTag
    Vec(array.reverse(toArray))
  }

  /**
   * Creates a view into original vector from an offset up to, but excluding,
   * another offset. Data is not copied.
   *
   * @param from Beginning offset
   * @param until One past ending offset
   * @param stride Increment within slice
   */
  def slice(from: Int, until: Int, stride: Int = 1): Vec[A]

  /**
   * Creates a view into original vector from an offset up to, and including,
   * another offset. Data is not copied.
   *
   * @param from Beginning offset
   * @param to Ending offset
   * @param stride Increment within slice
   */
  def sliceBy(from: Int, to: Int, stride: Int = 1): Vec[A] =
    slice(from, to + stride, stride)

  /**
   * Split Vec into two Vecs at position i
   * @param i Position at which to split Vec
   */
  def splitAt(i: Int): (Vec[A], Vec[A]) = (slice(0, i), slice(i, length))

  /**
   * Creates a view into original Vec, but shifted so that n
   * values at the beginning or end of the Vec are NA's. Data
   * is not copied.
   *
   * @param n Number of offsets to shift
   */
  def shift(n: Int): Vec[A]

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
  def pad: Vec[A] = VecImpl.pad(this)(scalarTag)

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
  def padAtMost(n: Int): Vec[A] = VecImpl.pad(this, n)(scalarTag)

  /**
   * Fills NA values in vector with result of a function which acts on the index of
   * the particular NA value found
   *
   * @param f A function from Int => A; yields value for NA value at ith position
   */
  def fillNA(f: Int => A): Vec[A] = VecImpl.vecfillNA(this)(f)(scalarTag)

  /**
   * Converts Vec to an indexed sequence (default implementation is immutable.Vector)
   *
   */
  def toSeq: IndexedSeq[A] = toArray.toIndexedSeq

  /**
   * Returns a Vec whose backing array has been copied
   */
  protected def copy: Vec[A]

  private[saddle] def toArray: Array[A]

  private[saddle] def toDoubleArray(implicit na: NUM[A]): Array[Double] = {
    val arr = toArray
    val buf = new Array[Double](arr.length)
    var i = 0
    while(i < arr.length) {
      buf(i) = scalarTag.toDouble(arr(i))
      i += 1
    }
    buf
  }

  /** Default hashcode is simple rolling prime multiplication of sums of hashcodes for all values. */
  override def hashCode(): Int = foldLeft(1)(_ * 31 + _.hashCode())

  /**
   * Default equality does an iterative, element-wise equality check of all values.
   *
   * NB: to avoid boxing, is overwritten in child classes
   */
  override def equals(o: Any): Boolean = o match {
    case rv: Vec[_] => (this eq rv) || (this.length == rv.length) && {
      var i = 0
      var eq = true
      while(eq && i < this.length) {
        eq &&= (apply(i) == rv(i) || this.scalarTag.isMissing(apply(i)) && rv.scalarTag.isMissing(rv(i)))
        i += 1
      }
      eq
    }
    case _ => false
  }

  /**
   * Creates a string representation of Vec
   * @param len Max number of elements to include
   */
  def stringify(len: Int = 10): String = {
    val half = len / 2

    val buf = new StringBuilder()

    val maxf = (a: Int, b: String) => math.max(a, b.length)
    def grab(arr: Array[A]) = arr.take(half) ++ arr.takeRight(half)

    val varr = toArray

    if (varr.length == 0)
      buf append "Empty Vec"
    else {
      buf.append("[%d x 1]\n" format (length))
      val vlen = grab(varr).map(scalarTag.show(_)).foldLeft(0)(maxf)
      def createRow(r: Int): String = ("%" + vlen + "s\n").format(scalarTag.show(apply(r)))
      buf append util.buildStr(len, length, createRow, " ... \n" )
    }

    buf.toString()
  }

  /**
   * Pretty-printer for Vec, which simply outputs the result of stringify.
   * @param len Number of elements to display
   */
  def print(len: Int = 10, stream: OutputStream = System.out) {
    stream.write(stringify(len).getBytes)
  }

  override def toString = stringify()
}

object Vec extends BinOpVec {
  // factory methods

  private val bc = classOf[Boolean]
  private val ic = classOf[Int]
  private val lc = classOf[Long]
  private val dc = classOf[Double]
  private val tc = classOf[DateTime]

  // todo: use typeclass pattern here for compile-time ad hoc polymorphism? better extension capabilities

  /**
   * Factory method to create a Vec from an array of elements
   *
   * @param arr Array
   * @tparam C Type of elements in array
   */
  def apply[C: ST](arr: Array[C]): Vec[C] = {
    val m = implicitly[ST[C]]
    m.runtimeClass match {
      case c if c == bc => new VecBool(arr.asInstanceOf[Array[Boolean]])
      case c if c == ic => new VecInt(arr.asInstanceOf[Array[Int]])
      case c if c == lc => new VecLong(arr.asInstanceOf[Array[Long]])
      case c if c == dc => new VecDouble(arr.asInstanceOf[Array[Double]])
      case c if c == tc => new VecTime(Vec(arr.asInstanceOf[Array[DateTime]].map(_.getMillis)))
      case _            => new VecAny(arr)
    }
  }.asInstanceOf[Vec[C]]

  /**
   * Factory method to create a Vec from a sequence of elements. For example,
   *
   * {{{
   *   Vec(1,2,3)
   *   Vec(Seq(1,2,3) : _*)
   * }}}
   *
   * @param values Sequence
   * @tparam C Type of elements in Vec
   */
  def apply[C: ST](values: C*): Vec[C] = Vec(values.toArray)

  /**
   * Creates an empty Vec of type C.
   *
   * @tparam C Vec type parameter
   */
  def empty[C: ST]: Vec[C] = Vec(Array.empty[C])

  // conversions

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
   * A Vec may be implicitly ''widened'' to a Vec.
   *
   * @param s Vec to widen to Series
   * @tparam A Type of elements in Vec
   */
  implicit def vecToSeries[A: ST](s: Vec[A]) = Series(s)

  /**
   * A Vec may be implicitly converted to a single column Mat
   */
  implicit def vecToMat[A: ST](s: Vec[A]): Mat[A] = Mat(s.length, 1, s.toArray)

  // stats functions

  type Vec2Stats[A] = Vec[A] => VecStats[A]

  /**
   * Enrich integer Vec with basic stats
   * @param s Vec[Int]
   */
  implicit def vecToIntStats(s: Vec[Int]): VecStats[Int] = new IntStats(s)

  /**
   * Enrich long Vec with basic stats
   * @param s Vec[Long]
   */
  implicit def vecToLongStats(s: Vec[Long]): VecStats[Long] = new LongStats(s)

  /**
   * Enrich double Vec with basic stats
   * @param s Vec[Double]
   */
  implicit def vecToDoubleStats(s: Vec[Double]): VecStats[Double] = new DoubleStats(s)

  /**
   * Enrich integer Vec with expanding stats
   * @param s Vec[Int]
   */
  implicit def vecToIntExpStats(s: Vec[Int]): VecExpandingStats[Int] = new IntExpandingStats(s)

  /**
   * Enrich long Vec with expanding stats
   * @param s Vec[Long]
   */
  implicit def vecToLongExpStats(s: Vec[Long]): VecExpandingStats[Long] = new LongExpandingStats(s)

  /**
   * Enrich double Vec with expanding stats
   * @param s Vec[Double]
   */
  implicit def vecToDoubleExpStats(s: Vec[Double]): VecExpandingStats[Double] = new DoubleExpandingStats(s)

  /**
   * Enrich integer Vec with rolling stats
   * @param s Vec[Int]
   */
  implicit def vecToIntRollingStats(s: Vec[Int]): VecRollingStats[Int] = new VecRollingStats[Int](s)

  /**
   * Enrich long Vec with rolling stats
   * @param s Vec[Long]
   */
  implicit def vecToLongRollingStats(s: Vec[Long]): VecRollingStats[Long] = new VecRollingStats[Long](s)

  /**
   * Enrich double Vec with rolling stats
   * @param s Vec[Double]
   */
  implicit def vecToDoubleRollingStats(s: Vec[Double]): VecRollingStats[Double] = new VecRollingStats[Double](s)

  // some pimped-on logic methods. scala.Function1 is not specialized on
  // Boolean input. not sure I care to work around this

  /**
   * Enrich Boolean Vec with logic methods
   * @param v Vec[Boolean]
   */
  implicit def vecToBoolLogic(v: Vec[Boolean]) = new {
    /**
     * True if all elements are true
     */
    def all: Boolean  = -1 == v.findOne(_ == false)

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