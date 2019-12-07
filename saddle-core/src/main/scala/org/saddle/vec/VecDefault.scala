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
package org.saddle.vec

import scala.{specialized => spec}
import org.saddle.scalar.{Scalar, NA, ScalarTagDouble}
import org.saddle.ops.NumericOps
import org.saddle.{ST, Vec, array, NUM, util, ORD, PctMethod, RankTie, Mat}
import org.saddle.index.Slice
import org.saddle.index.IndexIntRange
import java.io.OutputStream

class VecDefault[@spec(Boolean, Int, Long, Double) T](
    values: Array[T],
    val scalarTag: ST[T]
) extends NumericOps[Vec[T]]
    with Vec[T] { self =>
  implicit private[this] def st: ST[T] = scalarTag

  /**
    * Set to true when the vec is shifted over the backing array
    * false iff the backing array is a contiguous sequence of the elements of this Vec
    * false iff 0 until length map raw toArray structurally equals the backing array
    */
  override def needsCopy: Boolean = false

  /**
    * The number of elements in the container                                                  F
    */
  def length = values.length

  /**
    * Access an unboxed element of a Vec[A] at a single location
    * @param loc offset into Vec
    */
  def raw(loc: Int): T = values(loc)

  /** Returns an array containing the elements of this Vec in contiguous order
    *
    * May or may not return the backing array, therefore mutations to the returned array
    * may or may not are visible to this Vec
    *
    * If `needsCopy` is false then it returns the backing array
    * If `needsCopy` is true then the backing array is not contiguous
    */
  def toArray: Array[T] = {
    // need to check if we're a view on an array
    if (!needsCopy)
      values
    else {
      val buf = new Array[T](length)
      val n = length
      var i = 0
      while (i < n) {
        buf(i) = raw(i)
        i += 1
      }
      buf
    }
  }

  /**
    * Returns a Vec whose backing array has been copied
    */
  def copy: Vec[T] = Vec(this.contents)

  /**
    * Return copy of backing array
    */
  def contents: Array[T] = if (needsCopy) toArray else toArray.clone()

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
  def take(locs: Array[Int]): Vec[T] =
    Vec(array.take[T](toArray, locs, scalarTag.missing))

  /**
    * The complement of the take operation; slice out
    * elements NOT specified in list.
    *
    * @param locs Location of elements not to take
    */
  def without(locs: Array[Int]): Vec[T] = Vec(array.remove(toArray, locs))

  /**
    * Drop the elements of the Vec which are NA
    */
  def dropNA: Vec[T] = filter(_ => true)

  /**
    * Return true if there is an NA value in the Vec
    */
  def hasNA: Boolean = VecImpl.findOneNA(this)

  /**
    * Additive inverse of Vec with numeric elements
    *
    */
  def unary_-()(implicit num: NUM[T]): Vec[T] = map(num.negate)

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
  def concat(v: Vec[T]): Vec[T] =
    Vec(util.Concat.append(toArray, v.toArray))

  /**
    * Left fold over the elements of the Vec, as in scala collections library
    */
  def foldLeft[@spec(Boolean, Int, Long, Double) B: ST](
      init: B
  )(f: (B, T) => B): B =
    VecImpl.foldLeft(this)(init)(f)

  /**
    * Left fold that folds only while the test condition holds true. As soon as the condition function yields
    * false, the fold returns.
    *
    * @param cond Function whose signature is the same as the fold function, except that it evaluates to Boolean
    */
  def foldLeftWhile[@spec(Boolean, Int, Long, Double) B: ST](
      init: B
  )(f: (B, T) => B)(cond: (B, T) => Boolean): B =
    VecImpl.foldLeftWhile(this)(init)(f)(cond)

  /**
    * Filtered left fold over the elements of the Vec, as in scala collections library
    */
  def filterFoldLeft[@spec(Boolean, Int, Long, Double) B: ST](
      pred: T => Boolean
  )(init: B)(f: (B, T) => B): B =
    VecImpl.filterFoldLeft(this)(pred)(init)(f)

  /**
    * Filtered left scan over elements of the Vec, as in scala collections library
    */
  def filterScanLeft[@spec(Boolean, Int, Long, Double) B: ST](
      pred: T => Boolean
  )(init: B)(f: (B, T) => B): Vec[B] =
    VecImpl.filterScanLeft(this)(pred)(init)(f)

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
  ): Vec[B] =
    VecImpl.rolling(this)(winSz, f)

  /**
    * Map a function over the elements of the Vec, as in scala collections library
    */
  def map[@spec(Boolean, Int, Long, Double) B: ST](f: T => B): Vec[B] =
    VecImpl.map(this)(f)

  /**
    * Maps a function over elements of the Vec and flattens the result.
    */
  def flatMap[@spec(Boolean, Int, Long, Double) B: ST](f: T => Vec[B]): Vec[B] =
    VecImpl.flatMap(this)(f)

  /**
    * Left scan over the elements of the Vec, as in scala collections library
    */
  def scanLeft[@spec(Boolean, Int, Long, Double) B: ST](init: B)(
      f: (B, T) => B
  ): Vec[B] = VecImpl.scanLeft(this)(init)(f)

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
  ](other: Vec[B])(f: (T, B) => C): Vec[C] =
    VecImpl.zipMap(this, other)(f)

  /**
    * Creates a view into original vector from an offset up to, but excluding,
    * another offset. Data is not copied.
    *
    * @param from Beginning offset
    * @param until One past ending offset
    * @param stride Increment within slice
    */
  def slice(from: Int, until: Int, stride: Int = 1) = {
    val b = math.max(from, 0)
    val e = math.min(until, self.length)

    if (e <= b) Vec.empty
    else
      new VecDefault(values, scalarTag) {
        private val ub = math.min(self.length, e)

        override def length =
          org.saddle.util.dividePositiveRoundUp(ub - b, stride)

        override def raw(i: Int): T = {
          val loc = b + i * stride
          if (loc >= ub)
            throw new ArrayIndexOutOfBoundsException(
              "Cannot access location %d >= length %d".format(loc, ub)
            )
          self.raw(loc)
        }

        override def needsCopy = true

        override def update(offset: Int, value: T) = {
          val loc = b + offset * stride
          if (loc >= ub)
            throw new ArrayIndexOutOfBoundsException(
              "Cannot access location %d >= length %d".format(loc, ub)
            )
          self.update(loc, value)
        }

      }
  }

  /**
    * Creates a view into original Vec, but shifted so that n
    * values at the beginning or end of the Vec are NA's. Data
    * is not copied.
    * ex. shift(1)  : [1 2 3 4] => [NA 1 2 3]
    *     shift(-1) : [1 2 3 4] => [2 3 4 NA]
    *
    * @param n Number of offsets to shift
    */
  def shift(n: Int) = {
    val m = math.min(n, self.length)
    val b = -m
    val e = self.length - m

    new VecDefault(values, scalarTag) {
      override def length = self.length

      override def raw(i: Int): T = {
        val loc = b + i
        if (loc >= e || loc < b)
          throw new ArrayIndexOutOfBoundsException(
            "Cannot access location %d (vec length %d)".format(i, self.length)
          )
        else if (loc >= self.length || loc < 0)
          scalarTag.missing
        else
          self.raw(loc)
      }

      override def update(offset: Int, value: T) = {
        val loc = b + offset
        if (loc >= e || loc < b)
          throw new ArrayIndexOutOfBoundsException(
            "Cannot access location %d (vec length %d)"
              .format(offset, self.length)
          )
        self.update(loc, value)
      }

      override def needsCopy = true
    }
  }

  /**
    * Access a boxed element of a Vec[A] at a single location
    * @param loc offset into Vec
    */
  def at(loc: Int): Scalar[T] = {
    Scalar(raw(loc))(scalarTag)
  }

  /**
    * Slice a Vec at a sequence of locations, e.g.
    *
    * val v = Vec(1,2,3,4,5)
    * v(1,3) == Vec(2,4)
    *
    * @param locs locations at which to slice
    */
  def apply(locs: Int*): Vec[T] = take(locs.toArray)

  def apply(locs: Vec[Int]): Vec[T] = take(locs.toArray)

  /**
    * Slice a Vec at a sequence of locations, e.g.
    *
    * val v = Vec(1,2,3,4,5)
    * v(Array(1,3)) == Vec(2,4)
    *
    * @param locs locations at which to slice
    */
  def apply(locs: Array[Int]): Vec[T] = take(locs)

  /**
    * Slice a Vec at a bound of locations, e.g.
    *
    * val v = Vec(1,2,3,4,5)
    * v(1->3) == Vec(2,3,4)
    *
    * @param rng evaluates to IRange
    */
  def apply(rng: Slice[Int]): Vec[T] = {
    val idx = new IndexIntRange(length)
    val pair = rng(idx)
    slice(pair._1, pair._2)
  }

  /**
    * Access the first element of a Vec[A], or NA if length is zero
    */
  def first: Scalar[T] = {
    if (length > 0) Scalar(raw(0))(scalarTag) else NA
  }

  /**
    * Access the last element of a Vec[A], or NA if length is zero
    */
  def last: Scalar[T] = {
    if (length > 0) Scalar(raw(length - 1))(scalarTag) else NA
  }

  /**
    * Return first n elements
    * @param n Number of elements to access
    */
  def head(n: Int): Vec[T] = slice(0, n)

  /**
    * Return last n elements
    * @param n Number of elements to access
    */
  def tail(n: Int): Vec[T] = slice(length - n, length)

  /**
    * True if and only if number of elements is zero
    */
  def isEmpty: Boolean = length == 0

  /**
    * Returns Vec whose locations corresponding to true entries in the
    * boolean input mask vector are set to NA
    *
    * @param m Mask vector of Vec[Boolean]
    */
  def mask(m: Vec[Boolean]): Vec[T] =
    VecImpl.mask(this, m, scalarTag.missing)(scalarTag)

  /**
    * Returns Vec whose locations are NA where the result of the
    * provided function evaluates to true
    *
    * @param f A function taking an element and returning a Boolean
    */
  def mask(f: T => Boolean): Vec[T] =
    VecImpl.mask(this, f, scalarTag.missing)(scalarTag)

  /**
    * Execute a (side-effecting) operation on each (non-NA) element in the vec
    * @param op operation to execute
    */
  def foreach(op: T => Unit): Unit = { VecImpl.foreach(this)(op)(scalarTag) }

  /**
    * Execute a (side-effecting) operation on each (non-NA) element in vec which satisfies
    * some predicate.
    * @param pred Function A => Boolean
    * @param op Side-effecting function
    */
  def forall(pred: T => Boolean)(op: T => Unit): Unit =
    VecImpl.forall(this)(pred)(op)(scalarTag)

  /**
    * Return Vec of integer locations (offsets) which satisfy some predicate
    * @param pred Predicate function from A => Boolean
    */
  def find(pred: T => Boolean): Vec[Int] = VecImpl.find(this)(pred)(scalarTag)

  /**
    * Return first integer location which satisfies some predicate, or -1 if there is none
    * @param pred Predicate function from A => Boolean
    */
  def findOne(pred: T => Boolean): Int = VecImpl.findOne(this)(pred)(scalarTag)

  /**
    * Return true if there exists some element of the Vec which satisfies the predicate function
    * @param pred Predicate function from A => Boolean
    */
  def exists(pred: T => Boolean): Boolean = findOne(pred) != -1

  /**
    * Return Vec whose elements satisfy a predicate function
    * @param pred Predicate function from A => Boolean
    */
  def filter(pred: T => Boolean): Vec[T] = VecImpl.filter(this)(pred)(scalarTag)

  /**
    * Return vec whose offets satisfy a predicate function
    * @param pred Predicate function from Int => Boolean
    */
  def filterAt(pred: Int => Boolean): Vec[T] =
    VecImpl.filterAt(this)(pred)(scalarTag)

  /**
    * Return Vec whose elements are selected via a Vec of booleans (where that Vec holds the value true)
    * @param pred Predicate vector: Vec[Boolean]
    */
  def where(pred: Vec[Boolean]): Vec[T] =
    VecImpl.where(this)(pred.toArray)(scalarTag)

  /**
    * Yield a Vec whose elements have been sorted (in ascending order)
    * @param ev evidence of Ordering[A]
    */
  def sorted(implicit ev: ORD[T], st: ST[T]) = take(array.argsort(toArray))

  /**
    * Yield a Vec whose elements have been reversed from their original order
    */
  def reversed: Vec[T] =
    Vec(array.reverse(toArray))

  /**
    * Creates a view into original vector from an offset up to, and including,
    * another offset. Data is not copied.
    *
    * @param from Beginning offset
    * @param to Ending offset
    * @param stride Increment within slice
    */
  def sliceBy(from: Int, to: Int, stride: Int = 1): Vec[T] =
    slice(from, to + stride, stride)

  /**
    * Split Vec into two Vecs at position i
    * @param i Position at which to split Vec
    */
  def splitAt(i: Int): (Vec[T], Vec[T]) = (slice(0, i), slice(i, length))

  /**
    * Fills NA values in vector with result of a function which acts on the index of
    * the particular NA value found
    *
    * @param f A function from Int => A; yields value for NA value at ith position
    */
  def fillNA(f: Int => T): Vec[T] = VecImpl.vecfillNA(this)(f)(scalarTag)

  /**
    * Converts Vec to an indexed sequence (default implementation is immutable.Vector)
    *
    */
  def toSeq: IndexedSeq[T] = toArray.toIndexedSeq

  /* Min of numeric elements, ignoring NAs */
  def min(implicit na: NUM[T], st: ST[T]): Scalar[T] =
    if (count == 0) NA
    else {
      val res: T =
        VecImpl.filterFoldLeft(this)(st.notMissing)(st.inf)((x: T, y: T) =>
          if (na.lt(x, y)) x else y
        )
      Scalar(res)
    }

  /* Max of numeric elements, ignoring NAs */
  def max(implicit na: NUM[T], st: ST[T]): Scalar[T] =
    if (count == 0) NA
    else {
      val res: T =
        VecImpl.filterFoldLeft(this)(st.notMissing)(st.negInf)((x: T, y: T) =>
          if (na.gt(x, y)) x else y
        )
      Scalar(res)
    }

  /** Sums up the elements of a numeric Vec
    *
    * NOTE: scalac only specialized correctly if using the method in VecImpl
    * referring to this.filterFoldLeft boxes
    */
  def sum(implicit na: NUM[T], st: ST[T]): T =
    VecImpl.filterFoldLeft(this)(st.notMissing)(st.zero)((a, b) => na.plus(a, b)
    )

  def prod(implicit na: NUM[T], st: ST[T]): T =
    VecImpl.filterFoldLeft(this)(st.notMissing)(st.one)((a, b) => na.times(a, b)
    )

  /**
    * Integer offset of the minimum element of the Vec, if one exists, or else -1
    */
  def argmin(implicit na: NUM[T], st: ST[T], ord: ORD[T]): Int =
    array.argmin(toArray)

  /**
    * Integer offset of the minimum element of the Vec, if one exists, or else -1
    */
  def argmax(implicit na: NUM[T], st: ST[T], ord: ORD[T]): Int =
    array.argmax(toArray)

  /** Counts the number of non-NA elements
    */
  def count: Int =
    VecImpl.filterFoldLeft(this)(scalarTag.notMissing)(0)((a, _) => a + 1)

  /** Counts the number of non-NA elements satisfying the predicate
    */
  def countif(test: T => Boolean): Int =
    VecImpl.filterFoldLeft(this)(t => scalarTag.notMissing(t) && test(t))(0)(
      (a, _) => a + 1
    )

  /** Counts the number of elements which equal `a`
    */
  def countif(a: T): Int =
    VecImpl.filterFoldLeft(this)(t => t == a)(0)((a, _) => a + 1)

  /**
    * Return the percentile of the values at a particular threshold, ignoring NA
    * @param tile The percentile in [0, 100] at which to compute the threshold
    * @param method The percentile method; one of [[org.saddle.PctMethod]]
    *
    * percentile function: see: http://en.wikipedia.org/wiki/Percentile
    */
  def percentile(tile: Double, method: PctMethod = PctMethod.NIST)(
      implicit na: NUM[T]
  ): Double = {
    val vf = Vec(dropNA.toDoubleArray)
    if (vf.length == 0 || tile < 0 || tile > 100)
      ScalarTagDouble.missing
    else {
      val c = vf.length
      if (c == 1) vf.raw(0)
      else {
        val n = method match {
          case PctMethod.Excel => (tile / 100.0) * (c - 1.0) + 1.0
          case PctMethod.NIST  => (tile / 100.0) * (c + 1.0)
        }
        val s = vf.sorted
        val k = math.floor(n).toInt
        val d = n - k
        if (k <= 0) s.raw(0)
        else if (k >= c) s.last
        else s.raw(k - 1) + d * (s.raw(k) - s.raw(k - 1))
      }
    }
  }

  /**
    * Return a Vec of ranks corresponding to a Vec of numeric values.
    * @param tie Method with which to break ties; a [[org.saddle.RankTie]]
    * @param ascending Boolean, default true, whether to give lower values larger rank
    */
  def rank(tie: RankTie = RankTie.Avg, ascending: Boolean = true)(
      implicit na: NUM[T]
  ): Vec[Double] = {
    _rank(copy.toDoubleArray, tie, ascending)
  }

  // destructive to argument
  private def _rank(
      v: Array[Double],
      tie: RankTie,
      ascending: Boolean
  ): Vec[Double] = {
    val sd = ScalarTagDouble

    val nan =
      if (ascending) Double.PositiveInfinity else Double.NegativeInfinity
    val len = v.length

    var k = 0
    while (k < len) {
      if (sd.isMissing(v(k))) v(k) = nan
      k += 1
    }

    val srt =
      if (ascending)
        array.argsort(v)
      else
        array.reverse(array.argsort(v))

    val dat = array.take(v, srt, 0.0)

    var i = 0
    var s = 0.0 // summation
    var d = 0 // duplicate counter
    val res = array.empty[Double](len)
    while (i < len) {
      val v = dat(i)

      s += (i + 1.0)
      d += 1
      if (v == nan)
        res(srt(i)) = sd.missing
      else if (i == len - 1 || math.abs(dat(i + 1) - v) > 1e-13) {
        if (tie == RankTie.Avg) {
          var j = i - d + 1
          while (j < i + 1) {
            res(srt(j)) = s / d
            j += 1
          }
        } else if (tie == RankTie.Min) {
          var j = i - d + 1
          while (j < i + 1) {
            res(srt(j)) = i - d + 2
            j += 1
          }
        } else if (tie == RankTie.Max) {
          var j = i - d + 1
          while (j < i + 1) {
            res(srt(j)) = i + 1
            j += 1
          }
        } else if (tie == RankTie.Nat && ascending) {
          var j = i - d + 1
          while (j < i + 1) {
            res(srt(j)) = j + 1
            j += 1
          }
        } else {
          var j = i - d + 1
          while (j < i + 1) {
            res(srt(j)) = 2 * i - j - d + 2
            j += 1
          }
        }
        s = 0.0
        d = 0
      }
      i += 1
    }

    Vec(res)
  }

  def mean(implicit n: NUM[T]): Double = n.toDouble(sum) / count.toDouble

  def median(implicit n: NUM[T]) = _median(this)

  private def _median(r: Vec[T])(implicit n: NUM[T]): Double = {
    val sd = ScalarTagDouble

    def _arrCopyToDblArr(
        r: Vec[T]
    ): (Int, Array[Double]) = {
      val arr = Array.ofDim[Double](r.length)
      val sa = r.scalarTag
      var i = 0
      var j = 0
      while (i < r.length) {
        val v = sa.toDouble(r.raw(i))
        if (v == v) {
          arr(j) = v
          j += 1
        }
        i += 1
      }
      (j, arr)
    }

    val (len, arr) = _arrCopyToDblArr(r)

    if (len == 0)
      sd.missing
    else if (len % 2 != 0)
      _kSmallest(arr, len, len / 2)
    else
      (_kSmallest(arr, len, len / 2) + _kSmallest(arr, len, len / 2 - 1)) / 2d
  }

  // Find k_th smallest element,taken from N.Worth via pandas python library
  // (moments.pyx). Destructive to array input and not N/A friendly
  private def _kSmallest(a: Array[Double], n: Int, k: Int): Double = {
    var l = 0
    var m = n - 1

    while (l < m) {
      val x = a(k)
      var i = l
      var j = m
      while (i <= j) {
        while (a(i) < x) i += 1
        while (a(j) > x) j -= 1
        if (i <= j) {
          val t = a(i)
          a(i) = a(j)
          a(j) = t
          i += 1
          j -= 1
        }
      }
      if (j < k) l = i
      if (k < i) m = j
    }
    a(k)
  }

  private[saddle] def toDoubleArray(implicit na: NUM[T]): Array[Double] = {
    if (scalarTag == ScalarTagDouble) toArray.asInstanceOf[Array[Double]]
    else {
      val arr = toArray
      val buf = new Array[Double](arr.length)
      var i = 0
      while (i < arr.length) {
        buf(i) = scalarTag.toDouble(arr(i))
        i += 1
      }
      buf
    }
  }

  /** Default hashcode is simple rolling prime multiplication of sums of hashcodes for all values. */
  override def hashCode(): Int = foldLeft(1)(_ * 31 + _.hashCode())

  /**
    * Default equality does an iterative, element-wise equality check of all values.
    *
    */
  override def equals(o: Any): Boolean = o match {
    case rv: Vec[_] =>
      (this eq rv) || (this.length == rv.length) && {
        var i = 0
        var eq = true
        while (eq && i < this.length) {
          eq &&= (raw(i) == rv.raw(i) || this.scalarTag
            .isMissing(raw(i)) && rv.scalarTag.isMissing(rv.raw(i)))
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

    if (length == 0)
      buf append "Empty Vec"
    else {
      buf.append("[%d x 1]\n" format (length))
      val vlen = { head(half) concat tail(half) }
        .map(scalarTag.show(_))
        .foldLeft(0)(maxf)

      def createRow(r: Int): String =
        ("%" + { if (vlen > 0) vlen else 1 } + "s\n")
          .format(scalarTag.show(raw(r)))
      buf append util.buildStr(len, length, createRow, " ... \n")
    }

    buf.toString()
  }

  /**
    * Pretty-printer for Vec, which simply outputs the result of stringify.
    * @param len Number of elements to display
    */
  def print(len: Int = 10, stream: OutputStream = System.out) = {
    stream.write(stringify(len).getBytes)
  }

  override def toString = stringify()

  /**
    * Rounds elements in the vec (which must be numeric) to
    * a significance level
    *
    * @param sig Significance level to round to (e.g., 2 decimal places)
    */
  def roundTo(sig: Int = 2)(implicit ev: NUM[T]): Vec[Double] = {
    val pwr = math.pow(10, sig)
    val rounder = (x: T) => math.round(scalarTag.toDouble(x) * pwr) / pwr
    map(rounder)
  }

  /** Returns a new Vec with the value at `offset` set to `value
    *
    * Copies before mutating.
    */
  def updated(offset: Int, value: T): Vec[T] = {
    val ar = copy.toArray
    ar(offset) = value
    Vec(ar)
  }

  /** Returns a new Vec with the value at `offset` set to `value
    *
    * Copies before mutating.
    * Ignores invalid offsets in the array
    */
  def updated(offset: Array[Int], value: T): Vec[T] = {
    val ar = copy.toArray
    var i = 0
    val n = offset.size
    while (i < n) {
      val j = offset(i)
      if (j < length) {
        ar(j) = value
      }
      i += 1
    }
    Vec(ar)
  }

  /**
    * Returns a Vec with the first `i` elements
    */
  def takeLeft(i: Int) = take(array.range(0, math.min(i, length)))

  /**
    * Returns a Vec with the last `i` elements
    */
  def takeRight(i: Int) = take(array.range(math.max(0, length - i), length))

  /**
    * Returns a Vec with the first `i` elements removed
    */
  def dropLeft(i: Int) = take(array.range(math.min(i, length), length))

  /**
    * Returns a Vec with the last `i` elements removed
    */
  def dropRight(i: Int) = take(array.range(0, math.max(0, length - i)))

  @inline def update(offset: Int, value: T) =
    if (needsCopy) throw new RuntimeException("Update not implemented")
    else {
      values(offset) = value
    }

  def update(slice: Slice[Int], value: T) = {
    val (from, until) = slice(IndexIntRange(length))
    var i = from
    while (i < until) {
      update(i, value)
      i += 1
    }
  }

  def update(slice: Slice[Int], value: Vec[T]) = {
    val (from, until) = slice(IndexIntRange(length))
    if (until - from != value.length)
      throw new RuntimeException(
        s"Must have the correct length (slice: ${until - from}, argument: ${value.length})"
      )
    var i = from
    while (i < until) {
      update(i, value.raw(i - from))
      i += 1
    }
  }

  def reshape(rows: Int, columns: Int): Mat[T] = {
    if (rows * columns != length) throw new RuntimeException("Invalid size")
    else {
      Mat(rows, columns, toArray)
    }
  }

}
