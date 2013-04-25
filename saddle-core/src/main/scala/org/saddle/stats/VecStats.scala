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

package org.saddle.stats

import scala.{specialized => spec}
import org.saddle._
import org.saddle.vec._
import org.saddle.scalar._

/**
 * Trait which specifies how to break a rank tie
 */
sealed trait RankTie

object RankTie {
  /**
   * Take the average of the ranks for all ties
   *
   * {{{
   *   Vec(3,6,6,4).rank(tie = stats.RankTie.Avg) == Vec[Double](1,3.5,3.5,2)
   * }}}
   */
  object Avg extends RankTie

  /**
   * Take the minimum rank for all ties
   *
   * {{{
   *   Vec(3,6,6,4).rank(tie = stats.RankTie.Min) == Vec[Double](1,3,3,2)
   * }}}
   */
  object Min extends RankTie

  /**
   * Take the maximum rank for all ties
   *
   * {{{
   *   Vec(3,6,6,4).rank(tie = stats.RankTie.Max) == Vec[Double](1,4,4,2)
   * }}}
   */
  object Max extends RankTie

  /**
   * Take the rank according to natural (input) order
   *
   * {{{
   *   Vec(3,6,6,4).rank(tie = stats.RankTie.Nat) == Vec[Double](1,3,4,2)
   * }}}
   */
  object Nat extends RankTie
}

/**
 * Trait which specifies what percentile method to use
 */
sealed trait PctMethod

object PctMethod {
  /**
   * Take percentile as MS Excel does
   */
  object Excel extends PctMethod

  /**
   * Take percentile according to [[http://www.itl.nist.gov/div898/handbook/prc/section2/prc252.htm NIST]]
   */
  object NIST extends PctMethod
}

/**
 * Statistical methods made available on numeric Vec objects via enrichment.
 */
trait VecStats[@spec(Int, Long, Double) A] {
  /**
   * Sum of the elements of the Vec, ignoring NA values
   */
  def sum: A

  /**
   * Count of the non-NA elements of the Vec
   */
  def count: Int

  /**
   * Minimum element of the Vec, if one exists, or else None
   */
  def min: Option[A]

  /**
   * Maximum element of the Vec, if one exists, or else None
   */
  def max: Option[A]

  /**
   * Integer offset of the minimum element of the Vec, if one exists, or else -1
   */
  def argmin: Int

  /**
   * Integer offset of the minimum element of the Vec, if one exists, or else -1
   */
  def argmax: Int

  /**
   * Product of all the values in the Vec, ignoring NA values
   */
  def prod: A

  /**
   * Counts the non-NA elements of the Vec subject to passing the
   * predicate function
   * @param test A function from A to Boolean
   */
  def countif(test: A => Boolean): Int

  /**
   * Return the sum of the natural log of each element, ignoring NA values
   */
  def logsum: Double

  /**
   * Return the mean (average) of the values in the Vec, ignoring NA
   */
  def mean: Double

  /**
   * Return the median of the values in the Vec, ignoring NA
   */
  def median: Double

  /**
   * Return the geometric median of the values in the Vec, ignoring NA
   */
  def geomean: Double

  /**
   * Return the sample variance of the values in the Vec, ignoring NA
   */
  def variance: Double

  /**
   * Return the sample standard deviation of values in the Vec, ignoring NA
   */
  def stdev: Double = math.sqrt(variance)

  /**
   * Return the sample skewness of the values in the Vec, ignoring NA
   */
  def skew: Double

  /**
   * Return the sample kurtosis of the values in the Vec, ignoring NA
   */
  def kurt: Double

  /**
   * Return the percentile of the values at a particular threshold, ignoring NA
   * @param tile The percentile in [0, 100] at which to compute the threshold
   * @param method The percentile method; one of [[org.saddle.stats.PctMethod]]
   */
  def percentile(tile: Double, method: PctMethod = PctMethod.NIST): Double

  /**
   * Return a copy of a numeric Vec with its values demeaned according to the
   * mean function
   */
  def demeaned: Vec[Double]

  /**
   * Return a Vec of ranks corresponding to a Vec of numeric values.
   * @param tie Method with which to break ties; a [[org.saddle.stats.RankTie]]
   * @param ascending Boolean, default true, whether to give lower values larger rank
   */
  def rank(tie: RankTie = RankTie.Avg, ascending: Boolean = true): Vec[Double]

  protected def _variance(r: Vec[A], subOp: (A, Double) => Double): Double = {
    val sa = r.scalarTag
    val sd = ScalarTagDouble
    val c  = count

    if (c < 1)
      sd.missing
    else if (c == 1)
      0.0
    else {
      val m: Double = mean
      r.filterFoldLeft(sa.notMissing)(0d) { (x, y) =>
        val tmp = subOp(y, m)
        x + tmp * tmp / (c - 1.0)
      }
    }
  }

  protected def _skew(r: Vec[A], subOp: (A, Double) => Double): Double = {
    val sa = r.scalarTag
    val sd = ScalarTagDouble
    val c  = count

    if (c > 2) {
      val v: Double = variance
      val m: Double = mean
      val coef = c / ((c - 1) * (c - 2) * v * math.sqrt(v))
      r.filterFoldLeft(sa.notMissing)(0d) { (x, y) =>
        val tmp = subOp(y, m)
        x + coef * tmp * tmp * tmp
      }
    }
    else sd.missing
  }

  protected def _kurt(r: Vec[A], subOp: (A, Double) => Double): Double = {
    val sa = r.scalarTag
    val sd = ScalarTagDouble
    val c: Double = count

    if (c > 3) {
      val vari = variance
      val m: Double = mean
      val acacc = r.filterFoldLeft(sa.notMissing)(0d) { (x, y) =>
        val tmp = subOp(y, m)
        x + (tmp * tmp * tmp * tmp) / (vari * vari)
      }
      val coef1 = (c * (c + 1)) / ((c - 1) * (c - 2) * (c - 3))
      val coef2 = (c - 1) * (c - 1) / ((c - 2) * (c - 3))
      (coef1 * acacc - 3.0 * coef2)
    }
    else sd.missing
  }

 protected def _demeaned(r: Vec[A], subOp: (A, Double) => Double): Vec[Double] = {
    val sa = r.scalarTag
    val sd = ScalarTagDouble

    val mn = mean
    val ar = Array.ofDim[Double](r.length)
    var i = 0
    while( i < r.length ) {
      val v = r(i)
      if (sa.notMissing(v))
        ar(i) = subOp(r(i), mn)
      else
        ar(i) = sd.missing
      i += 1
    }
    new VecDouble(ar)
  }

  // Fast median function that is N/A friendly; destructive to array
  protected def _median(r: Vec[A])(implicit n: NUM[A]): Double = {
    val sd = ScalarTagDouble

    def _arrCopyToDblArr(r: Vec[A])(implicit n: NUM[A]): (Int, Array[Double]) = {
      val arr = Array.ofDim[Double](r.length)
      val sa = r.scalarTag
      var i = 0
      var j = 0
      while(i < r.length) {
        val v = sa.toDouble(r(i))
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

  // NB: destructive to argument v
  protected def _rank(v: Array[Double], tie: RankTie, ascending: Boolean): Vec[Double] = {
    val sd = ScalarTagDouble

    val nan = if (ascending) Double.PositiveInfinity else Double.NegativeInfinity
    val len = v.length

    var k = 0
    while (k < len) {
      if (sd.isMissing(v(k))) v(k) = nan
      k += 1
    }

    val srt = if (ascending)
      array.argsort(v)
    else
      array.reverse(array.argsort(v))

    val dat = array.take(v, srt, 0.0)

    var i = 0
    var s = 0.0   // summation
    var d = 0     // duplicate counter
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
        }
        else if (tie == RankTie.Min) {
          var j = i - d + 1
          while (j < i + 1) {
            res(srt(j)) = i - d + 2
            j += 1
          }
        }
        else if (tie == RankTie.Max) {
          var j = i - d + 1
          while (j < i + 1) {
            res(srt(j)) = i + 1
            j += 1
          }
        }
        else if (tie == RankTie.Nat && ascending) {
          var j = i - d + 1
          while (j < i + 1) {
            res(srt(j)) = j + 1
            j += 1
          }
        }
        else {
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

  // percentile function: see: http://en.wikipedia.org/wiki/Percentile
  protected def _percentile(v: Vec[Double], tile: Double, method: PctMethod)(implicit n: NUM[A]): Double = {
    val sd = ScalarTagDouble
    val vf = v.dropNA
    if (vf.length == 0 || tile < 0 || tile > 100)
      sd.missing
    else {
      val c = vf.length
      if (c == 1) vf(0)
      else {
        val n = method match {
          case PctMethod.Excel => (tile / 100.0) * (c - 1.0) + 1.0
          case PctMethod.NIST  => (tile / 100.0) * (c + 1.0)
        }
        val s = vf.sorted
        val k = math.floor(n).toInt
        val d = n - k
        if (k <= 0) s(0) else if (k >= c) s.last else s(k - 1) + d * (s(k) - s(k-1))
      }
    }
  }
}

class DoubleStats(r: Vec[Double]) extends VecStats[Double] {
  val sd = ScalarTagDouble

  def sum: Double = r.filterFoldLeft(sd.notMissing)(0d)(_ + _)
  def count: Int = r.filterFoldLeft(sd.notMissing)(0)((a, b) => a + 1)

  def min: Option[Double] =
    if (r.count == 0)
      None
    else {
      val res = r.filterFoldLeft(sd.notMissing)(sd.inf)((x: Double, y: Double) => if (x < y) x else y)
      Some(res)
    }

  def max: Option[Double] =
    if (r.count == 0)
      None
    else {
      val res: Double = r.filterFoldLeft(sd.notMissing)(sd.negInf)((x: Double, y: Double) => if (x > y) x else y)
      Some(res)
    }

  def prod: Double = r.filterFoldLeft(sd.notMissing)(1d)(_ * _)
  def countif(test: Double => Boolean): Int = r.filterFoldLeft(t => sd.notMissing(t) && test(t))(0)((a,b) => a + 1)
  def logsum: Double = r.filterFoldLeft(sd.notMissing)(0d)((x, y) => x + math.log(y))
  def mean: Double = sum / count
  def median: Double = _median(r)
  def geomean: Double = math.exp(logsum / count)
  def variance: Double = _variance(r, _ - _)
  def skew: Double = _skew(r, _ - _)
  def kurt: Double = _kurt(r, _ - _)
  def percentile(tile: Double, method: PctMethod = PctMethod.NIST): Double = _percentile(r, tile, method)

  def demeaned: Vec[Double] = _demeaned(r, _ - _)
  def rank(tie: RankTie = RankTie.Avg, ascending: Boolean = true): Vec[Double] = _rank(r.contents, tie, ascending)

  def argmin: Int = array.argmin(r.toArray)
  def argmax: Int = array.argmax(r.toArray)
}

class IntStats(r: Vec[Int]) extends VecStats[Int] {
  val si = ScalarTagInt

  def min: Option[Int] =
    if (r.count == 0) None
    else {
      val res: Int = r.filterFoldLeft(si.notMissing)(si.inf)((x: Int, y: Int) => if (x < y) x else y)
      Some(res)
    }

  def max: Option[Int] =
    if (r.count == 0) None
    else {
      val res: Int = r.filterFoldLeft(si.notMissing)(si.negInf)((x: Int, y: Int) => if (x > y) x else y)
      Some(res)
    }

  def sum: Int = r.filterFoldLeft(si.notMissing)(0)(_ + _)
  def count: Int = r.filterFoldLeft(si.notMissing)(0)((a, b) => a + 1)
  def prod: Int = r.filterFoldLeft(si.notMissing)(1)(_ * _)
  def countif(test: Int => Boolean): Int = r.filterFoldLeft(t => si.notMissing(t) && test(t))(0)((a,b) => a + 1)
  def logsum: Double = r.filterFoldLeft(si.notMissing)(0d)((x, y) => x + math.log(y.asInstanceOf[Double]))
  def mean: Double = sum.asInstanceOf[Double] / count
  def median: Double = _median(r)
  def geomean: Double = math.exp(logsum / count)
  def variance: Double = _variance(r, _ - _)
  def skew: Double = _skew(r, _ - _)
  def kurt: Double = _kurt(r, _ - _)
  def percentile(tile: Double, method: PctMethod = PctMethod.NIST): Double = _percentile(r.toDoubleArray, tile, method)

  def demeaned: Vec[Double] = _demeaned(r, _ - _)
  def rank(tie: RankTie = RankTie.Avg, ascending: Boolean = true): Vec[Double] = _rank(r.toDoubleArray, tie, ascending)

  def argmin: Int = array.argmin(r.toArray)
  def argmax: Int = array.argmax(r.toArray)
}

class LongStats(r: Vec[Long]) extends VecStats[Long] {
  val sl = ScalarTagLong

  def min: Option[Long] =
    if (r.count == 0) None
    else {
      val res: Long = r.filterFoldLeft(sl.notMissing)(sl.inf)((x: Long, y: Long) => if (x < y) x else y)
      Some(res)
    }

  def max: Option[Long] =
    if (r.count == 0) None
    else {
      val res: Long = r.filterFoldLeft(sl.notMissing)(sl.negInf)((x: Long, y: Long) => if (x > y) x else y)
      Some(res)
    }

  def sum: Long = r.filterFoldLeft(sl.notMissing)(0L)(_ + _)
  def count: Int = r.filterFoldLeft(sl.notMissing)(0)((a, b) => a + 1)
  def prod: Long = r.filterFoldLeft(sl.notMissing)(1L)(_ * _)
  def countif(test: Long => Boolean): Int = r.filterFoldLeft(t => sl.notMissing(t) && test(t))(0)((a,b) => a + 1)
  def logsum: Double = r.filterFoldLeft(sl.notMissing)(0d)((x, y) => x + math.log(y))
  def mean: Double = sum.asInstanceOf[Double] / count
  def median: Double = _median(r)
  def geomean: Double = math.exp(logsum / count)
  def variance: Double = _variance(r, _ - _)
  def skew: Double = _skew(r, _ - _)
  def kurt: Double = _kurt(r, _ - _)
  def percentile(tile: Double, method: PctMethod = PctMethod.NIST): Double = _percentile(r.toDoubleArray, tile, method)

  def demeaned: Vec[Double] = _demeaned(r, _ - _)
  def rank(tie: RankTie = RankTie.Avg, ascending: Boolean = true): Vec[Double] = _rank(r.toDoubleArray, tie, ascending)

  def argmin: Int = array.argmin(r.toArray)
  def argmax: Int = array.argmax(r.toArray)
}

