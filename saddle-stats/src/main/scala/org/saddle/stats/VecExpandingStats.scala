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

import org.saddle.Vec

import scala.{specialized => spec}
import org.saddle.scalar._

/**
  * Expanding statistical methods made available on numeric Vec objects via enrichment.
  * These methods scan over the Vec and compute cumulative values.
  */
trait VecExpandingStats[@spec(Int, Long, Double) A] {

  /**
    * Cumulative sum; each successive element of the output is the cumulative
    * sum from the initial element, ignoring NAs.
    */
  def cumSum: Vec[A]

  /**
    * Cumulative count; each successive element of the output is the cumulative
    * count from the initial element, ignoring NAs.
    */
  def cumCount: Vec[Int]

  /**
    * Cumulative min; each successive element of the output is the cumulative
    * min from the initial element, ignoring NAs.
    */
  def cumMin: Vec[A]

  /**
    * Cumulative max; each successive element of the output is the cumulative
    * max from the initial element, ignoring NAs.
    */
  def cumMax: Vec[A]

  /**
    * Cumulative product; each successive element of the output is the cumulative
    * product from the initial element, ignoring NAs.
    */
  def cumProd: Vec[A]
}

class DoubleExpandingStats(r: Vec[Double]) extends VecExpandingStats[Double] {
  private val sd = ScalarTagDouble

  def cumSum: Vec[Double] = r.filterScanLeft(sd.notMissing)(0d)(_ + _)
  def cumCount: Vec[Int] = r.filterScanLeft(sd.notMissing)(0)((a, _) => a + 1)
  def cumMin: Vec[Double] =
    r.filterScanLeft(sd.notMissing)(sd.inf)((x: Double, y: Double) =>
      if (x < y) x else y
    )
  def cumMax: Vec[Double] =
    r.filterScanLeft(sd.notMissing)(sd.negInf)((x: Double, y: Double) =>
      if (x > y) x else y
    )
  def cumProd: Vec[Double] = r.filterScanLeft(sd.notMissing)(1d)(_ * _)
}

class IntExpandingStats(r: Vec[Int]) extends VecExpandingStats[Int] {
  private val sa = ScalarTagInt

  def cumSum: Vec[Int] = r.filterScanLeft(sa.notMissing)(0)(_ + _)
  def cumCount: Vec[Int] = r.filterScanLeft(sa.notMissing)(0)((a, _) => a + 1)
  def cumMin: Vec[Int] =
    r.filterScanLeft(sa.notMissing)(sa.inf)((x: Int, y: Int) =>
      if (x < y) x else y
    )
  def cumMax: Vec[Int] =
    r.filterScanLeft(sa.notMissing)(sa.negInf)((x: Int, y: Int) =>
      if (x > y) x else y
    )
  def cumProd: Vec[Int] = r.filterScanLeft(sa.notMissing)(1)(_ * _)
}

class LongExpandingStats(r: Vec[Long]) extends VecExpandingStats[Long] {
  private val sl = ScalarTagLong

  def cumSum: Vec[Long] = r.filterScanLeft(sl.notMissing)(0L)(_ + _)
  def cumCount: Vec[Int] = r.filterScanLeft(sl.notMissing)(0)((a, _) => a + 1)
  def cumMin: Vec[Long] =
    r.filterScanLeft(sl.notMissing)(sl.inf)((x: Long, y: Long) =>
      if (x < y) x else y
    )
  def cumMax: Vec[Long] =
    r.filterScanLeft(sl.notMissing)(sl.negInf)((x: Long, y: Long) =>
      if (x > y) x else y
    )
  def cumProd: Vec[Long] = r.filterScanLeft(sl.notMissing)(1L)(_ * _)
}
