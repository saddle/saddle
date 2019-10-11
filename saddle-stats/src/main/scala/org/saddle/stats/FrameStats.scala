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

import org.saddle._

/**
  * Statistical methods made available to operate on columns of a Frame via enrichment.
  *
  * @param frame The frame to enrich
  * @tparam RX Type of the row index elements
  * @tparam CX Type of the column index elements
  * @tparam T Type of the elements of the frame
  */
class FrameStats[RX, CX, T: ST: NUM](frame: Frame[RX, CX, T]) {
  // --------------------------------------
  // helpful math ops

  type S2Stats = Series2Stats[T]

  /**
    * Conditional count of the elements of each column, ignoring NA values
    * @param test Function predicate to utilize in count, T => Boolean
    */
  def countif(test: T => Boolean): Series[CX, Int] =
    frame.reduce(_.countif(test))

  /**
    * Sum of the natural logs of the elements of each column, ignoring NA values.
    */
  def logsum(implicit ev: S2Stats): Series[CX, Double] = frame.reduce(_.logsum)

  /**
    * Sample mean of each column
    */
  def mean(implicit ev: S2Stats): Series[CX, Double] = frame.reduce(_.mean)

  /**
    * Geometric mean of each column
    */
  def geomean(implicit ev: S2Stats): Series[CX, Double] =
    frame.reduce(_.geomean)

  /**
    * Sample variance of each column
    */
  def variance(implicit ev: S2Stats): Series[CX, Double] =
    frame.reduce(_.variance)

  /**
    * Sample standard deviation of each column
    */
  def stdev(implicit ev: S2Stats): Series[CX, Double] =
    variance.mapValues(math.sqrt)

  /**
    * Sample skewness of each column
    */
  def skew(implicit ev: S2Stats): Series[CX, Double] = frame.reduce(_.skew)

  /**
    * Sample kurtosis of each column
    */
  def kurt(implicit ev: S2Stats): Series[CX, Double] = frame.reduce(_.kurt)

  private type V2Stats = Vec[T] => VecStats[T]

  /**
    * Demean each column in the frame
    */
  def demeaned(implicit ev: V2Stats): Frame[RX, CX, Double] =
    frame.mapVec(_.demeaned)

  private type V2RollingStats = Vec[T] => VecRollingStats[T]

  /**
    * Rolling count; compute count of number of elements in columns of Frame over a sliding window, ignoring
    * any NA values.
    * @param winSz Size of the rolling window
    */
  def rollingCount(winSz: Int)(
      implicit ev: V2RollingStats
  ): Frame[RX, CX, Int] = frame.mapVec(_.rollingCount(winSz))

  /**
    * Rolling sum; compute sum of elements in columns of Frame over a sliding window, ignoring any NA
    * values.
    * @param winSz Size of the sliding window
    */
  def rollingSum(winSz: Int)(implicit ev: V2RollingStats): Frame[RX, CX, T] =
    frame.mapVec(_.rollingSum(winSz))

  /**
    * Rolling mean; compute mean of elements in columns of Frame over a sliding window, ignoring any NA
    * values.
    * @param winSz Size of the sliding window
    */
  def rollingMean(winSz: Int)(
      implicit ev: V2RollingStats
  ): Frame[RX, CX, Double] = frame.mapVec(_.rollingMean(winSz))

  /**
    * Rolling median; compute median of elements in columns of Frame over a sliding window, ignoring any NA
    * values.
    * @param winSz Size of the sliding window
    */
  def rollingMedian(winSz: Int)(
      implicit ev: V2RollingStats
  ): Frame[RX, CX, Double] = frame.mapVec(_.rollingMedian(winSz))

  private type V2ExpandingStats = Vec[T] => VecExpandingStats[T]

  /**
    * Cumulative count for each column; each successive element of the output is the cumulative
    * count from the initial element, ignoring NAs.
    */
  def cumCount(implicit ev: V2ExpandingStats): Frame[RX, CX, Int] =
    frame.mapVec(_.cumCount)

  /**
    * Cumulative sum for each column; each successive element of the output is the cumulative
    * sum from the initial element, ignoring NAs.
    */
  def cumSum(implicit ev: V2ExpandingStats): Frame[RX, CX, T] =
    frame.mapVec(_.cumSum)

  /**
    * Cumulative product for each column; each successive element of the output is the cumulative
    * product from the initial element, ignoring NAs.
    */
  def cumProd(implicit ev: V2ExpandingStats): Frame[RX, CX, T] =
    frame.mapVec(_.cumProd)

  /**
    * Cumulative min for each column; each successive element of the output is the cumulative
    * min from the initial element, ignoring NAs.
    */
  def cumMin(implicit ev: V2ExpandingStats): Frame[RX, CX, T] =
    frame.mapVec(_.cumMin)

  /**
    * Cumulative max for each column; each successive element of the output is the cumulative
    * max from the initial element, ignoring NAs.
    */
  def cumMax(implicit ev: V2ExpandingStats): Frame[RX, CX, T] =
    frame.mapVec(_.cumMax)
}
