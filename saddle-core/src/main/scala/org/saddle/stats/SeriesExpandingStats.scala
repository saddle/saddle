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
import Series.Vec2ExpandingStats

/**
 * Expanding statistical methods made available on numeric Series objects via enrichment.
 * These methods scan over the Series and compute values over a specified historical
 * window.
 */
class SeriesExpandingStats[X: ST: ORD, T: Vec2ExpandingStats: ST](s: Series[X, T]) {
  protected val ev = implicitly[Vec2ExpandingStats[T]]

  /**
   * Cumulative sum; each successive element of the output is the cumulative
   * sum from the initial element, ignoring NAs.
   */
  def cumSum: Series[X, T] = Series(ev(s.values).cumSum, s.index)

  /**
   * Cumulative count; each successive element of the output is the cumulative
   * count from the initial element, ignoring NAs.
   */
  def cumCount: Series[X, Int] = Series(ev(s.values).cumCount, s.index)

  /**
   * Cumulative min; each successive element of the output is the cumulative
   * min from the initial element, ignoring NAs.
   */
  def cumMin: Series[X, T] = Series(ev(s.values).cumMin, s.index)

  /**
   * Cumulative max; each successive element of the output is the cumulative
   * max from the initial element, ignoring NAs.
   */
  def cumMax: Series[X, T] = Series(ev(s.values).cumMax, s.index)

  /**
   * Cumulative product; each successive element of the output is the cumulative
   * product from the initial element, ignoring NAs.
   */
  def cumProd: Series[X, T] = Series(ev(s.values).cumProd, s.index)
}

object SeriesExpandingStats {
  /**
   * Factory method for creating an enriched Series object containing statistical functions;
   * usually created implicitly.
   *
   * @param s Series to wrap
   * @tparam X Type of index
   * @tparam T Type of elements
   */
  def apply[X: ST: ORD, T: Vec2ExpandingStats: ST](s: Series[X, T]) =
    new SeriesExpandingStats(s)
}