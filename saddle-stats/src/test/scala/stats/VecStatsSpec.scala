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

import org.specs2.mutable.Specification
import org.saddle._
import org.saddle.scalar.NA

/**
  * Hand-calculated tests
  */
class VecStatsSpec extends Specification with StatsHelper {

  val v1 = Vec[Double](1d, 2, 20, 23, 76, 12, -5, -27, 76, 67)
  val v1pos = Vec[Double](1d, 2, 20, 23, 76, 12, 76, 67)
  val v2 = Vec[Double](12d, 4, 19, 23, 76, 7, 6, -29, 50, 17)
  val v3 = Vec[Double](1d, 2, 20, 15, 23, 56, 12)
  val v4 = Vec[Double](1d, 2, 20, 23, 56, 12)
  val v5 = Vec[Double](2d, 89, 23)

  "compute mean of a vector" in {
    areClose(v1.mean, 24.5d)
  }

  "compute the median of a vector" in {
    areClose(v1.median, 16d)
  }

  "compute the geometric mean of a vector with positive elements" in {
    areClose(v1pos.geomean, 15.9895, 1e-4)
  }

  "compute the sample variance of a vector" in {
    areClose(v1.variance, 1318.9444, 1e-4)
  }

  "compute the sample standard deviation of a vector" in {
    areClose(v1.stdev, 36.3173, 1e-4)
  }

  "compute the sample skewness of a vector (unbiased)" in {
    areClose(v1.skew, 0.4676, 1e-4)
  }

  "compute the sample excess kurtosis of a vector (unbiased)" in {
    areClose(v1.kurt, -1.1138, 1e-4)
  }

  "find the maximum element of a vector" in {
    areClose(v1.max.get, 76d)
  }

  "find the minimum element of a vector" in {
    areClose(v1.min.get, -27d)
  }

  "find the sum of all elements in a vector" in {
    areClose(v1.sum, 245d)
  }

  "find the product of all elements in a vector" in {
    areClose(v1.prod, 5.7677e11, 1e-4)
  }

  "Vector.median on an even vector is equivalent to the mean of the two center elements" in {
    areClose(v4.median, (12 + 20) / 2d)
  }

  "Vector.geometricMean on a 3 element vector is equivalent to the cube root of the product of elements" in {
    areClose(v5.geomean, math.cbrt(v5.foldLeft(1d)(_ * _)))
  }

  "Vector skew corner case works" in {
    val vec = Vec[Double](-1d, 1000, -1000, 1)
    vec.skew must beLessThan(1e-9)
  }

  "Rank works" in {
    val vec = Vec[Double](1.0, 5.0, 4.0, 4.0, NA, 3.0)

    vec.rank(tie = RankTie.Avg, ascending = true) must_== Vec[Double](
      1.0,
      5.0,
      3.5,
      3.5,
      NA,
      2.0
    )
    vec.rank(tie = RankTie.Min, ascending = true) must_== Vec[Double](
      1.0,
      5.0,
      3.0,
      3.0,
      NA,
      2.0
    )
    vec.rank(tie = RankTie.Max, ascending = true) must_== Vec[Double](
      1.0,
      5.0,
      4.0,
      4.0,
      NA,
      2.0
    )
    vec.rank(tie = RankTie.Nat, ascending = true) must_== Vec[Double](
      1.0,
      5.0,
      3.0,
      4.0,
      NA,
      2.0
    )

    vec.rank(tie = RankTie.Avg, ascending = false) must_== Vec[Double](
      5.0,
      1.0,
      2.5,
      2.5,
      NA,
      4.0
    )
    vec.rank(tie = RankTie.Min, ascending = false) must_== Vec[Double](
      5.0,
      1.0,
      2.0,
      2.0,
      NA,
      4.0
    )
    vec.rank(tie = RankTie.Max, ascending = false) must_== Vec[Double](
      5.0,
      1.0,
      3.0,
      3.0,
      NA,
      4.0
    )
    vec.rank(tie = RankTie.Nat, ascending = false) must_== Vec[Double](
      5.0,
      1.0,
      2.0,
      3.0,
      NA,
      4.0
    )

    val vec2 = Vec.empty[Double]
    vec2.rank() must_== vec2

    val vec3 = Vec(1d)
    vec3.rank() must_== vec3
  }

  "Percentile works" in {
    val vec = Vec[Double](15d, 20, 35, 40, NA, 50)
    areClose(vec.percentile(40), 26.0)

    vec.percentile(-1).isNaN must beTrue
    vec.percentile(101).isNaN must beTrue

    Vec.empty[Double].percentile(0).isNaN must beTrue

    Vec(1d).percentile(0) must_== Vec(1d).percentile(100)

    val tst = Vec[Double](
      NA,
      -1000.0000,
      0.0000,
      -946.7879,
      -256.7953,
      1000.0000,
      -307.5079,
      -832.8867
    )
    areClose(tst.percentile(50), -307.5079, 1e-4)

    val tst2 = Vec[Double](1, 0)
    areClose(tst2.percentile(50), 0.5, 1e-4)

    val tst3 = Vec(0.785, 0.0296, 0.2408, 0.884, 0.5759, 0.8087, 0.4421)
    areClose(tst3.percentile(0, PctMethod.Excel), 0.0296, 1e-4)
    areClose(tst3.percentile(35, PctMethod.Excel), 0.4555, 1e-4)
    areClose(tst3.percentile(100, PctMethod.Excel), 0.8840, 1e-4)
  }
}
