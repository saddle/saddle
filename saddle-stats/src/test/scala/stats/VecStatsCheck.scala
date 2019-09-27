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
import org.specs2.ScalaCheck
import org.scalacheck.Prop.forAll
import org.saddle.{VecArbitraries, Vec}
import org.scalacheck.{Gen, Arbitrary}

// check stats implementation against apache math
import org.apache.commons.math.stat.descriptive.DescriptiveStatistics

/**
  * Tests on arbitrarily-generated vectors
  */
class VecStatsCheck extends Specification with ScalaCheck with StatsHelper {

  /**
    * Double Vectors
    */
  "Double Vec Tests" in {

    "take sum of a double vector with no NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecDoubleWithoutNA)

      forAll { (v: Vec[Double]) =>
        val data = v.contents
        v.sum must_== data.sum
      }
    }

    "take sum of a double vector with NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecDoubleWithNA)

      forAll { (v: Vec[Double]) =>
        val data = v.contents.filter(!_.isNaN)
        v.sum must_== data.sum
      }
    }

    "take count of a double vector with no NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecDoubleWithoutNA)

      forAll { (v: Vec[Double]) =>
        val data = v.contents
        v.count must_== data.length
      }
    }

    "take count of a double vector with NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecDoubleWithNA)

      forAll { (v: Vec[Double]) =>
        val data = v.contents.filter(!_.isNaN)
        v.count must_== data.length
      }
    }

    "take min of a double vector with no NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecDoubleWithoutNA)

      forAll { (v: Vec[Double]) =>
        val data = v.contents
        v.min.isEmpty || (v.min.get must_== data.min)
      }
    }

    "take min of a double vector with NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecDoubleWithNA)

      forAll { (v: Vec[Double]) =>
        val data = v.contents.filter(!_.isNaN)
        v.min.isEmpty || (v.min.get must_== data.min)
      }
    }

    "take max of a double vector with no NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecDoubleWithoutNA)

      forAll { (v: Vec[Double]) =>
        val data = v.contents
        v.max.isEmpty || (v.max.get must_== data.max)
      }
    }

    "take max of a double vector with NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecDoubleWithNA)

      forAll { (v: Vec[Double]) =>
        val data = v.contents.filter(!_.isNaN)
        v.max.isEmpty || (v.max.get must_== data.max)
      }
    }

    "take prod of a double vector with no NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecDoubleWithoutNA)

      forAll { (v: Vec[Double]) =>
        val data = v.contents
        areClose(v.prod, data.foldLeft(1.0)(_ * _))
      }
    }

    "take prod of a double vector with NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecDoubleWithNA)

      forAll { (v: Vec[Double]) =>
        val data = v.contents.filter(!_.isNaN)
        areClose(v.prod, data.foldLeft(1.0)(_ * _))
      }
    }

    "take countif of a double vector with no NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecDoubleWithoutNA)

      forAll { (v: Vec[Double]) =>
        val data = v.contents
        v.countif(_ > 0.5) must_== data
          .filter(_ > 0.5)
          .foldLeft(0)((x, _) => x + 1)
      }
    }

    "take countif of a double vector with NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecDoubleWithNA)

      forAll { (v: Vec[Double]) =>
        val data = v.contents.filter(!_.isNaN)
        v.countif(_ > 0.5) must_== data
          .filter(_ > 0.5)
          .foldLeft(0)((x, _) => x + 1)
      }
    }

    "take logsum of a double vector with no NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecDoublePWithoutNA)

      forAll { (v: Vec[Double]) =>
        val data = v.contents
        areClose(v.logsum, data.foldLeft(0.0)(_ + math.log(_)))
      }
    }

    "take logsum of a double vector with NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecDoublePWithNA)

      forAll { (v: Vec[Double]) =>
        val data = v.contents.filter(!_.isNaN)
        areClose(v.logsum, data.foldLeft(0.0)(_ + math.log(_)))
      }
    }

    "take mean of a double vector with no NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecDoubleWithoutNA)

      forAll { (v: Vec[Double]) =>
        val data = v.contents
        val mean = v.mean
        areClose(mean, data.sum / data.length)
      }
    }

    "take mean of a double vector with NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecDoubleWithNA)

      forAll { (v: Vec[Double]) =>
        val data = v.contents.filter(!_.isNaN)
        val mean = v.mean
        areClose(mean, data.sum / data.length)
      }
    }

    "take median of a double vector with no NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecDoubleWithoutNA)

      forAll { (v: Vec[Double]) =>
        val data = v.contents.sorted
        val len = data.length
        val med = v.median
        (len == 0 && med.isNaN) || {
          len % 2 match {
            case 0 => areClose(med, (data(len / 2) + data(len / 2 - 1)) / 2.0)
            case 1 => areClose(med, data(len / 2))
          }
        }
      }
    }

    "take median of a double vector with NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecDoubleWithNA)

      forAll { (v: Vec[Double]) =>
        val data = v.contents.filter(!_.isNaN).sorted
        val len = data.length
        val med = v.median
        (len == 0 && med.isNaN) || {
          len % 2 match {
            case 0 => areClose(med, (data(len / 2) + data(len / 2 - 1)) / 2.0)
            case 1 => areClose(med, data(len / 2))
          }
        }
      }
    }

    "take geomean of a double vector with no NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecDoublePWithoutNA)

      forAll { (v: Vec[Double]) =>
        val mean = v.geomean

        val stats = new DescriptiveStatistics()
        v.contents.foreach { v =>
          stats.addValue(v)
        }

        areClose(mean, stats.getGeometricMean)
      }
    }

    "take geomean of a double vector with NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecDoublePWithNA)

      forAll { (v: Vec[Double]) =>
        val mean = v.geomean

        val stats = new DescriptiveStatistics()
        v.contents.filter(!_.isNaN).foreach { v =>
          stats.addValue(v)
        }

        areClose(mean, stats.getGeometricMean)
      }
    }

    "take variance of a double vector with no NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecDoubleWithoutNA)

      forAll { (v: Vec[Double]) =>
        val vrnc = v.variance

        val stats = new DescriptiveStatistics()
        v.contents.foreach { v =>
          stats.addValue(v)
        }

        areClose(vrnc, stats.getVariance)
      }
    }

    "take variance of a double vector with NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecDoubleWithNA)

      forAll { (v: Vec[Double]) =>
        val vrnc = v.variance

        val stats = new DescriptiveStatistics()
        v.contents.filter(!_.isNaN).foreach { v =>
          stats.addValue(v)
        }

        areClose(vrnc, stats.getVariance)
      }
    }

    "take skew of a double vector with no NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecDoubleWithoutNA)

      forAll { (v: Vec[Double]) =>
        val skew = v.skew

        val stats = new DescriptiveStatistics()
        v.contents.foreach { v =>
          stats.addValue(v)
        }

        areClose(skew, stats.getSkewness)
      }
    }

    "take skew of a double vector with NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecDoubleWithNA)

      forAll { (v: Vec[Double]) =>
        val skew = v.skew

        val stats = new DescriptiveStatistics()
        v.contents.filter(!_.isNaN).foreach { v =>
          stats.addValue(v)
        }

        areClose(skew, stats.getSkewness)
      }
    }

    "take kurt of a double vector with no NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecDoubleWithoutNA)

      forAll { (v: Vec[Double]) =>
        val kurt = v.kurt

        val stats = new DescriptiveStatistics()
        v.contents.foreach { v =>
          stats.addValue(v)
        }

        areClose(kurt, stats.getKurtosis)
      }
    }

    "take kurt of a double vector with NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecDoubleWithNA)

      forAll { (v: Vec[Double]) =>
        val kurt = v.kurt

        val stats = new DescriptiveStatistics()
        v.contents.filter(!_.isNaN).foreach { v =>
          stats.addValue(v)
        }

        areClose(kurt, stats.getKurtosis)
      }
    }

    "check percentiles of an double vector make sense" in {
      implicit val vec = Arbitrary(VecArbitraries.vecDoubleWithNA)

      forAll { (v: Vec[Double]) =>
        val pct50 = v.percentile(35)
        val pct100 = v.percentile(100)

        val stats = new DescriptiveStatistics()
        v.contents.filter(!_.isNaN).foreach { v =>
          stats.addValue(v)
        }

        areClose(pct50, stats.getPercentile(35))
        areClose(v.percentile(50), v.median)

        areClose(pct100, stats.getPercentile(100))
      }
    }

    "take n-rolling median of a vec with NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecDoubleWithNA)

      case class Window(sz: Int)

      forAll { (v: Vec[Double]) =>
        (v.length > 3) ==> {
          implicit val win =
            Arbitrary(for (sz <- Gen.choose(2, v.length)) yield Window(sz))

          forAll { (k: Window) =>
            v.rollingMedian(k.sz) must_== v.rolling(k.sz, _.median)
          }
        }
      }
    }
  }

  /**
    * Long vectors
    */
  "Long Vec Tests" in {

    "take sum of a Long vector with no NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecLongWithoutNA)

      forAll { (v: Vec[Long]) =>
        val data = v.contents
        v.sum must_== data.sum
      }
    }

    "take sum of a Long vector with NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecLongWithNA)

      forAll { (v: Vec[Long]) =>
        val data = v.contents.filter(_ != Long.MinValue)
        v.sum must_== data.sum
      }
    }

    "take count of a Long vector with no NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecLongWithoutNA)

      forAll { (v: Vec[Long]) =>
        val data = v.contents
        v.count must_== data.length
      }
    }

    "take count of a Long vector with NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecLongWithNA)

      forAll { (v: Vec[Long]) =>
        val data = v.contents.filter(_ != Long.MinValue)
        v.count must_== data.length
      }
    }

    "take min of a Long vector with no NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecLongWithoutNA)

      forAll { (v: Vec[Long]) =>
        val data = v.contents
        v.min.isEmpty || (v.min.get must_== data.min)
      }
    }

    "take min of a Long vector with NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecLongWithNA)

      forAll { (v: Vec[Long]) =>
        val data = v.contents.filter(_ != Long.MinValue)
        v.min.isEmpty || (v.min.get must_== data.min)
      }
    }

    "take max of a Long vector with no NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecLongWithoutNA)

      forAll { (v: Vec[Long]) =>
        val data = v.contents
        v.max.isEmpty || (v.max.get must_== data.max)
      }
    }

    "take max of a Long vector with NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecLongWithNA)

      forAll { (v: Vec[Long]) =>
        val data = v.contents.filter(_ != Long.MinValue)
        v.max.isEmpty || (v.max.get must_== data.max)
      }
    }

    "take prod of a Long vector with no NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecLongWithoutNA)

      forAll { (v: Vec[Long]) =>
        val data = v.contents
        areClose(v.prod.toDouble, data.foldLeft(1L)(_ * _).toDouble)
      }
    }

    "take prod of a Long vector with NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecLongWithNA)

      forAll { (v: Vec[Long]) =>
        val data = v.contents.filter(_ != Long.MinValue)
        areClose(v.prod.toDouble, data.foldLeft(1L)(_ * _).toDouble)
      }
    }

    "take countif of a Long vector with no NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecLongWithoutNA)

      forAll { (v: Vec[Long]) =>
        val data = v.contents
        v.countif(_ > 0) must_== data.filter(_ > 0).foldLeft(0)((x, _) => x + 1)
      }
    }

    "take countif of a Long vector with NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecLongWithNA)

      forAll { (v: Vec[Long]) =>
        val data = v.contents.filter(_ != Long.MinValue)
        v.countif(_ > 0) must_== data.filter(_ > 0).foldLeft(0)((x, _) => x + 1)
      }
    }

    "take logsum of a Long vector with no NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecLongPWithoutNA)

      forAll { (v: Vec[Long]) =>
        val data = v.contents
        areClose(v.logsum, data.map(_.toDouble).foldLeft(0.0)(_ + math.log(_)))
      }
    }

    "take logsum of a Long vector with NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecLongPWithNA)

      forAll { (v: Vec[Long]) =>
        val data = v.contents.filter(_ != Long.MinValue)
        areClose(v.logsum, data.map(_.toDouble).foldLeft(0.0)(_ + math.log(_)))
      }
    }

    "take mean of a Long vector with no NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecLongWithoutNA)

      forAll { (v: Vec[Long]) =>
        val data = v.contents
        val mean = v.mean
        areClose(mean, data.sum.toDouble / data.length)
      }
    }

    "take mean of a Long vector with NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecLongWithNA)

      forAll { (v: Vec[Long]) =>
        val data = v.contents.filter(_ != Long.MinValue)
        val mean = v.mean
        areClose(mean, data.sum.toDouble / data.length)
      }
    }

    "take median of a Long vector with no NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecLongWithoutNA)

      forAll { (v: Vec[Long]) =>
        val data = v.contents.sorted
        val len = data.length
        val med = v.median
        (len == 0 && med.isNaN) || {
          len % 2 match {
            case 0 => areClose(med, (data(len / 2) + data(len / 2 - 1)) / 2.0)
            case 1 => areClose(med, data(len / 2).toDouble)
          }
        }
      }
    }

    "take median of a Long vector with NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecLongWithNA)

      forAll { (v: Vec[Long]) =>
        val data = v.contents.filter(_ != Long.MinValue).sorted
        val len = data.length
        val med = v.median
        (len == 0 && med.isNaN) || {
          len % 2 match {
            case 0 => areClose(med, (data(len / 2) + data(len / 2 - 1)) / 2.0)
            case 1 => areClose(med, data(len / 2).toDouble)
          }
        }
      }
    }

    "take geomean of a Long vector with no NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecLongPWithoutNA)

      forAll { (v: Vec[Long]) =>
        val mean = v.geomean

        val stats = new DescriptiveStatistics()
        v.contents.foreach { v =>
          stats.addValue(v.toDouble)
        }

        areClose(mean, stats.getGeometricMean)
      }
    }

    "take geomean of a Long vector with NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecLongPWithNA)

      forAll { (v: Vec[Long]) =>
        val mean = v.geomean

        val stats = new DescriptiveStatistics()
        v.contents.filter(_ != Long.MinValue).foreach { v =>
          stats.addValue(v.toDouble)
        }

        areClose(mean, stats.getGeometricMean)
      }
    }

    "take variance of a Long vector with no NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecLongWithoutNA)

      forAll { (v: Vec[Long]) =>
        val vrnc = v.variance

        val stats = new DescriptiveStatistics()
        v.contents.foreach { v =>
          stats.addValue(v)
        }

        areClose(vrnc, stats.getVariance)
      }
    }

    "take variance of a Long vector with NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecLongWithNA)

      forAll { (v: Vec[Long]) =>
        val vrnc = v.variance

        val stats = new DescriptiveStatistics()
        v.contents.filter(_ != Long.MinValue).foreach { v =>
          stats.addValue(v)
        }

        areClose(vrnc, stats.getVariance)
      }
    }

    "take skew of a Long vector with no NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecLongWithoutNA)

      forAll { (v: Vec[Long]) =>
        val skew = v.skew

        val stats = new DescriptiveStatistics()
        v.contents.foreach { v =>
          stats.addValue(v)
        }

        areClose(skew, stats.getSkewness)
      }
    }

    "take skew of a Long vector with NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecLongWithNA)

      forAll { (v: Vec[Long]) =>
        val skew = v.skew

        val stats = new DescriptiveStatistics()
        v.contents.filter(_ != Long.MinValue).foreach { v =>
          stats.addValue(v)
        }

        areClose(skew, stats.getSkewness)
      }
    }

    "take kurt of a Long vector with no NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecLongWithoutNA)

      forAll { (v: Vec[Long]) =>
        val kurt = v.kurt

        val stats = new DescriptiveStatistics()
        v.contents.foreach { v =>
          stats.addValue(v)
        }

        areClose(kurt, stats.getKurtosis)
      }
    }

    "take kurt of a Long vector with NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecLongWithNA)

      forAll { (v: Vec[Long]) =>
        val kurt = v.kurt

        val stats = new DescriptiveStatistics()
        v.contents.filter(_ != Long.MinValue).foreach { v =>
          stats.addValue(v)
        }

        areClose(kurt, stats.getKurtosis)
      }
    }
  }

  /**
    * Int vectors
    */
  "Int Vec Tests" in {

    "take sum of a Int vector with no NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecIntWithoutNA)

      forAll { (v: Vec[Int]) =>
        val data = v.contents
        v.sum must_== data.sum
      }
    }

    "take sum of a Int vector with NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecIntWithNA)

      forAll { (v: Vec[Int]) =>
        val data = v.contents.filter(_ != Int.MinValue)
        v.sum must_== data.sum
      }
    }

    "take count of a Int vector with no NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecIntWithoutNA)

      forAll { (v: Vec[Int]) =>
        val data = v.contents
        v.count must_== data.length
      }
    }

    "take count of a Int vector with NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecIntWithNA)

      forAll { (v: Vec[Int]) =>
        val data = v.contents.filter(_ != Int.MinValue)
        v.count must_== data.length
      }
    }

    "take min of a Int vector with no NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecIntWithoutNA)

      forAll { (v: Vec[Int]) =>
        val data = v.contents
        v.min.isEmpty || (v.min.get must_== data.min)
      }
    }

    "take min of a Int vector with NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecIntWithNA)

      forAll { (v: Vec[Int]) =>
        val data = v.contents.filter(_ != Int.MinValue)
        v.min.isEmpty || (v.min.get must_== data.min)
      }
    }

    "take max of a Int vector with no NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecIntWithoutNA)

      forAll { (v: Vec[Int]) =>
        val data = v.contents
        v.max.isEmpty || (v.max.get must_== data.max)
      }
    }

    "take max of a Int vector with NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecIntWithNA)

      forAll { (v: Vec[Int]) =>
        val data = v.contents.filter(_ != Int.MinValue)
        v.max.isEmpty || (v.max.get must_== data.max)
      }
    }

    "take prod of a Int vector with no NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecIntWithoutNA)

      forAll { (v: Vec[Int]) =>
        val data = v.contents
        areClose(v.prod, data.foldLeft(1)(_ * _))
      }
    }

    "take prod of a Int vector with NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecIntWithNA)

      forAll { (v: Vec[Int]) =>
        val data = v.contents.filter(_ != Int.MinValue)
        areClose(v.prod, data.foldLeft(1)(_ * _))
      }
    }

    "take countif of a Int vector with no NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecIntWithoutNA)

      forAll { (v: Vec[Int]) =>
        val data = v.contents
        v.countif(_ > 0) must_== data.filter(_ > 0).foldLeft(0)((x, _) => x + 1)
      }
    }

    "take countif of a Int vector with NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecIntWithNA)

      forAll { (v: Vec[Int]) =>
        val data = v.contents.filter(_ != Int.MinValue)
        v.countif(_ > 0) must_== data.filter(_ > 0).foldLeft(0)((x, _) => x + 1)
      }
    }

    "take logsum of a Int vector with no NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecIntPWithoutNA)

      forAll { (v: Vec[Int]) =>
        val data = v.contents
        areClose(v.logsum, data.foldLeft(0.0)(_ + math.log(_)))
      }
    }

    "take logsum of a Int vector with NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecIntPWithNA)

      forAll { (v: Vec[Int]) =>
        val data = v.contents.filter(_ != Int.MinValue)
        areClose(v.logsum, data.foldLeft(0.0)(_ + math.log(_)))
      }
    }

    "take mean of a Int vector with no NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecIntWithoutNA)

      forAll { (v: Vec[Int]) =>
        val data = v.contents
        val mean = v.mean
        areClose(mean, data.sum.toDouble / data.length)
      }
    }

    "take mean of a Int vector with NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecIntWithNA)

      forAll { (v: Vec[Int]) =>
        val data = v.contents.filter(_ != Int.MinValue)
        val mean = v.mean
        areClose(mean, data.sum.toDouble / data.length)
      }
    }

    "take median of a Int vector with no NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecIntWithoutNA)

      forAll { (v: Vec[Int]) =>
        val data = v.contents.sorted
        val len = data.length
        val med = v.median
        (len == 0 && med.isNaN) || {
          len % 2 match {
            case 0 => areClose(med, (data(len / 2) + data(len / 2 - 1)) / 2.0)
            case 1 => areClose(med, data(len / 2))
          }
        }
      }
    }

    "take median of a Int vector with NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecIntWithNA)

      forAll { (v: Vec[Int]) =>
        val data = v.contents.filter(_ != Int.MinValue).sorted
        val len = data.length
        val med = v.median
        (len == 0 && med.isNaN) || {
          len % 2 match {
            case 0 => areClose(med, (data(len / 2) + data(len / 2 - 1)) / 2.0)
            case 1 => areClose(med, data(len / 2))
          }
        }
      }
    }

    "take geomean of a Int vector with no NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecIntPWithoutNA)

      forAll { (v: Vec[Int]) =>
        val mean = v.geomean

        val stats = new DescriptiveStatistics()
        v.contents.foreach { v =>
          stats.addValue(v)
        }

        areClose(mean, stats.getGeometricMean)
      }
    }

    "take geomean of a Int vector with NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecIntPWithNA)

      forAll { (v: Vec[Int]) =>
        val mean = v.geomean

        val stats = new DescriptiveStatistics()
        v.contents.filter(_ != Int.MinValue).foreach { v =>
          stats.addValue(v)
        }

        areClose(mean, stats.getGeometricMean)
      }
    }

    "take variance of a Int vector with no NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecIntWithoutNA)

      forAll { (v: Vec[Int]) =>
        val vrnc = v.variance

        val stats = new DescriptiveStatistics()
        v.contents.foreach { v =>
          stats.addValue(v)
        }

        areClose(vrnc, stats.getVariance)
      }
    }

    "take variance of a Int vector with NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecIntWithNA)

      forAll { (v: Vec[Int]) =>
        val vrnc = v.variance

        val stats = new DescriptiveStatistics()
        v.contents.filter(_ != Int.MinValue).foreach { v =>
          stats.addValue(v)
        }

        areClose(vrnc, stats.getVariance)
      }
    }

    "take skew of a Int vector with no NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecIntWithoutNA)

      forAll { (v: Vec[Int]) =>
        val skew = v.skew

        val stats = new DescriptiveStatistics()
        v.contents.foreach { v =>
          stats.addValue(v)
        }

        areClose(skew, stats.getSkewness)
      }
    }

    "take skew of a Int vector with NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecIntWithNA)

      forAll { (v: Vec[Int]) =>
        val skew = v.skew

        val stats = new DescriptiveStatistics()
        v.contents.filter(_ != Int.MinValue).foreach { v =>
          stats.addValue(v)
        }

        areClose(skew, stats.getSkewness)
      }
    }

    "take kurt of a Int vector with no NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecIntWithoutNA)

      forAll { (v: Vec[Int]) =>
        val kurt = v.kurt

        val stats = new DescriptiveStatistics()
        v.contents.foreach { v =>
          stats.addValue(v)
        }

        areClose(kurt, stats.getKurtosis)
      }
    }

    "take kurt of a Int vector with NA's" in {
      implicit val vec = Arbitrary(VecArbitraries.vecIntWithNA)

      forAll { (v: Vec[Int]) =>
        val kurt = v.kurt

        val stats = new DescriptiveStatistics()
        v.contents.filter(_ != Int.MinValue).foreach { v =>
          stats.addValue(v)
        }

        areClose(kurt, stats.getKurtosis)
      }
    }
  }
}
