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

import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import org.scalacheck.{Gen, Arbitrary}
import org.scalacheck.Prop._

class SeriesCheck extends Specification with ScalaCheck {

  "Series[Int, Double] Tests" in {
    implicit val ser = Arbitrary(SeriesArbitraries.seriesDoubleWithNA)

    "median works" in {
      forAll { (s: Series[Int, Double]) =>
        (s.median must_== s.toVec.median) or (s.median.isNaN && s.toVec.median.isNaN)
      }
    }

    "series equality" in {
      forAll { (s: Series[Int, Double]) =>
        (s must_== Series(s.toVec)) and (s must_== s)
      }
    }
    "dropNA works" in {
      forAll { (s: Series[Int, Double]) =>
        (s.dropNA must_== s.toSeq.filterNot(_._2.isNaN).toSeries) and
          (s.dropNA.toVec.toSeq.count(_.isNaN) must_== 0)
      }
    }

    "mask works" in {
      forAll { (s: Series[Int, Double]) =>
        if (s.length > 0) {
          val m = s.index.toVec.map(_ % 2 == 0)
          val masked = s.mask(m)
          m.toSeq.zipWithIndex.forall {
            case (b, idx) =>
              if (b) masked.at(idx).isNA else true
          }
        } else true
      }
    }
    "mask works" in {
      forAll { (s: Series[Int, Double]) =>
        if (s.length > 0) {
          val m = s.index.toVec.map(_ % 2 == 0)
          val masked = s.maskIx(_ % 2 == 0)
          m.toSeq.zipWithIndex.forall {
            case (b, idx) =>
              if (b) masked.at(idx).isNA else true
          }
        } else true
      }
    }
    "mask works" in {
      forAll { (s: Series[Int, Double]) =>
        if (s.length > 0) {
          val m = s.values.map(_.toInt % 2 == 0)
          val masked = s.mask(_.toInt % 2 == 0)
          m.toSeq.zipWithIndex.forall {
            case (b, idx) =>
              if (b) masked.at(idx).isNA else true
          }
        } else true
      }
    }

    "filterIx works" in {
      forAll { (s: Series[Int, Double]) =>
        val evens = s.filterIx(_ % 2 == 0)
        evens must_== s.toSeq.filter(_._1 % 2 == 0).toSeries
      }
    }

    "find works" in {
      forAll { (s: Series[Int, Double]) =>
        s(s.find(_.toInt % 2 == 0)) must_== s.filter(_.toInt % 2 == 0)
      }
    }
    "findKey works" in {
      forAll { (s: Series[Int, Double]) =>
        s.findKey(_.toInt % 2 == 0) must_== s.toSeq
          .filterNot(_._2.isNaN)
          .filter(_._2.toInt % 2 == 0)
          .map(_._1)
          .toIndex
      }
    }
    "findOne works" in {
      forAll { (s: Series[Int, Double]) =>
        s.findOne(_.toInt % 2 == 0) must_== s.toVec.toSeq.zipWithIndex
          .filterNot(_._1.isNaN)
          .filter(_._1.toInt % 2 == 0)
          .map(_._2)
          .headOption
          .getOrElse(-1)
      }
    }
    "findOneKey works" in {
      forAll { (s: Series[Int, Double]) =>
        (s.findOneKey(_.toInt % 2 == 0): Option[Int]) must_== s.toSeq
          .filterNot(_._2.isNaN)
          .filter(_._2.toInt % 2 == 0)
          .map(_._1)
          .headOption
      }
    }
    "minKey works" in {
      forAll { (s: Series[Int, Double]) =>
        s.minKey.toOption must_== s.toSeq
          .filterNot(_._2.isNaN)
          .sortBy(_._2)
          .headOption
          .map(_._1)
      }
    }
    "maxKey works" in {
      forAll { (s: Series[Int, Double]) =>
        s.maxKey.toOption must_== s.toSeq
          .filterNot(_._2.isNaN)
          .sortBy(_._2 * (-1))
          .headOption
          .map(_._1)
      }
    }
    "contains works" in {
      forAll { (s: Series[Int, Double], elem: Int) =>
        s.contains(elem) must_== s.index.toSeq.contains(elem)
      }
    }
    "exists works" in {
      forAll { (s: Series[Int, Double]) =>
        s.exists(_.toInt % 2 == 0) must_== s.toVec.dropNA.toSeq
          .exists(_.toInt % 2 == 0)
      }
    }
    "mapValues" in {
      Series(1, 2, 3, 4).mapValues(_ * 2) must_== Series(2, 4, 6, 8)
    }
    "scanLeft" in {
      Series(1, 2, 3, 4).scanLeft(0)(_ + _) must_== Series(1, 3, 6, 10)
    }
    "joinMap" in {
      Series(1, na.to[Int], 3, 4)
        .joinMap(Series(1, 2, na.to[Int], 4))(_ + _) must_== Series(
        2,
        na.to[Int],
        na.to[Int],
        8
      )
    }
    "sorted" in {
      forAll { (s: Series[Int, Double]) =>
        (s.sorted.dropNA must_== s.toSeq.sortBy(_._2).toSeries.dropNA) and
          (Series(1, na.to[Int], 2).sorted must_== Series(
            1 -> na.to[Int],
            0 -> 1,
            2 -> 2
          ))
      }
    }
    "take works" in {
      forAll { (s: Series[Int, Double]) =>
        (s.length > 0) ==> {
          val idx = Gen.listOfN(3, Gen.choose(0, s.length - 1))
          forAll(idx) { i =>
            val res = s.take(i.toArray)
            val exp = s(i(0)) concat s(i(1)) concat s(i(2))
            res must_== exp
          }
        }
      }
    }

    "head works" in {
      forAll { (s: Series[Int, Double]) =>
        s.head(0) must_== Series.empty[Int, Double]
        if (s.length == 1) {
          s.head(1) must_== s(0)
        } else {
          val exp = s(0) concat s(1)
          s.head(2) must_== exp
        }
      }
    }

    "tail works" in {
      forAll { (s: Series[Int, Double]) =>
        s.tail(0) must_== Series.empty[Int, Double]
        if (s.length == 1) {
          s.tail(1) must_== s(0)
        } else {
          val exp = s(s.length - 2) concat s(s.length - 1)
          s.tail(2) must_== exp
        }
      }
    }

    "shift works" in {
      forAll { (s: Series[Int, Double]) =>
        s.shift(1).index must_== s.index

        if (!s.isEmpty) {
          val exp = Vec(na.to[Double]) concat s.values.slice(0, s.length - 1)
          s.shift(1).values must_== exp
        } else s.shift(1).isEmpty must beTrue

        s.shift(-1).index must_== s.index

        if (!s.isEmpty) {
          val exp = s.values.slice(1, s.length) concat Vec(na.to[Double])
          s.shift(-1).values must_== exp
        } else s.shift(1).isEmpty must beTrue
      }
    }

    "first works" in {
      forAll { (s: Series[Int, Double]) =>
        if (s.isEmpty)
          s.first must_== scalar.NA
        else
          s.first must_== s.values.at(0)
      }
    }

    "last works" in {
      forAll { (s: Series[Int, Double]) =>
        if (s.isEmpty)
          s.last must_== scalar.NA
        else
          s.last must_== s.values.at(s.length - 1)
      }
    }

    "first (key) works" in {
      implicit val ser = Arbitrary(SeriesArbitraries.dupSeriesDoubleWithNA)

      forAll { (s: Series[Int, Double]) =>
        (s.length > 0) ==> {
          val loc = Gen.choose(0, s.length - 1)
          forAll(loc) { i =>
            val idx = s.index.raw(i)
            s.first(idx) must_== s.values.at(s.index.findOne(_ == idx))
          }
        }
      }
    }

    "last (key) works" in {
      implicit val ser = Arbitrary(SeriesArbitraries.dupSeriesDoubleWithNA)

      forAll { (s: Series[Int, Double]) =>
        (s.length > 0) ==> {
          val loc = Gen.choose(0, s.length - 1)
          forAll(loc) { i =>
            val idx = s.index.raw(i)
            s.last(idx) must_== s(idx).tail(1).at(0)
          }
        }
      }
    }

    "apply/slice (no index dups) works" in {
      forAll { (s: Series[Int, Double]) =>
        (s.length > 0) ==> {
          val idx = Gen.listOfN(3, Gen.choose(0, s.length - 1))

          forAll(idx) { i =>
            s(i.toArray) must_== s.take(i.toArray)
            s(i: _*) must_== s.take(i.toArray)
          }

          val locs = for {
            i <- Gen.choose(0, s.length - 1)
            j <- Gen.choose(i, s.length - 1)
          } yield (i, j)

          forAll(locs) {
            case (i, j) =>
              val exp = s.take(Range(i, j + 1).toArray)
              s(i -> j) must_== exp
              s.sliceBy(i, j) must_== exp
              s.sliceBy(i, j, inclusive = false) must_== s.take(
                Range(i, j).toArray
              )
          }
        }
      }
    }

    "apply/slice (with index dups) works" in {
      implicit val ser = Arbitrary(SeriesArbitraries.dupSeriesDoubleWithNA)

      forAll { (s: Series[Int, Double]) =>
        (s.length > 0) ==> {
          val idx = Gen.listOfN(3, Gen.choose(0, s.length - 1))

          forAll(idx) { i =>
            (i.length must be_<=(2)) or {
              val locs = i.toArray
              val keys = s.index.take(locs).toArray
              val exp = s(keys(0)) concat s(keys(1)) concat s(keys(2))

              s(keys) must_== exp
              s(keys: _*) must_== exp

              val srt = s.sortedIx

              val exp2 = srt.slice(
                srt.index.getFirst(keys(0)),
                srt.index.getLast(keys(1)) + 1
              )
              srt(keys(0) -> keys(1)) must_== exp2
              srt.sliceBy(keys(0), keys(1)) must_== exp2

              val exp3 = srt.slice(
                srt.index.getFirst(keys(0)),
                srt.index.getLast(keys(1)) - srt.index.count(keys(1)) + 1
              )
              srt.sliceBy(keys(0), keys(1), inclusive = false) must_== exp3
            }
          }
        }
      }
    }

    "splitAt works" in {
      forAll { (s: Series[Int, Double]) =>
        (s.length > 0) ==> {
          val idx = Gen.choose(0, s.length - 1)
          forAll(idx) { i =>
            val (res1, res2) = s.splitAt(i)
            res1.length must_== i
            res2.length must_== (s.length - i)
            (res1 concat res2) must_== s
          }
        }
      }
    }

    "proxyWith" in {
      forAll { (s1: Series[Int, Double], s2: Series[Int, Double]) =>
        val proxied = s1.proxyWith(s2)
        val all =
          for (i <- 0 until proxied.length if s1.at(i).isNA && i < s2.length)
            yield {
              proxied.at(i) must_== s2.at(i)
            }
        all.foldLeft(true)((acc, v) => acc && v.isSuccess)
      }
    }

    "filter works" in {
      forAll { (s1: Series[Int, Double]) =>
        s1.filter(_ > 0).sum >= 0 must beTrue
        s1.filter(_ < 0).sum <= 0 must beTrue
      }
    }

    "filterAt works" in {
      forAll { (s: Series[Int, Double]) =>
        (s.length > 0) ==> {
          val idx = Gen.choose(0, s.length - 1)
          forAll(idx) { i =>
            (s.filterAt(_ != i).length == 0 || s
              .filterAt(_ != i)
              .length == s.length - 1) must beTrue
          }
        }
      }
    }

    "reindex works" in {
      forAll { (s1: Series[Int, Double], s2: Series[Int, Double]) =>
        s1.reindex(s2.index).index must_== s2.index
      }
    }

    "reindex works" in {
      forAll { (s1: Series[Int, Double], s2: Series[Int, Double]) =>
        s1.reindex(s2.index.toSeq: _*).index must_== s2.index
      }
    }

    "pivot works" in {
      val v1 = vec.rand(8)
      val v3 = vec.rand(7)
      val x1 = Index(
        ("a", "1m"),
        ("a", "3m"),
        ("a", "6m"),
        ("a", "1y"),
        ("a", "2y"),
        ("a", "3y"),
        ("a", "10y"),
        ("a", "20y")
      )
      val x2 = Index(
        ("b", "1m"),
        ("b", "3m"),
        ("b", "6m"),
        ("b", "1y"),
        ("b", "2y"),
        ("b", "3y"),
        ("b", "20y")
      )

      val a = Series(v1, x1)
      val b = Series(v3, x2)

      val c = a concat b

      val dat1 = v1.toDoubleArray
      val dat2 = v3.sliceBy(0, 5).toDoubleArray ++ Array(na.to[Double]) ++ v3
        .sliceBy(6, 7)
        .toDoubleArray
      val exp = Frame(Mat(2, 8, dat1 ++ dat2), Index("a", "b"), x1.map(_._2))

      c.pivot must_== exp
    }

    "pivot/melt are opposites" in {
      implicit val frame = Arbitrary(FrameArbitraries.frameDoubleWithNA)
      forAll { (f: Frame[Int, Int, Double]) =>
        f.melt.pivot must_== f
      }
    }

  }

}
