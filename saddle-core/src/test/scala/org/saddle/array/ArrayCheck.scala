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
package org.saddle.array

import org.saddle._
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import org.scalacheck.Gen
import org.scalacheck.Prop._

/**
  * Test properties of array package
  */
class ArrayCheck extends Specification with ScalaCheck {

  "sum works for" in {
    "array of doubles" in {
      def arrAndLocs =
        for {
          arr <- Gen.listOf(Gen.choose(-3d, 3d))
          loc <- Gen.listOf(Gen.choose(-1, arr.length - 1))
        } yield (arr.toArray, loc.toArray)

      forAll(arrAndLocs) {
        case (arr: Array[Double], locs: Array[Int]) =>
          val v = Vec(array.take(arr, locs, 0d))
          array.sum(arr, locs, 0d) must_== v.sum
      }
    }

    "array of ints" in {
      def arrAndLocs =
        for {
          arr <- Gen.listOf(Gen.choose(-3, 3))
          loc <- Gen.listOf(Gen.choose(-1, arr.length - 1))
        } yield (arr.toArray, loc.toArray)

      forAll(arrAndLocs) {
        case (arr: Array[Int], locs: Array[Int]) =>
          val v = Vec(array.take(arr, locs, 0))
          array.sum(arr, locs, 0) must_== v.sum
      }
    }
    "tile" in {
      forAll { (arr1: Array[Int]) =>
        (array.tile(arr1, 0).size must_== 0) and
          (array
            .tile(arr1, 3)
            must_== (0 until 3 flatMap (_ => arr1.toSeq)).toArray)
      }
    }
    "insertion sort" in {
      forAll { (arr1: Seq[Int]) =>
        val p = Array.ofDim[Int](arr1.size)
        import spire.std.int._
        array.PermuteInsertionSort.sort(arr1.toArray, 0, arr1.size, p)
        p must_== arr1.toArray.zipWithIndex.sortBy(_._1).map(_._2)
      }
    }
    "merge sort" in {
      forAll { (arr1: Seq[Int]) =>
        array.argsort(arr1.toArray) must_== arr1.toArray.zipWithIndex
          .sortBy(_._1)
          .map(_._2)
      }
    }
    "shuffle" in {
      forAll { (arr1: Array[Int]) =>
        array.shuffle(arr1).toSet must_== arr1.toSet
      }
    }
    "shuffle" in {
      val ar1 = 0 until 10 toArray

      ((0 until 10000) map (_ => array.shuffle(ar1).apply(0))).toSet must_== ar1.toSet
    }
    "randInt" in {
      array.randInt(10000, -10, 10).toSet must_== (-10 to 10).toSet
    }
    "randIntPos" in {
      val r = array.randIntPos(10000)
      r.forall(_ > 0)
    }
    "randLongPos" in {
      val r = array.randLongPos(10000)
      r.forall(_ > 0)
    }
    "randDoublePos" in {
      val r = array.randLongPos(10000)
      r.forall(_ > 0)
    }
    "randLong" in {
      array.randLong(10000, -10, 10).toSet must_== (-10L to 10L).toSet
    }
    "send" in {
      array.send(Array(5, 6, 7), Array(2, 0, 1)) must_== Array(6, 7, 5)
    }
    "put" in {
      array.put(Array(5, 6, 7), Array(0, 1), -1) must_== Array(-1, -1, 7)
    }
    "put" in {
      array.put(Array(5, 6, 7), Array(true, false, true), -1) must_== Array(
        -1,
        6,
        -1
      )
    }
    "put" in {
      array.putn(Array(5, 6, 7), Array(1, 2), Array(-1, -2)) must_== Array(
        5,
        -1,
        -2
      )
    }
    "linspace" in {
      array.linspace(2d, 3d, 5, true) must_== Array(2d, 2.25, 2.5, 2.75, 3d)
    }
    "linspace" in {
      array.linspace(0d, 1d, 3, true) must_== Array(0d, 0.5, 1d)
    }
    "linspace" in {
      array.linspace(0d, 1d, 0, true) must_== Array[Double]()
    }
    "linspace" in {
      array.linspace(1d, 0d, 3, true) must_== Array[Double](1d, 0.5, 0d)
    }
    "linspace" in {
      array.linspace(2d, 3d, 5, false) must_== Array(
        2,
        2 + 1d / 5,
        2 + 2d / 5,
        2 + 3d / 5,
        2 + 4d / 5
      )
    }

  }
}
