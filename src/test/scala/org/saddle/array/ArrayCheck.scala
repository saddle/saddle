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
      def arrAndLocs = for {
        arr <- Gen.listOf(Gen.choose(-3d, 3d))
        loc <- Gen.listOf(Gen.choose(-1, arr.length - 1))
      } yield (arr.toArray, loc.toArray)

      forAll(arrAndLocs) { case (arr: Array[Double], locs: Array[Int]) =>
        val v = Vec(array.take(arr, locs, 0d))
        array.sum(arr, locs, 0d) must_== v.sum
      }
    }

    "array of ints" in {
      def arrAndLocs = for {
        arr <- Gen.listOf(Gen.choose(-3, 3))
        loc <- Gen.listOf(Gen.choose(-1, arr.length - 1))
      } yield (arr.toArray, loc.toArray)

      forAll(arrAndLocs) { case (arr: Array[Int], locs: Array[Int]) =>
        val v = Vec(array.take(arr, locs, 0))
        array.sum(arr, locs, 0) must_== v.sum
      }
    }
  }
}
