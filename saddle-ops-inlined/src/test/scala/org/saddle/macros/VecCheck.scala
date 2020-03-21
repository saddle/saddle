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
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.saddle.macros.BinOps._

/**
  * Test on properties of Vec
  */
class VecCheck extends Specification with ScalaCheck {

  "Double Vec Tests" in {
    implicit val vecA = Arbitrary(VecArbitraries.vecDoubleWithNA)
    "update" in {
      forAll { (v: Vec[Double], d: Double) =>
        (v.length > 0) ==> {
          val i = v.length / 2
          v(i) = d
          (v.raw(i) must_== d) and
            (v.toArray(i) must_== d)
        }

      }
    }
    "update in slice" in {
      val v = org.saddle.vec.ones(10)
      val v2 = v.slice(2, 5)
      v2(1) = 0d
      v.toArray(3) must_== 0d
    }
    "update slice" in {
      val v = org.saddle.vec.ones(5)
      v(2 -> 3) = 0d
      v.toSeq must_== Seq(1d, 1d, 0d, 0d, 1d)
    }
    "update slice" in {
      val v = org.saddle.vec.ones(5)
      v(2 -> *) = 0d
      v.toSeq must_== Seq(1d, 1d, 0d, 0d, 0d)
    }
    "update slice with Vec" in {
      val v = org.saddle.vec.ones(5)
      v(2 -> *) = Vec(0d, 0d, 0d)
      v.toSeq must_== Seq(1d, 1d, 0d, 0d, 0d)
    }
    "update slice with Vec in slice" in {
      val v = Vec(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
      val v2 = v.slice(1, 8, 3)
      v2(1 -> 3) = Vec(-1, -1)
      v must_== Vec(0, 1, 2, 3, -1, 5, 6, -1, 8, 9)
    }
    "scalar operation in place works" in {
      forAll { (m: Vec[Double], b: Int) =>
        val m2 = m * b
        val m1 = m.copy
        m1 *= b
        m1 must_== m2
      }
    }
    "scalar operations on slice with Vec in slice" in {
      val v = Vec(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
      val v2 = v.slice(1, 8, 3)
      val v3 = v2.slice(1, 3)
      v3 *= 10
      v must_== Vec(0, 1, 2, 3, 40, 5, 6, 70, 8, 9)
    }
    "scalar operations on slice with Vec in slice" in {
      val v = Vec(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
      val v2 = v.slice(1, 8, 3)
      v2.take(1 -> 3) *= 10
      v must_== Vec(0, 1, 2, 3, 40, 5, 6, 70, 8, 9)
    }
    "vec operations on slice with Vec in slice" in {
      val v = Vec(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
      val v2 = v.slice(1, 8, 3)
      val v3 = v2.slice(1, 3)
      v3 *= Vec(-10, 10)
      v must_== Vec(0, 1, 2, 3, -40, 5, 6, 70, 8, 9)
    }
    "Elementwise vec operations with scalar (D,D) => D" in {
      "op + works" in {
        forAll { (m: Vec[Double], b: Double) => (m + b) must_== m.map(_ + b) }
      }
    }
    "Elementwise vec in place operations with scalar (D,D) => D" in {
      "op += works" in {
        forAll { (m: Vec[Double], b: Double) =>
          val m1 = m.copy
          m += b
          m must_== m1.map(_ + b)
        }
      }
    }
    "Elementwise vec in place operations with vec (D,D) => D" in {
      "op += works" in {
        forAll { (m: Vec[Double], b: Double) =>
          val mb = vec.zeros(m.length).map(_ + b)
          val m1 = m.copy
          m += mb
          m must_== m1.map(_ + b)
        }
      }
    }
    "Elementwise vec operations with vec (D,D) => D" in {
      "op + works" in {
        forAll { (m: Vec[Double], b: Double) =>
          val mb = vec.zeros(m.length).map(_ + b)
          (m + mb) must_== m.map(_ + b)
        }
      }
    }
  }

}
