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
package org.saddle.util

import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import org.scalacheck.Prop._
import org.saddle.Vec

/**
  * Tests for Concat
  */
class ConcatCheck extends Specification with ScalaCheck {

  "concat Boolean, Boolean" in {
    forAll { (a1: Array[Boolean], a2: Array[Boolean]) =>
      val res = Concat.append(a1, a2)
      val exp = a1 ++ a2
      res must_== exp
    }
  }

  // -----
  // Byte

  "concat Byte, Byte" in {
    forAll { (a1: Array[Byte], a2: Array[Byte]) =>
      val res = Concat.append(a1, a2)
      val exp = a1 ++ a2
      res must_== exp
    }
  }

  // -----
  // Char

  "concat Char, Char" in {
    forAll { (a1: Array[Char], a2: Array[Char]) =>
      val res = Concat.append(a1, a2)
      val exp = a1 ++ a2
      res must_== exp
    }
  }

  // ------
  // Short

  "concat Short, Short" in {
    forAll { (a1: Array[Short], a2: Array[Short]) =>
      val res = Concat.append(a1, a2)
      val exp = a1 ++ a2
      res must_== exp
    }
  }

  // ------
  // Int

  "concat Int, Int" in {
    forAll { (a1: Array[Int], a2: Array[Int]) =>
      val res = Concat.append(a1, a2)
      val exp = Vec(a1).map(_.toInt).contents ++ a2
      res must_== exp
    }
  }

  // ------
  // Float

  "concat Float, Float" in {
    forAll { (a1: Array[Float], a2: Array[Float]) =>
      val res = Concat.append(a1, a2)
      val exp = a1 ++ a2
      Vec(res) must_== Vec(exp) // must handle equality on NaN's properly
    }
  }

  // -----
  // Long

  "concat Long, Long" in {
    forAll { (a1: Array[Long], a2: Array[Long]) =>
      val res = Concat.append(a1, a2)
      val exp = a1 ++ a2
      res must_== exp
    }
  }

  // -----
  // Double

  "concat Double, Double" in {
    forAll { (a1: Array[Double], a2: Array[Double]) =>
      val res = Concat.append(a1, a2)
      val exp = a1 ++ a2
      Vec(res) must_== Vec(exp) // must handle equality on NaN's properly
    }
  }
}
