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

  "concat Byte, Boolean" in {
    forAll { (a1: Array[Byte], a2: Array[Boolean]) =>
      val res = Concat.append(a1, a2)
      val exp = a1 ++ Vec(a2).map(if(_) 1.toByte else 0.toByte).contents
      res must_== exp
    }
  }

  "concat Char, Boolean" in {
    forAll { (a1: Array[Char], a2: Array[Boolean]) =>
      val res = Concat.append(a1, a2)
      val exp = a1 ++ Vec(a2).map(if(_) 1.toChar else 0.toChar).contents
      res must_== exp
    }
  }

  "concat Short, Boolean" in {
    forAll { (a1: Array[Short], a2: Array[Boolean]) =>
      val res = Concat.append(a1, a2)
      val exp = a1 ++ Vec(a2).map(if(_) 1.toShort else 0.toShort).contents
      res must_== exp
    }
  }

  "concat Int, Boolean" in {
    forAll { (a1: Array[Int], a2: Array[Boolean]) =>
      val res = Concat.append(a1, a2)
      val exp = a1 ++ Vec(a2).map(if(_) 1 else 0).contents
      res must_== exp
    }
  }

  "concat Float, Boolean" in {
    forAll { (a1: Array[Float], a2: Array[Boolean]) =>
      val res = Concat.append(a1, a2)
      val exp = a1 ++ Vec(a2).map(if(_) 1.toFloat else 0.toFloat).contents
      res must_== exp
    }
  }

  "concat Long, Boolean" in {
    forAll { (a1: Array[Long], a2: Array[Boolean]) =>
      val res = Concat.append(a1, a2)
      val exp = a1 ++ Vec(a2).map(if(_) 1.toLong else 0.toLong).contents
      res must_== exp
    }
  }

  "concat Double, Boolean" in {
    forAll { (a1: Array[Double], a2: Array[Boolean]) =>
      val res = Concat.append(a1, a2)
      val exp = a1 ++ Vec(a2).map(if(_) 1.toDouble else 0.toDouble).contents
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

  "concat Char, Byte" in {
    forAll { (a1: Array[Char], a2: Array[Byte]) =>
      val res = Concat.append(a1, a2)
      val exp = a1 ++ Vec(a2).map(_.toChar).contents
      res must_== exp
    }
  }

  "concat Short, Byte" in {
    forAll { (a1: Array[Short], a2: Array[Byte]) =>
      val res = Concat.append(a1, a2)
      val exp = a1 ++ Vec(a2).map(_.toShort).contents
      res must_== exp
    }
  }

  "concat Int, Byte" in {
    forAll { (a1: Array[Int], a2: Array[Byte]) =>
      val res = Concat.append(a1, a2)
      val exp = a1 ++ Vec(a2).map(_.toInt).contents
      res must_== exp
    }
  }

  "concat Float, Byte" in {
    forAll { (a1: Array[Float], a2: Array[Byte]) =>
      val res = Concat.append(a1, a2)
      val exp = a1 ++ Vec(a2).map(_.toFloat).contents
      Vec(res) must_== Vec(exp)
    }
  }

  "concat Long, Byte" in {
    forAll { (a1: Array[Long], a2: Array[Byte]) =>
      val res = Concat.append(a1, a2)
      val exp = a1 ++ Vec(a2).map(_.toLong).contents
      res must_== exp
    }
  }

  "concat Double, Byte" in {
    forAll { (a1: Array[Double], a2: Array[Byte]) =>
      val res = Concat.append(a1, a2)
      val exp = a1 ++ Vec(a2).map(_.toDouble).contents
      Vec(res) must_== Vec(exp)
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

  "concat Short, Char" in {
    forAll { (a1: Array[Short], a2: Array[Char]) =>
      val res = Concat.append(a1, a2)
      val exp = a1 ++ Vec(a2).map(_.toShort).contents
      res must_== exp
    }
  }

  "concat Int, Char" in {
    forAll { (a1: Array[Int], a2: Array[Char]) =>
      val res = Concat.append(a1, a2)
      val exp = a1 ++ Vec(a2).map(_.toInt).contents
      res must_== exp
    }
  }

  "concat Float, Char" in {
    forAll { (a1: Array[Float], a2: Array[Char]) =>
      val res = Concat.append(a1, a2)
      val exp = a1 ++ Vec(a2).map(_.toFloat).contents
      Vec(res) must_== Vec(exp)
    }
  }

  "concat Long, Char" in {
    forAll { (a1: Array[Long], a2: Array[Char]) =>
      val res = Concat.append(a1, a2)
      val exp = a1 ++ Vec(a2).map(_.toLong).contents
      res must_== exp
    }
  }

  "concat Double, Char" in {
    forAll { (a1: Array[Double], a2: Array[Char]) =>
      val res = Concat.append(a1, a2)
      val exp = a1 ++ Vec(a2).map(_.toDouble).contents
      Vec(res) must_== Vec(exp)
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

  "concat Int, Short" in {
    forAll { (a1: Array[Int], a2: Array[Short]) =>
      val res = Concat.append(a1, a2)
      val exp = a1 ++ Vec(a2).map(_.toInt).contents
      res must_== exp
    }
  }

  "concat Float, Short" in {
    forAll { (a1: Array[Float], a2: Array[Short]) =>
      val res = Concat.append(a1, a2)
      val exp = a1 ++ Vec(a2).map(_.toFloat).contents
      Vec(res) must_== Vec(exp)
    }
  }

  "concat Long, Short" in {
    forAll { (a1: Array[Long], a2: Array[Short]) =>
      val res = Concat.append(a1, a2)
      val exp = a1 ++ Vec(a2).map(_.toLong).contents
      res must_== exp
    }
  }

  "concat Double, Short" in {
    forAll { (a1: Array[Double], a2: Array[Short]) =>
      val res = Concat.append(a1, a2)
      val exp = a1 ++ Vec(a2).map(_.toDouble).contents
      Vec(res) must_== Vec(exp)
    }
  }

  // ------
  // Int
  
  "concat Int, Int" in {
    forAll { (a1: Array[Char], a2: Array[Int]) =>
      val res = Concat.append(a1, a2)
      val exp = Vec(a1).map(_.toInt).contents ++ a2
      res must_== exp
    }
  }


  "concat Float, Int" in {
    forAll { (a1: Array[Float], a2: Array[Int]) =>
      val res = Concat.append(a1, a2)
      val exp = a1 ++ Vec(a2).map(_.toFloat).contents
      Vec(res) must_== Vec(exp)   // must handle equality on NaN's properly
    }
  }

  "concat Long, Int" in {
    forAll { (a1: Array[Long], a2: Array[Int]) =>
      val res = Concat.append(a1, a2)
      val exp = a1 ++ Vec(a2).map(_.toLong).contents
      res must_== exp
    }
  }

  "concat Double, Int" in {
    forAll { (a1: Array[Double], a2: Array[Int]) =>
      val res = Concat.append(a1, a2)
      val exp = a1 ++ Vec(a2).map(_.toDouble).contents
      Vec(res) must_== Vec(exp)   // must handle equality on NaN's properly
    }
  }

  // ------
  // Float

  "concat Float, Float" in {
    forAll { (a1: Array[Float], a2: Array[Float]) =>
      val res = Concat.append(a1, a2)
      val exp = a1 ++ a2
      Vec(res) must_== Vec(exp)   // must handle equality on NaN's properly
    }
  }

  "concat Double, Float" in {
    forAll { (a1: Array[Double], a2: Array[Float]) =>
      val res = Concat.append(a1, a2)
      val exp = a1 ++ Vec(a2).map(_.toDouble).contents
      Vec(res) must_== Vec(exp)   // must handle equality on NaN's properly
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

  "concat Double, Long" in {
    forAll { (a1: Array[Double], a2: Array[Long]) =>
      val res = Concat.append(a1, a2)
      val exp = a1 ++ Vec(a2).map(_.toDouble).contents
      Vec(res) must_== Vec(exp)   // must handle equality on NaN's properly
    }
  }

  // -----
  // Double

  "concat Double, Double" in {
    forAll { (a1: Array[Double], a2: Array[Double]) =>
      val res = Concat.append(a1, a2)
      val exp = a1 ++ a2
      Vec(res) must_== Vec(exp)   // must handle equality on NaN's properly
    }
  }
}
