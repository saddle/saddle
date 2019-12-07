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

import mat.MatMath
import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import org.scalacheck.{Gen, Arbitrary}
import org.scalacheck.Prop._
import org.saddle.array._
import org.saddle.framework._
import org.saddle.scalar.{ScalarTagLong => stL, ScalarTagInt => stI}

/**
  * Test Mat
  */
class MatCheck extends Specification with ScalaCheck {

  "Elementwise matrix operations with scalar (D,D) => B" in {
    "op < works" in {
      forAll { (m: Mat[Double], b: Double) =>
        (m < b) must_== m.map(_ < b)
      }
    }
    "op <= works" in {
      forAll { (m: Mat[Double], b: Double) =>
        (m <= b) must_== m.map(_ <= b)
      }
    }
    "op > works" in {
      forAll { (m: Mat[Double], b: Double) =>
        (m > b) must_== m.map(_ > b)
      }
    }
    "op >= works" in {
      forAll { (m: Mat[Double], b: Double) =>
        (m >= b) must_== m.map(_ >= b)
      }
    }
    "op == works" in {
      forAll { (m: Mat[Double], b: Double) =>
        (m =? b) must_== m.map(_ == b)
      }
    }
    "op <> works" in {
      forAll { (m: Mat[Double], b: Double) =>
        (m <> b) must_== m.map(_ != b)
      }
    }
  }
  "Elementwise matrix operations with scalar (L,D) => B" in {
    "op < works" in {
      forAll { (m: Mat[Long], b: Double) =>
        (m < b) must_== m.map(_ < b)
      }
    }
    "op <= works" in {
      forAll { (m: Mat[Long], b: Double) =>
        (m <= b) must_== m.map(_ <= b)
      }
    }
    "op > works" in {
      forAll { (m: Mat[Long], b: Double) =>
        (m > b) must_== m.map(_ > b)
      }
    }
    "op >= works" in {
      forAll { (m: Mat[Long], b: Double) =>
        (m >= b) must_== m.map(_ >= b)
      }
    }
    "op == works" in {
      forAll { (m: Mat[Long], b: Double) =>
        (m =? b) must_== m.map(_ == b)
      }
    }
    "op <> works" in {
      forAll { (m: Mat[Long], b: Double) =>
        (m <> b) must_== m.map(_ != b)
      }
    }
  }
  "Elementwise matrix operations with scalar (I,D) => B" in {
    "op < works" in {
      forAll { (m: Mat[Int], b: Double) =>
        (m < b) must_== m.map(_ < b)
      }
    }
    "op <= works" in {
      forAll { (m: Mat[Int], b: Double) =>
        (m <= b) must_== m.map(_ <= b)
      }
    }
    "op > works" in {
      forAll { (m: Mat[Int], b: Double) =>
        (m > b) must_== m.map(_ > b)
      }
    }
    "op >= works" in {
      forAll { (m: Mat[Int], b: Double) =>
        (m >= b) must_== m.map(_ >= b)
      }
    }
    "op == works" in {
      forAll { (m: Mat[Int], b: Double) =>
        (m =? b) must_== m.map(_ == b)
      }
    }
    "op <> works" in {
      forAll { (m: Mat[Int], b: Double) =>
        (m <> b) must_== m.map(_ != b)
      }
    }
  }
  "Elementwise matrix operations with scalar (D,L) => B" in {
    "op < works" in {
      forAll { (m: Mat[Double], b: Long) =>
        (m < b) must_== m.map(v => if (stL.isMissing(b)) false else v < b)
      }
    }
    "op <= works" in {
      forAll { (m: Mat[Double], b: Long) =>
        (m <= b) must_== m.map(v => if (stL.isMissing(b)) false else v <= b)
      }
    }
    "op > works" in {
      forAll { (m: Mat[Double], b: Long) =>
        (m > b) must_== m.map(v => if (stL.isMissing(b)) false else v > b)
      }
    }
    "op >= works" in {
      forAll { (m: Mat[Double], b: Long) =>
        (m >= b) must_== m.map(v => if (stL.isMissing(b)) false else v >= b)
      }
    }
    "op == works" in {
      forAll { (m: Mat[Double], b: Long) =>
        (m =? b) must_== m.map(v => if (stL.isMissing(b)) false else v == b)
      }
    }
    "op <> works" in {
      forAll { (m: Mat[Double], b: Long) =>
        (m <> b) must_== m.map(v => if (stL.isMissing(b)) false else v != b)
      }
    }
  }
  "Elementwise matrix operations with scalar (D,I) => B" in {
    "op < works" in {
      forAll { (m: Mat[Double], b: Int) =>
        (m < b) must_== m.map(v => if (stI.isMissing(b)) false else v < b)
      }
    }
    "op <= works" in {
      forAll { (m: Mat[Double], b: Int) =>
        (m <= b) must_== m.map(v => if (stI.isMissing(b)) false else v <= b)
      }
    }
    "op > works" in {
      forAll { (m: Mat[Double], b: Int) =>
        (m > b) must_== m.map(v => if (stI.isMissing(b)) false else v > b)
      }
    }
    "op >= works" in {
      forAll { (m: Mat[Double], b: Int) =>
        (m >= b) must_== m.map(v => if (stI.isMissing(b)) false else v >= b)
      }
    }
    "op == works" in {
      forAll { (m: Mat[Double], b: Int) =>
        (m =? b) must_== m.map(v => if (stI.isMissing(b)) false else v == b)
      }
    }
    "op <> works" in {
      forAll { (m: Mat[Double], b: Int) =>
        (m <> b) must_== m.map(v => if (stI.isMissing(b)) false else v != b)
      }
    }
  }
  "Elementwise matrix operations with scalar (L,L) => B" in {
    "op < works" in {
      forAll { (m: Mat[Long], b: Long) =>
        (m < b) must_== m.map(v => if (stL.isMissing(b)) false else v < b)
      }
    }
    "op <= works" in {
      forAll { (m: Mat[Long], b: Long) =>
        (m <= b) must_== m.map(v => if (stL.isMissing(b)) false else v <= b)
      }
    }
    "op > works" in {
      forAll { (m: Mat[Long], b: Long) =>
        (m > b) must_== m.map(v => if (stL.isMissing(b)) false else v > b)
      }
    }
    "op >= works" in {
      forAll { (m: Mat[Long], b: Long) =>
        (m >= b) must_== m.map(v => if (stL.isMissing(b)) false else v >= b)
      }
    }
    "op == works" in {
      forAll { (m: Mat[Long], b: Long) =>
        (m =? b) must_== m.map(v => if (stL.isMissing(b)) false else v == b)
      }
    }
    "op <> works" in {
      forAll { (m: Mat[Long], b: Long) =>
        (m <> b) must_== m.map(v => if (stL.isMissing(b)) false else v != b)
      }
    }
  }
  "Elementwise matrix operations with scalar (I,I) => B" in {
    "op < works" in {
      forAll { (m: Mat[Int], b: Int) =>
        (m < b) must_== m.map(v => if (stI.isMissing(b)) false else v < b)
      }
    }
    "op <= works" in {
      forAll { (m: Mat[Int], b: Int) =>
        (m <= b) must_== m.map(v => if (stI.isMissing(b)) false else v <= b)
      }
    }
    "op > works" in {
      forAll { (m: Mat[Int], b: Int) =>
        (m > b) must_== m.map(v => if (stI.isMissing(b)) false else v > b)
      }
    }
    "op >= works" in {
      forAll { (m: Mat[Int], b: Int) =>
        (m >= b) must_== m.map(v => if (stI.isMissing(b)) false else v >= b)
      }
    }
    "op == works" in {
      forAll { (m: Mat[Int], b: Int) =>
        (m =? b) must_== m.map(v => if (stI.isMissing(b)) false else v == b)
      }
    }
    "op <> works" in {
      forAll { (m: Mat[Int], b: Int) =>
        (m <> b) must_== m.map(v => if (stI.isMissing(b)) false else v != b)
      }
    }
  }
  "Elementwise matrix operations with scalar (L,I) => B" in {
    "op < works" in {
      forAll { (m: Mat[Long], b: Int) =>
        (m < b) must_== m.map(v => if (stI.isMissing(b)) false else v < b)
      }
    }
    "op <= works" in {
      forAll { (m: Mat[Long], b: Int) =>
        (m <= b) must_== m.map(v => if (stI.isMissing(b)) false else v <= b)
      }
    }
    "op > works" in {
      forAll { (m: Mat[Long], b: Int) =>
        (m > b) must_== m.map(v => if (stI.isMissing(b)) false else v > b)
      }
    }
    "op >= works" in {
      forAll { (m: Mat[Long], b: Int) =>
        (m >= b) must_== m.map(v => if (stI.isMissing(b)) false else v >= b)
      }
    }
    "op == works" in {
      forAll { (m: Mat[Long], b: Int) =>
        (m =? b) must_== m.map(v => if (stI.isMissing(b)) false else v == b)
      }
    }
    "op <> works" in {
      forAll { (m: Mat[Long], b: Int) =>
        (m <> b) must_== m.map(v => if (stI.isMissing(b)) false else v != b)
      }
    }
  }

  "Elementwise matrix operations with scalar (D,D) => D" in {
    "op + works" in {
      forAll { (m: Mat[Double], b: Double) =>
        (m + b) must_== m.map(_ + b)
      }
    }
    "op - works" in {
      forAll { (m: Mat[Double], b: Double) =>
        (m - b) must_== m.map(_ - b)
      }
    }
    "op * works" in {
      forAll { (m: Mat[Double], b: Double) =>
        (m * b) must_== m.map(_ * b)
      }
    }
    "op / works" in {
      forAll { (m: Mat[Double], b: Double) =>
        (m / b) must_== m.map(_ / b)
      }
    }
    "op % works" in {
      forAll { (m: Mat[Double], b: Double) =>
        (m % b) must_== m.map(_ % b)
      }
    }
    "op ** works" in {
      forAll { (m: Mat[Double], b: Double) =>
        (m ** b) must_== m.map(v => math.pow(v, b))
      }
    }
  }
  "Elementwise matrix operations with scalar (L,D) => D" in {
    "op + works" in {
      forAll { (m: Mat[Long], b: Double) =>
        (m + b) must_== m.map(_ + b)
      }
    }
    "op - works" in {
      forAll { (m: Mat[Long], b: Double) =>
        (m - b) must_== m.map(_ - b)
      }
    }
    "op * works" in {
      forAll { (m: Mat[Long], b: Double) =>
        (m * b) must_== m.map(_ * b)
      }
    }
    "op / works" in {
      forAll { (m: Mat[Long], b: Double) =>
        (m / b) must_== m.map(_ / b)
      }
    }
    "op % works" in {
      forAll { (m: Mat[Long], b: Double) =>
        (m % b) must_== m.map(_ % b)
      }
    }
    "op ** works" in {
      forAll { (m: Mat[Long], b: Double) =>
        (m ** b) must_== m.map(v => math.pow(v, b))
      }
    }
  }
  "Elementwise matrix operations with scalar (I,D) => D" in {
    "op + works" in {
      forAll { (m: Mat[Int], b: Double) =>
        (m + b) must_== m.map(_ + b)
      }
    }
    "op - works" in {
      forAll { (m: Mat[Int], b: Double) =>
        (m - b) must_== m.map(_ - b)
      }
    }
    "op * works" in {
      forAll { (m: Mat[Int], b: Double) =>
        (m * b) must_== m.map(_ * b)
      }
    }
    "op / works" in {
      forAll { (m: Mat[Int], b: Double) =>
        (m / b) must_== m.map(_ / b)
      }
    }
    "op % works" in {
      forAll { (m: Mat[Int], b: Double) =>
        (m % b) must_== m.map(_ % b)
      }
    }
    "op ** works" in {
      forAll { (m: Mat[Int], b: Double) =>
        (m ** b) must_== m.map(v => math.pow(v, b))
      }
    }
  }
  "Elementwise matrix operations with scalar (D,L) => D" in {
    "op + works" in {
      forAll { (m: Mat[Double], b: Long) =>
        (m + b) must_== m.map(v => if (stL.isMissing(b)) Double.NaN else v + b)
      }
    }
    "op - works" in {
      forAll { (m: Mat[Double], b: Long) =>
        (m - b) must_== m.map(v => if (stL.isMissing(b)) Double.NaN else v - b)
      }
    }
    "op * works" in {
      forAll { (m: Mat[Double], b: Long) =>
        (m * b) must_== m.map(v => if (stL.isMissing(b)) Double.NaN else v * b)
      }
    }
    "op / works" in {
      forAll { (m: Mat[Double], b: Long) =>
        (m / b) must_== m.map(v => if (stL.isMissing(b)) Double.NaN else v / b)
      }
    }
    "op % works" in {
      forAll { (m: Mat[Double], b: Long) =>
        (m % b) must_== m.map(v => if (stL.isMissing(b)) Double.NaN else v % b)
      }
    }
    "op ** works" in {
      forAll { (m: Mat[Double], b: Long) =>
        (m ** b) must_== m.map(v =>
          if (stL.isMissing(b)) Double.NaN else math.pow(v, b)
        )
      }
    }
  }
  "Elementwise matrix operations with scalar (D,I) => D" in {
    "op + works" in {
      forAll { (m: Mat[Double], b: Int) =>
        (m + b) must_== m.map(v => if (stI.isMissing(b)) Double.NaN else v + b)
      }
    }
    "op - works" in {
      forAll { (m: Mat[Double], b: Int) =>
        (m - b) must_== m.map(v => if (stI.isMissing(b)) Double.NaN else v - b)
      }
    }
    "op * works" in {
      forAll { (m: Mat[Double], b: Int) =>
        (m * b) must_== m.map(v => if (stI.isMissing(b)) Double.NaN else v * b)
      }
    }
    "op / works" in {
      forAll { (m: Mat[Double], b: Int) =>
        (m / b) must_== m.map(v => if (stI.isMissing(b)) Double.NaN else v / b)
      }
    }
    "op % works" in {
      forAll { (m: Mat[Double], b: Int) =>
        (m % b) must_== m.map(v => if (stI.isMissing(b)) Double.NaN else v % b)
      }
    }
    "op ** works" in {
      forAll { (m: Mat[Double], b: Int) =>
        (m ** b) must_== m.map(v =>
          if (stI.isMissing(b)) Double.NaN else math.pow(v, b)
        )
      }
    }
  }
  "Elementwise matrix operations with scalar (L,L) => L" in {
    "op + works" in {
      forAll { (m: Mat[Long], b: Long) =>
        (m + b) must_== m.map(v => if (stL.isMissing(b)) stL.missing else v + b)
      }
    }
    "op - works" in {
      forAll { (m: Mat[Long], b: Long) =>
        (m - b) must_== m.map(v => if (stL.isMissing(b)) stL.missing else v - b)
      }
    }
    "op * works" in {
      forAll { (m: Mat[Long], b: Long) =>
        (m * b) must_== m.map(v => if (stL.isMissing(b)) stL.missing else v * b)
      }
    }
    "op / works" in {
      forAll { (m: Mat[Long], b: Long) =>
        (b != 0L) ==> {
          (m / b) must_== m.map(v =>
            if (stL.isMissing(b)) stL.missing else v / b
          )
        }
      }
    }
    "op % works" in {
      forAll { (m: Mat[Long], b: Long) =>
        (b != 0L) ==> {
          (m % b) must_== m.map(v =>
            if (stL.isMissing(b)) stL.missing else v % b
          )
        }
      }
    }
    "op ** works" in {
      forAll { (m: Mat[Long], b: Long) =>
        (m ** b) must_== m.map(v =>
          if (stL.isMissing(b)) stL.missing else math.pow(v, b).toLong
        )
      }
    }
    "op & works" in {
      forAll { (m: Mat[Long], b: Long) =>
        (m & b) must_== m.map(v => if (stL.isMissing(b)) stL.missing else v & b)
      }
    }
    "op | works" in {
      forAll { (m: Mat[Long], b: Long) =>
        (m | b) must_== m.map(v => if (stL.isMissing(b)) stL.missing else v | b)
      }
    }
    "op ^ works" in {
      forAll { (m: Mat[Long], b: Long) =>
        (m ^ b) must_== m.map(v => if (stL.isMissing(b)) stL.missing else v ^ b)
      }
    }
  }
  "Elementwise matrix operations with scalar (L,I) => L" in {
    "op + works" in {
      forAll { (m: Mat[Long], b: Int) =>
        (m + b) must_== m.map(v => if (stI.isMissing(b)) stL.missing else v + b)
      }
    }
    "op - works" in {
      forAll { (m: Mat[Long], b: Int) =>
        (m - b) must_== m.map(v => if (stI.isMissing(b)) stL.missing else v - b)
      }
    }
    "op * works" in {
      forAll { (m: Mat[Long], b: Int) =>
        (m * b) must_== m.map(v => if (stI.isMissing(b)) stL.missing else v * b)
      }
    }
    "op / works" in {
      forAll { (m: Mat[Long], b: Int) =>
        (b != 0L) ==> {
          (m / b) must_== m.map(v =>
            if (stI.isMissing(b)) stL.missing else v / b
          )
        }
      }
    }
    "op % works" in {
      forAll { (m: Mat[Long], b: Int) =>
        (b != 0L) ==> {
          (m % b) must_== m.map(v =>
            if (stI.isMissing(b)) stL.missing else v % b
          )
        }
      }
    }
    "op ** works" in {
      forAll { (m: Mat[Long], b: Int) =>
        (m ** b) must_== m.map(v =>
          if (stI.isMissing(b)) stL.missing else math.pow(v, b).toLong
        )
      }
    }
    "op & works" in {
      forAll { (m: Mat[Long], b: Int) =>
        (m & b) must_== m.map(v => if (stI.isMissing(b)) stL.missing else v & b)
      }
    }
    "op | works" in {
      forAll { (m: Mat[Long], b: Int) =>
        (m | b) must_== m.map(v => if (stI.isMissing(b)) stL.missing else v | b)
      }
    }
    "op ^ works" in {
      forAll { (m: Mat[Long], b: Int) =>
        (m ^ b) must_== m.map(v => if (stI.isMissing(b)) stL.missing else v ^ b)
      }
    }
  }
  "Elementwise matrix operations with scalar (I,L) => L" in {
    "op + works" in {
      forAll { (m: Mat[Int], b: Long) =>
        (m + b) must_== m.map(v => if (stL.isMissing(b)) stL.missing else v + b)
      }
    }
    "op - works" in {
      forAll { (m: Mat[Int], b: Long) =>
        (m - b) must_== m.map(v => if (stL.isMissing(b)) stL.missing else v - b)
      }
    }
    "op * works" in {
      forAll { (m: Mat[Int], b: Long) =>
        (m * b) must_== m.map(v => if (stL.isMissing(b)) stL.missing else v * b)
      }
    }
    "op / works" in {
      forAll { (m: Mat[Int], b: Long) =>
        (b != 0L) ==> {
          (m / b) must_== m.map(v =>
            if (stL.isMissing(b)) stL.missing else v / b
          )
        }
      }
    }
    "op % works" in {
      forAll { (m: Mat[Int], b: Long) =>
        (b != 0L) ==> {
          (m % b) must_== m.map(v =>
            if (stL.isMissing(b)) stL.missing else v % b
          )
        }
      }
    }
    "op ** works" in {
      forAll { (m: Mat[Int], b: Long) =>
        (m ** b) must_== m.map(v =>
          if (stL.isMissing(b)) stL.missing else math.pow(v, b).toLong
        )
      }
    }
    "op & works" in {
      forAll { (m: Mat[Int], b: Long) =>
        (m & b) must_== m.map(v => if (stL.isMissing(b)) stL.missing else v & b)
      }
    }
    "op | works" in {
      forAll { (m: Mat[Int], b: Long) =>
        (m | b) must_== m.map(v => if (stL.isMissing(b)) stL.missing else v | b)
      }
    }
    "op ^ works" in {
      forAll { (m: Mat[Int], b: Long) =>
        (m ^ b) must_== m.map(v => if (stL.isMissing(b)) stL.missing else v ^ b)
      }
    }
  }
  "Elementwise matrix operations with scalar (I,I) => I" in {
    "op + works" in {
      forAll { (m: Mat[Int], b: Int) =>
        (m + b) must_== m.map(v => if (stI.isMissing(b)) stI.missing else v + b)
      }
    }
    "op - works" in {
      forAll { (m: Mat[Int], b: Int) =>
        (m - b) must_== m.map(v => if (stI.isMissing(b)) stI.missing else v - b)
      }
    }
    "op * works" in {
      forAll { (m: Mat[Int], b: Int) =>
        (m * b) must_== m.map(v => if (stI.isMissing(b)) stI.missing else v * b)
      }
    }
    "op / works" in {
      forAll { (m: Mat[Int], b: Int) =>
        (b != 0L) ==> {
          (m / b) must_== m.map(v =>
            if (stI.isMissing(b)) stI.missing else v / b
          )
        }
      }
    }
    "op % works" in {
      forAll { (m: Mat[Int], b: Int) =>
        (b != 0L) ==> {
          (m % b) must_== m.map(v =>
            if (stI.isMissing(b)) stI.missing else v % b
          )
        }
      }
    }
    "op ** works" in {
      forAll { (m: Mat[Int], b: Int) =>
        (m ** b) must_== m.map(v =>
          if (stI.isMissing(b)) stI.missing else math.pow(v, b).toInt
        )
      }
    }
    "op & works" in {
      forAll { (m: Mat[Int], b: Int) =>
        (m & b) must_== m.map(v => if (stI.isMissing(b)) stI.missing else v & b)
      }
    }
    "op | works" in {
      forAll { (m: Mat[Int], b: Int) =>
        (m | b) must_== m.map(v => if (stI.isMissing(b)) stI.missing else v | b)
      }
    }
    "op ^ works" in {
      forAll { (m: Mat[Int], b: Int) =>
        (m ^ b) must_== m.map(v => if (stI.isMissing(b)) stI.missing else v ^ b)
      }
    }
    "op << works" in {
      forAll { (m: Mat[Int], b: Int) =>
        (m << b) must_== m.map(v =>
          if (stI.isMissing(b)) stI.missing else v << b
        )
      }
    }
    "op >> works" in {
      forAll { (m: Mat[Int], b: Int) =>
        (m >> b) must_== m.map(v =>
          if (stI.isMissing(b)) stI.missing else v >> b
        )
      }
    }
    "op >>> works" in {
      forAll { (m: Mat[Int], b: Int) =>
        (m >>> b) must_== m.map(v =>
          if (stI.isMissing(b)) stI.missing else v >>> b
        )
      }
    }
  }
  "Elementwise matrix operations with scalar (B,B) => B" in {
    "op && works" in {
      forAll { (m: Mat[Boolean], b: Boolean) =>
        (m && b) must_== m.map(_ && b)
      }
    }
    "op || works" in {
      forAll { (m: Mat[Boolean], b: Boolean) =>
        (m || b) must_== m.map(_ || b)
      }
    }
    "op xor works" in {
      forAll { (m: Mat[Boolean], b: Boolean) =>
        (m xor b) must_== m.map(a => (a && b) || (!a && !b))
      }
    }
  }

  "Double Mat Tests" in {
    implicit val arbMat = Arbitrary(MatArbitraries.matDouble)
    "scalar operation in place works" in {
      forAll { (m: Mat[Double], b: Int) =>
        val m2 = m * b
        val m1 = m.copy
        m1 *= b
        m1 must_== m2
      }
    }
    "broadcast in place works" in {
      val m1 = Mat(Vec(1, 2, 3), Vec(1, 2, 3), Vec(1, 2, 3)).T
      val m2 = Mat(Vec(10, 20, 30))
      val m3 = m1 + m2
      val m4 = Mat(Vec(11, 21, 31), Vec(12, 22, 32), Vec(13, 23, 33))
      m1 += m2
      (m1 must_== m3) and (m1 must_== m4)
    }
    "broadcast works" in {
      val m1 = Mat(Vec(1, 2, 3)).T
      val m2 = Mat(Vec(10, 20, 30))
      val m3 = Mat(Vec(11, 21, 31), Vec(12, 22, 32), Vec(13, 23, 33))
      (m1 + m2) must_== m3
    }
    "equality works" in {
      forAll { (m: Mat[Double]) =>
        (m must_== Mat(m.numRows, m.numCols, m.toArray)) and (m must_== m)
      }
    }

    "map works" in {
      forAll { (m: Mat[Double]) =>
        val res = m.map(_ + 1)
        val exp = m.contents.map(_ + 1)
        res.contents must_== exp
      }
    }
    "map rows works" in {
      forAll { (m: Mat[Double]) =>
        val res = m.mapRows((v, i) => v + i)
        val exp = m.rows.zipWithIndex.map(v => v._1 + v._2).toMat.T
        res must_== exp
      }
    }
    "mutate rows works" in {
      forAll { (m: Mat[Double]) =>
        val res = m.copy
        res.mutateRows[Double]((v, i) => v + i)
        val exp = m.rows.zipWithIndex.map(v => v._1 + v._2).toMat.T
        res must_== exp
      }
    }
    "map cols works" in {
      forAll { (m: Mat[Double]) =>
        val res = m.mapCols((v, i) => v + i)
        val exp = m.cols.zipWithIndex.map(v => v._1 + v._2).toMat
        res must_== exp
      }
    }
    "mutate cols works" in {
      forAll { (m: Mat[Double]) =>
        val res = m.copy
        res.mutateCols[Double]((v, i) => v + i)
        val exp = m.cols.zipWithIndex.map(v => v._1 + v._2).toMat
        res must_== exp
      }
    }

    "reshape works" in {
      forAll { (m: Mat[Double]) =>
        val res = m.reshape(m.numCols, m.numRows)
        res.contents must_== m.contents
        res.numCols must_== m.numRows
        res.numRows must_== m.numCols
      }
    }

    "isSquare works" in {
      forAll { (m: Mat[Double]) =>
        m.isSquare must_== (m.numRows == m.numCols)
      }
    }

    "map works" in {
      forAll { (m: Mat[Double]) =>
        val data = m.contents
        m.map(_ + 1.0) must_== Mat(m.numRows, m.numCols, data.map(_ + 1.0))
        m.map(_ => 5.0) must_== Mat(
          m.numRows,
          m.numCols,
          (data.map(d => if (d.isNaN) na.to[Double] else 5.0))
        )
        m.map(_ => 5) must_== Mat[Int](
          m.numRows,
          m.numCols,
          data.map(d => if (d.isNaN) na.to[Int] else 5)
        )
      }
    }

    "transpose works" in {
      implicit val arbMat = Arbitrary(MatArbitraries.matDoubleWithNA)

      forAll { (m: Mat[Double]) =>
        val res = m.T
        res.numCols must_== m.numRows
        res.numRows must_== m.numCols
        for (i <- Range(0, m.numRows); j <- Range(0, m.numCols))
          m.at(i, j) must_== res.at(j, i)
        res.T must_== m
      }
    }

    "takeRows works" in {
      forAll { (m: Mat[Double]) =>
        (m.numRows > 0) ==> {
          val idx = Gen.listOfN(3, Gen.choose[Int](0, m.numRows - 1))
          forAll(idx) { i =>
            val res = m.takeRows(i: _*)
            res.numRows must_== i.size
            val exp = for (j <- i) yield m.row(j)
            res must_== Mat(exp: _*).T
          }
        }
      }
    }
    "takeRows works - 2" in {
      forAll { (m: Mat[Double]) =>
        (m.numRows > 0) ==> {
          val idx = Gen.listOfN(3, Gen.choose[Int](0, m.numRows - 1))
          forAll(idx) { i =>
            val res = m.takeRows(i.toVec)
            res.numRows must_== i.size
            val exp = for (j <- i) yield m.row(j)
            res must_== Mat(exp: _*).T
          }
        }
      }
    }

    "takeCols works" in {
      forAll { (m: Mat[Double]) =>
        (m.numRows > 0) ==> {
          val idx = Gen.listOfN(3, Gen.choose[Int](0, m.numCols - 1))
          forAll(idx) { i =>
            val res = m.takeCols(i: _*)
            res.numCols must_== i.size
            val exp = for (j <- i) yield m.col(j)
            res must_== Mat(exp: _*)
          }
        }
      }
    }

    "withoutRows works" in {
      forAll { (m: Mat[Double]) =>
        (m.numRows > 0) ==> {
          val idx = Gen.listOfN(3, Gen.choose[Int](0, m.numRows - 1))
          forAll(idx) { i =>
            val loc = Set(i: _*)
            val res = m.withoutRows(i: _*)
            res.numRows must_== (m.numRows - loc.size)
            val exp =
              for (j <- 0 until m.numRows if !loc.contains(j)) yield m.row(j)
            res must_== Mat(exp: _*).T
          }
        }
      }
    }

    "withoutCols works" in {
      forAll { (m: Mat[Double]) =>
        (m.numCols > 0) ==> {
          val idx = Gen.listOfN(3, Gen.choose[Int](0, m.numCols - 1))
          forAll(idx) { i =>
            val loc = Set(i: _*)
            val res = m.withoutCols(i: _*)
            res.numCols must_== (m.numCols - loc.size)
            val exp =
              for (j <- 0 until m.numCols if !loc.contains(j)) yield m.col(j)
            res must_== Mat(exp: _*)
          }
        }
      }
    }

    "rowsWithNA works (no NA)" in {
      forAll { (m: Mat[Double]) =>
        m.rowsWithNA must_== Set.empty[Double]
      }
    }

    "rowsWithNA works (with NA)" in {
      implicit val arbMat = Arbitrary(MatArbitraries.matDoubleWithNA)
      forAll { (m: Mat[Double]) =>
        val exp = (m.rows() zip Range(0, m.numRows)).flatMap {
          case (a: Vec[_], b: Int) => if (a.hasNA) Some(b) else None
        }
        m.rowsWithNA must_== exp.toSet
      }
    }

    "dropRowsWithNA works" in {
      implicit val arbMat = Arbitrary(MatArbitraries.matDoubleWithNA)
      forAll { (m: Mat[Double]) =>
        m.dropRowsWithNA must_== m.toFrame.rdropNA.toMat
      }
    }

    "dropColsWithNA works" in {
      implicit val arbMat = Arbitrary(MatArbitraries.matDoubleWithNA)
      forAll { (m: Mat[Double]) =>
        m.dropColsWithNA must_== m.toFrame.dropNA.toMat
      }
    }

    "cols works" in {
      forAll { (m: Mat[Double]) =>
        val data = m.T.contents
        val exp =
          for (i <- IndexedSeq(Range(0, m.numCols): _*))
            yield Vec(data).slice(i * m.numRows, (i + 1) * m.numRows)
        m.cols() must_== exp
      }
    }

    "rows works" in {
      forAll { (m: Mat[Double]) =>
        val data = m.contents
        val exp =
          for (i <- IndexedSeq(Range(0, m.numRows): _*))
            yield Vec(data).slice(i * m.numCols, (i + 1) * m.numCols)
        m.rows() must_== exp
      }
    }

    "col works" in {
      forAll { (m: Mat[Double]) =>
        (m.numCols > 0) ==> {
          val idx = Gen.choose(0, m.numCols - 1)
          val data = m.T.contents
          forAll(idx) { i =>
            m.col(i) must_== Vec(data).slice(i * m.numRows, (i + 1) * m.numRows)
          }
        }
      }
    }

    "row works" in {
      forAll { (m: Mat[Double]) =>
        (m.numRows > 0) ==> {
          val idx = Gen.choose(0, m.numRows - 1)
          val data = m.contents
          forAll(idx) { i =>
            m.row(i) must_== Vec(data).slice(i * m.numCols, (i + 1) * m.numCols)
          }
        }
      }
    }

    "roundTo works" in {
      forAll { (ma: Mat[Double]) =>
        ma.contents.map((v: Double) => math.round(v * 100) / 100d) must_== ma
          .roundTo(2)
          .contents
      }
    }
    "isEmpty works" in {
      forAll { (ma: Mat[Double]) =>
        ma.isEmpty must_== (ma.toArray.isEmpty || ma.numCols == 0 || ma.numRows == 0)
      }
    }
    "at works" in {
      forAll { (ma: Mat[Double], i: Int) =>
        ma.length == 0 || i < 0 || ma.length <= i || ma.at(i).isNA || ma
          .at(i)
          .get == ma.raw(i)
      }
    }
    "at works" in {
      val m = Mat(Vec(1, 2, 3), Vec(4, 5, 6), Vec(7, 8, 9))
      m.at(Array(1, 2), Array(0, 1)) must_== Mat(Vec(2, 3), Vec(5, 6))
    }
    "at works" in {
      val m = Mat(Vec(1, 2, 3), Vec(4, 5, 6), Vec(7, 8, 9))
      m.at(1 -> *, 0 -> *) must_== Mat(Vec(2, 3), Vec(5, 6), Vec(8, 9))
    }
    "at works" in {
      val m = Mat(Vec(1, 2, 3), Vec(4, 5, 6), Vec(7, 8, 9))
      m.at(Array(0, 1, 2), 2) must_== Vec(7, 8, 9)
    }
    "at works" in {
      val m = Mat(Vec(1, 2, 3), Vec(4, 5, 6), Vec(7, 8, 9))
      m.at(2, Array(0, 1, 2)) must_== Vec(3, 6, 9)
    }
    "cols works" in {
      forAll { (ma: Mat[Double], i: Int) =>
        ma.length == 0 || i < 0 || ma.numCols <= i || ma.cols(Vector(i)) == Vector(
          ma.cols()(i)
        )
      }
    }
    "rows works" in {
      forAll { (ma: Mat[Double], i: Int) =>
        ma.length == 0 || i < 0 || ma.numRows <= i || ma.rows(Vector(i)) == Vector(
          ma.rows()(i)
        )
      }
    }

    "cov works" in {
      forAll { (ma: Mat[Double]) =>
        import org.apache.commons.math.stat.correlation.Covariance

        if (ma.numRows < 2 || ma.numCols < 2) {
          MatMath.cov(ma) must throwAn[IllegalArgumentException]
        } else {
          val aCov = new Covariance(ma.rows().map(_.toArray).toArray)
          val exp = aCov.getCovarianceMatrix
          val res = MatMath.cov(ma).contents

          Vec(res) must BeCloseToVec(Vec(flatten(exp.getData)), 1e-9)
        }
      }
    }
    "setCell works" in {
      forAll { (m: Mat[Double]) =>
        (m.numRows > 0 && m.numCols > 0) ==> {

          m.mutateSetCell(0, 0, 3.0)
          m.raw(0, 0) must_== 3.0
        }
      }
    }
    "setCell works" in {
      forAll { (m: Mat[Double]) =>
        (m.numRows > 0 && m.numCols > 0) ==> {

          m(0, 0) = 3.0
          m.raw(0, 0) must_== 3.0
        }
      }
    }
    "setRow works" in {
      forAll { (m: Mat[Double]) =>
        (m.numRows > 0 && m.numCols > 0) ==> {
          m.mutateSetRow(0, 3.0)
          m.row(0) must_== vec.zeros(m.numCols) + 3d
        }
      }
    }
    "setCol works" in {
      forAll { (m: Mat[Double]) =>
        (m.numRows > 0 && m.numCols > 0) ==> {
          m.mutateSetColumn(0, 3.0)
          m.col(0) must_== vec.zeros(m.numRows) + 3d
        }
      }
    }
    "setDiagonal works" in {
      forAll { (m: Mat[Double]) =>
        (m.numRows > 0 && m.numCols > 0) ==> {
          m.mutateSetDiagonal(3.0)
          (for (i <- 0 until math.min(m.numRows, m.numCols))
            yield m.raw(i, i)).toVec must_== vec.zeros(
            math.min(m.numRows, m.numCols)
          ) + 3d
        }
      }
    }
    "setLowerTriangle works" in {
      forAll { (m: Mat[Double]) =>
        (m.numRows > 0 && m.numCols > 0) ==> {
          val cl = m.copy
          m.mutateSetLowerTriangle(3.0)
          val lower =
            (for {
              i <- 0 until math.min(m.numRows, m.numCols)
              j <- 0 until i
            } yield m.raw(i, j)).toSeq.distinct must_== Seq.fill(
              if (math.min(m.numRows, m.numCols) < 2) 0 else 1
            )(3d)
          val upper = for {
            i <- 0 until math.min(m.numRows, m.numCols)
            j <- i until math.min(m.numRows, m.numCols)
          } yield m.raw(i, j) == cl.raw(i, j)

          lower and upper.forall(identity)
        }
      }
    }
    "setUpperTriangle works" in {
      forAll { (m: Mat[Double]) =>
        (m.numRows > 0 && m.numCols > 0) ==> {
          val cl = m.copy
          m.mutateSetUpperTriangle(3.0)
          val upper =
            (for {
              i <- 0 until math.min(m.numRows, m.numCols)
              j <- (i + 1) until math.min(m.numRows, m.numCols)
            } yield m.raw(i, j)).toSeq.distinct must_== Seq.fill(
              if (math.min(m.numRows, m.numCols) < 2) 0 else 1
            )(3d)
          val lower = for {
            i <- 0 until math.min(m.numRows, m.numCols)
            j <- 0 to i
          } yield m.raw(i, j) == cl.raw(i, j)
          upper and lower.forall(identity)
        }
      }
    }
  }

}
