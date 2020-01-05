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
import org.saddle.framework._
import org.saddle.scalar.{ScalarTagLong => stL, ScalarTagInt => stI}
import org.saddle.macros.BinOps._

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

  "Elementwise matrix in place operations with scalar (D,D) => D" in {
    "op += works" in {
      forAll { (m: Mat[Double], b: Double) =>
        val m1 = m.copy
        m += b
        m must_== m1.map(_ + b)
      }
    }
  }
  "Elementwise matrix in place operations with matrix (D,D) => D" in {
    "op += works" in {
      forAll { (m: Mat[Double], b: Double) =>
        val mb = mat.zeros(m.numRows, m.numCols).map(_ + b)
        val m1 = m.copy
        m += mb
        m must_== m1.map(_ + b)
      }
    }
  }
  "Elementwise matrix operations with matrix (D,D) => D" in {
    "op + works" in {
      forAll { (m: Mat[Double], b: Double) =>
        val mb = mat.zeros(m.numRows, m.numCols).map(_ + b)
        (m + mb) must_== m.map(_ + b)
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
  }
}
