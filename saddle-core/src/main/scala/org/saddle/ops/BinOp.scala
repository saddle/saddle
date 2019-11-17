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
package org.saddle.ops

import annotation.implicitNotFound

import scala.{specialized => spec}
import org.saddle.scalar.{
  ScalarTagDouble => stD,
  ScalarTagLong => stL,
  ScalarTagInt => stI,
  ScalarTagBool => stB
}

/**
  * Concrete implementations of BinOp provide primitive-specialized support for performing
  * binary operations on elements of the following objects, as well as the objects themselves:
  *
  *   - [[org.saddle.Vec]]
  *   - [[org.saddle.Series]]
  *   - [[org.saddle.Mat]]
  *   - [[org.saddle.Frame]]
  *
  * Appropriate BinOp instances are made available in an implicit context in order for the
  * [[org.saddle.ops.NumericOps]] methods inherited by the structures above to operate in a
  * seamless fashion.
  *
  * For example:
  *
  * {{{
  *   Vec(1,2,3) * Vec(4,5,6) == Vec(4,10,18)
  * }}}
  *
  * The multiplication above relies on two BinOp implementations: the first is BinOp[Multiply, Vec, Vec, Vec],
  * whose implementation in turn relies on BinOp[Multiply, Int, Int, Int].
  */
@implicitNotFound(
  msg =
    "No BinOp ${O} instance available to operate on values of type ${X} and ${Y}"
)
trait BinOp[
    O <: OpType,
    @spec(Boolean, Int, Long, Double) -X,
    @spec(Boolean, Int, Long, Double) -Y,
    @spec(Boolean, Int, Long, Double) +Z
] {
  def apply(a: X, b: Y): Z
}

trait BinOpInPlace[
    O <: OpType,
    @spec(Boolean, Int, Long, Double) -X,
    @spec(Boolean, Int, Long, Double) -Y
] {
  def apply(a: X, b: Y): Unit
}

/**
  * Contains implementations of primitive binary ops that are NA-aware
  *
  * Double primitive has NA bit pattern baked into its representation, but
  * for others we must check for the appropriate sentinel value.
  *
  * Note scala.Function2 is not specialized on Boolean inputs, only output
  */
object BinOp {
  // ********************************************************
  // ** Concrete implementations necessary for specialization
  // ********************************************************

  // (D,D) => D

  implicit val powDD = new BinOp[Power, Double, Double, Double] {
    def apply(a: Double, b: Double): Double =
      if (stD.isMissing(a) || stD.isMissing(b)) stD.missing
      else math.pow(a, b)
  }
  implicit val modDD = new BinOp[Mod, Double, Double, Double] {
    def apply(a: Double, b: Double): Double =
      if (stD.isMissing(a) || stD.isMissing(b)) stD.missing
      else a % b
  }
  implicit val addDD = new BinOp[Add, Double, Double, Double] {
    def apply(a: Double, b: Double): Double =
      if (stD.isMissing(a) || stD.isMissing(b)) stD.missing
      else a + b
  }
  implicit val mulDD = new BinOp[Multiply, Double, Double, Double] {
    def apply(a: Double, b: Double): Double =
      if (stD.isMissing(a) || stD.isMissing(b)) stD.missing
      else a * b
  }
  implicit val divDD = new BinOp[Divide, Double, Double, Double] {
    def apply(a: Double, b: Double): Double =
      if (stD.isMissing(a) || stD.isMissing(b)) stD.missing
      else a / b
  }
  implicit val subDD = new BinOp[Subtract, Double, Double, Double] {
    def apply(a: Double, b: Double): Double =
      if (stD.isMissing(a) || stD.isMissing(b)) stD.missing
      else a - b
  }

  // (D,L) => D

  implicit val powDL = new BinOp[Power, Double, Long, Double] {
    def apply(a: Double, b: Long): Double =
      if (stD.isMissing(a) || stL.isMissing(b)) stD.missing
      else math.pow(a, b)
  }
  implicit val modDL = new BinOp[Mod, Double, Long, Double] {
    def apply(a: Double, b: Long): Double =
      if (stD.isMissing(a) || stL.isMissing(b)) stD.missing
      else a % b
  }
  implicit val addDL = new BinOp[Add, Double, Long, Double] {
    def apply(a: Double, b: Long): Double =
      if (stD.isMissing(a) || stL.isMissing(b)) stD.missing
      else a + b
  }
  implicit val mulDL = new BinOp[Multiply, Double, Long, Double] {
    def apply(a: Double, b: Long): Double =
      if (stD.isMissing(a) || stL.isMissing(b)) stD.missing
      else a * b
  }
  implicit val divDL = new BinOp[Divide, Double, Long, Double] {
    def apply(a: Double, b: Long): Double =
      if (stD.isMissing(a) || stL.isMissing(b)) stD.missing
      else a / b
  }
  implicit val subDL = new BinOp[Subtract, Double, Long, Double] {
    def apply(a: Double, b: Long): Double =
      if (stD.isMissing(a) || stL.isMissing(b)) stD.missing
      else a - b
  }

  // (L,D) => D

  implicit val powLD = new BinOp[Power, Long, Double, Double] {
    def apply(a: Long, b: Double): Double =
      if (stL.isMissing(a) || stD.isMissing(b)) stD.missing
      else math.pow(a, b)
  }
  implicit val modLD = new BinOp[Mod, Long, Double, Double] {
    def apply(a: Long, b: Double): Double =
      if (stL.isMissing(a) || stD.isMissing(b)) stD.missing
      else a % b
  }
  implicit val addLD = new BinOp[Add, Long, Double, Double] {
    def apply(a: Long, b: Double): Double =
      if (stL.isMissing(a) || stD.isMissing(b)) stD.missing
      else a + b
  }
  implicit val mulLD = new BinOp[Multiply, Long, Double, Double] {
    def apply(a: Long, b: Double): Double =
      if (stL.isMissing(a) || stD.isMissing(b)) stD.missing
      else a * b
  }
  implicit val divLD = new BinOp[Divide, Long, Double, Double] {
    def apply(a: Long, b: Double): Double =
      if (stL.isMissing(a) || stD.isMissing(b)) stD.missing
      else a / b
  }
  implicit val subLD = new BinOp[Subtract, Long, Double, Double] {
    def apply(a: Long, b: Double): Double =
      if (stL.isMissing(a) || stD.isMissing(b)) stD.missing
      else a - b
  }

  // (I,D) => D

  implicit val powID = new BinOp[Power, Int, Double, Double] {
    def apply(a: Int, b: Double): Double =
      if (stI.isMissing(a) || stD.isMissing(b)) stD.missing
      else math.pow(a, b)
  }
  implicit val modID = new BinOp[Mod, Int, Double, Double] {
    def apply(a: Int, b: Double): Double =
      if (stI.isMissing(a) || stD.isMissing(b)) stD.missing
      else a % b
  }
  implicit val addID = new BinOp[Add, Int, Double, Double] {
    def apply(a: Int, b: Double): Double =
      if (stI.isMissing(a) || stD.isMissing(b)) stD.missing
      else a + b
  }
  implicit val mulID = new BinOp[Multiply, Int, Double, Double] {
    def apply(a: Int, b: Double): Double =
      if (stI.isMissing(a) || stD.isMissing(b)) stD.missing
      else a * b
  }
  implicit val divID = new BinOp[Divide, Int, Double, Double] {
    def apply(a: Int, b: Double): Double =
      if (stI.isMissing(a) || stD.isMissing(b)) stD.missing
      else a / b
  }
  implicit val subID = new BinOp[Subtract, Int, Double, Double] {
    def apply(a: Int, b: Double): Double =
      if (stI.isMissing(a) || stD.isMissing(b)) stD.missing
      else a - b
  }

  // (D,I) => D

  implicit val powDI = new BinOp[Power, Double, Int, Double] {
    def apply(a: Double, b: Int): Double =
      if (stD.isMissing(a) || stI.isMissing(b)) stD.missing
      else math.pow(a, b)
  }
  implicit val modDI = new BinOp[Mod, Double, Int, Double] {
    def apply(a: Double, b: Int): Double =
      if (stD.isMissing(a) || stI.isMissing(b)) stD.missing
      else a % b
  }
  implicit val addDI = new BinOp[Add, Double, Int, Double] {
    def apply(a: Double, b: Int): Double =
      if (stD.isMissing(a) || stI.isMissing(b)) stD.missing
      else a + b
  }
  implicit val mulDI = new BinOp[Multiply, Double, Int, Double] {
    def apply(a: Double, b: Int): Double =
      if (stD.isMissing(a) || stI.isMissing(b)) stD.missing
      else a * b
  }
  implicit val divDI = new BinOp[Divide, Double, Int, Double] {
    def apply(a: Double, b: Int): Double =
      if (stD.isMissing(a) || stI.isMissing(b)) stD.missing
      else a / b
  }
  implicit val subDI = new BinOp[Subtract, Double, Int, Double] {
    def apply(a: Double, b: Int): Double =
      if (stD.isMissing(a) || stI.isMissing(b)) stD.missing
      else a - b
  }

  // (L,L) => L

  implicit val powLL = new BinOp[Power, Long, Long, Long] {
    def apply(a: Long, b: Long): Long =
      if (stL.isMissing(a) || stL.isMissing(b)) stL.missing
      else math.pow(a, b).toLong
  }
  implicit val modLL = new BinOp[Mod, Long, Long, Long] {
    def apply(a: Long, b: Long): Long =
      if (stL.isMissing(a) || stL.isMissing(b)) stL.missing
      else a % b
  }
  implicit val addLL = new BinOp[Add, Long, Long, Long] {
    def apply(a: Long, b: Long): Long =
      if (stL.isMissing(a) || stL.isMissing(b)) stL.missing
      else a + b
  }
  implicit val mulLL = new BinOp[Multiply, Long, Long, Long] {
    def apply(a: Long, b: Long): Long =
      if (stL.isMissing(a) || stL.isMissing(b)) stL.missing
      else a * b
  }
  implicit val divLL = new BinOp[Divide, Long, Long, Long] {
    def apply(a: Long, b: Long): Long =
      if (stL.isMissing(a) || stL.isMissing(b)) stL.missing
      else a / b
  }
  implicit val subLL = new BinOp[Subtract, Long, Long, Long] {
    def apply(a: Long, b: Long): Long =
      if (stL.isMissing(a) || stL.isMissing(b)) stL.missing
      else a - b
  }
  implicit val andLL = new BinOp[BitAnd, Long, Long, Long] {
    def apply(a: Long, b: Long): Long =
      if (stL.isMissing(a) || stL.isMissing(b)) stL.missing
      else a & b
  }
  implicit val orLL = new BinOp[BitOr, Long, Long, Long] {
    def apply(a: Long, b: Long): Long =
      if (stL.isMissing(a) || stL.isMissing(b)) stL.missing
      else a | b
  }
  implicit val xorLL = new BinOp[BitXor, Long, Long, Long] {
    def apply(a: Long, b: Long): Long =
      if (stL.isMissing(a) || stL.isMissing(b)) stL.missing
      else a ^ b
  }

  // (I,L) => L

  implicit val powIL = new BinOp[Power, Int, Long, Long] {
    def apply(a: Int, b: Long): Long =
      if (stI.isMissing(a) || stL.isMissing(b)) stL.missing
      else math.pow(a, b).toLong
  }
  implicit val modIL = new BinOp[Mod, Int, Long, Long] {
    def apply(a: Int, b: Long): Long =
      if (stI.isMissing(a) || stL.isMissing(b)) stL.missing
      else a % b
  }
  implicit val addIL = new BinOp[Add, Int, Long, Long] {
    def apply(a: Int, b: Long): Long =
      if (stI.isMissing(a) || stL.isMissing(b)) stL.missing
      else a + b
  }
  implicit val mulIL = new BinOp[Multiply, Int, Long, Long] {
    def apply(a: Int, b: Long): Long =
      if (stI.isMissing(a) || stL.isMissing(b)) stL.missing
      else a * b
  }
  implicit val divIL = new BinOp[Divide, Int, Long, Long] {
    def apply(a: Int, b: Long): Long =
      if (stI.isMissing(a) || stL.isMissing(b)) stL.missing
      else a / b
  }
  implicit val subIL = new BinOp[Subtract, Int, Long, Long] {
    def apply(a: Int, b: Long): Long =
      if (stI.isMissing(a) || stL.isMissing(b)) stL.missing
      else a - b
  }
  implicit val andIL = new BinOp[BitAnd, Int, Long, Long] {
    def apply(a: Int, b: Long): Long =
      if (stI.isMissing(a) || stL.isMissing(b)) stL.missing
      else a & b
  }
  implicit val orIL = new BinOp[BitOr, Int, Long, Long] {
    def apply(a: Int, b: Long): Long =
      if (stI.isMissing(a) || stL.isMissing(b)) stL.missing
      else a | b
  }
  implicit val xorIL = new BinOp[BitXor, Int, Long, Long] {
    def apply(a: Int, b: Long): Long =
      if (stI.isMissing(a) || stL.isMissing(b)) stL.missing
      else a ^ b
  }

  // (L,I) => L

  implicit val powLI = new BinOp[Power, Long, Int, Long] {
    def apply(a: Long, b: Int): Long =
      if (stL.isMissing(a) || stI.isMissing(b)) stL.missing
      else math.pow(a, b).toLong
  }
  implicit val modLI = new BinOp[Mod, Long, Int, Long] {
    def apply(a: Long, b: Int): Long =
      if (stL.isMissing(a) || stI.isMissing(b)) stL.missing
      else a % b
  }
  implicit val addLI = new BinOp[Add, Long, Int, Long] {
    def apply(a: Long, b: Int): Long =
      if (stL.isMissing(a) || stI.isMissing(b)) stL.missing
      else a + b
  }
  implicit val mulLI = new BinOp[Multiply, Long, Int, Long] {
    def apply(a: Long, b: Int): Long =
      if (stL.isMissing(a) || stI.isMissing(b)) stL.missing
      else a * b
  }
  implicit val divLI = new BinOp[Divide, Long, Int, Long] {
    def apply(a: Long, b: Int): Long =
      if (stL.isMissing(a) || stI.isMissing(b)) stL.missing
      else a / b
  }
  implicit val subLI = new BinOp[Subtract, Long, Int, Long] {
    def apply(a: Long, b: Int): Long =
      if (stL.isMissing(a) || stI.isMissing(b)) stL.missing
      else a - b
  }
  implicit val andLI = new BinOp[BitAnd, Long, Int, Long] {
    def apply(a: Long, b: Int): Long =
      if (stL.isMissing(a) || stI.isMissing(b)) stL.missing
      else a & b
  }
  implicit val orLI = new BinOp[BitOr, Long, Int, Long] {
    def apply(a: Long, b: Int): Long =
      if (stL.isMissing(a) || stI.isMissing(b)) stL.missing
      else a | b
  }
  implicit val xorLI = new BinOp[BitXor, Long, Int, Long] {
    def apply(a: Long, b: Int): Long =
      if (stL.isMissing(a) || stI.isMissing(b)) stL.missing
      else a ^ b
  }

  // (I,I) => I

  implicit val powII = new BinOp[Power, Int, Int, Int] {
    def apply(a: Int, b: Int): Int =
      if (stI.isMissing(a) || stI.isMissing(b)) stI.missing
      else math.pow(a, b).toInt
  }
  implicit val modII = new BinOp[Mod, Int, Int, Int] {
    def apply(a: Int, b: Int): Int =
      if (stI.isMissing(a) || stI.isMissing(b)) stI.missing
      else a % b
  }
  implicit val addII = new BinOp[Add, Int, Int, Int] {
    def apply(a: Int, b: Int): Int =
      if (stI.isMissing(a) || stI.isMissing(b)) stI.missing
      else a + b
  }
  implicit val mulII = new BinOp[Multiply, Int, Int, Int] {
    def apply(a: Int, b: Int): Int =
      if (stI.isMissing(a) || stI.isMissing(b)) stI.missing
      else a * b
  }
  implicit val divII = new BinOp[Divide, Int, Int, Int] {
    def apply(a: Int, b: Int): Int =
      if (stI.isMissing(a) || stI.isMissing(b)) stI.missing
      else a / b
  }
  implicit val subII = new BinOp[Subtract, Int, Int, Int] {
    def apply(a: Int, b: Int): Int =
      if (stI.isMissing(a) || stI.isMissing(b)) stI.missing
      else a - b
  }
  implicit val andII = new BinOp[BitAnd, Int, Int, Int] {
    def apply(a: Int, b: Int): Int =
      if (stI.isMissing(a) || stI.isMissing(b)) stI.missing
      else a & b
  }
  implicit val orII = new BinOp[BitOr, Int, Int, Int] {
    def apply(a: Int, b: Int): Int =
      if (stI.isMissing(a) || stI.isMissing(b)) stI.missing
      else a | b
  }
  implicit val xorII = new BinOp[BitXor, Int, Int, Int] {
    def apply(a: Int, b: Int): Int =
      if (stI.isMissing(a) || stI.isMissing(b)) stI.missing
      else a ^ b
  }
  implicit val shlII = new BinOp[BitShl, Int, Int, Int] {
    def apply(a: Int, b: Int): Int =
      if (stI.isMissing(a) || stI.isMissing(b)) stI.missing
      else a << b
  }
  implicit val shrII = new BinOp[BitShr, Int, Int, Int] {
    def apply(a: Int, b: Int): Int =
      if (stI.isMissing(a) || stI.isMissing(b)) stI.missing
      else a >> b
  }
  implicit val ushrII = new BinOp[BitUShr, Int, Int, Int] {
    def apply(a: Int, b: Int): Int =
      if (stI.isMissing(a) || stI.isMissing(b)) stI.missing
      else a >>> b
  }

  // (Bool, Bool) => Bool ops

  implicit val andBB = new BinOp[AndOp, Boolean, Boolean, Boolean] {
    def apply(a: Boolean, b: Boolean) = a && b
  }
  implicit val orBB = new BinOp[OrOp, Boolean, Boolean, Boolean] {
    def apply(a: Boolean, b: Boolean) = a || b
  }
  implicit val xorBB = new BinOp[XorOp, Boolean, Boolean, Boolean] {
    def apply(a: Boolean, b: Boolean) = a && b || !a && !b
  }

  /* comparisons ops */

  // >
  implicit val gtDD =
    new BinOp[GtOp, Double, Double, Boolean] {
      def apply(a: Double, b: Double) =
        if (stD.isMissing(a) || stD.isMissing(b)) stB.missing
        else (a > b)
    }
  implicit val gtDL =
    new BinOp[GtOp, Double, Long, Boolean] {
      def apply(a: Double, b: Long) =
        if (stD.isMissing(a) || stL.isMissing(b)) stB.missing
        else (a > b)
    }
  implicit val gtDI =
    new BinOp[GtOp, Double, Int, Boolean] {
      def apply(a: Double, b: Int) =
        if (stD.isMissing(a) || stI.isMissing(b)) stB.missing
        else (a > b)
    }
  implicit val gtLD =
    new BinOp[GtOp, Long, Double, Boolean] {
      def apply(a: Long, b: Double) =
        if (stL.isMissing(a) || stD.isMissing(b)) stB.missing
        else (a > b)
    }
  implicit val gtLL =
    new BinOp[GtOp, Long, Long, Boolean] {
      def apply(a: Long, b: Long) =
        if (stL.isMissing(a) || stL.isMissing(b)) stB.missing
        else (a > b)
    }
  implicit val gtLI =
    new BinOp[GtOp, Long, Int, Boolean] {
      def apply(a: Long, b: Int) =
        if (stL.isMissing(a) || stI.isMissing(b)) stB.missing
        else (a > b)
    }
  implicit val gtID =
    new BinOp[GtOp, Int, Double, Boolean] {
      def apply(a: Int, b: Double) =
        if (stI.isMissing(a) || stD.isMissing(b)) stB.missing
        else (a > b)
    }
  implicit val gtII =
    new BinOp[GtOp, Int, Int, Boolean] {
      def apply(a: Int, b: Int) =
        if (stI.isMissing(a) || stI.isMissing(b)) stB.missing
        else (a > b)
    }
  implicit val gtBB =
    new BinOp[GtOp, Boolean, Boolean, Boolean] {
      def apply(a: Boolean, b: Boolean) =
        if (stB.isMissing(a) || stB.isMissing(b)) stB.missing
        else (a > b)
    }

  // <

  implicit val ltDD =
    new BinOp[LtOp, Double, Double, Boolean] {
      def apply(a: Double, b: Double) =
        if (stD.isMissing(a) || stD.isMissing(b)) stB.missing
        else (a < b)
    }
  implicit val ltDL =
    new BinOp[LtOp, Double, Long, Boolean] {
      def apply(a: Double, b: Long) =
        if (stD.isMissing(a) || stL.isMissing(b)) stB.missing
        else (a < b)
    }
  implicit val ltDI =
    new BinOp[LtOp, Double, Int, Boolean] {
      def apply(a: Double, b: Int) =
        if (stD.isMissing(a) || stI.isMissing(b)) stB.missing
        else (a < b)
    }
  implicit val ltLD =
    new BinOp[LtOp, Long, Double, Boolean] {
      def apply(a: Long, b: Double) =
        if (stL.isMissing(a) || stD.isMissing(b)) stB.missing
        else (a < b)
    }
  implicit val ltLL =
    new BinOp[LtOp, Long, Long, Boolean] {
      def apply(a: Long, b: Long) =
        if (stL.isMissing(a) || stL.isMissing(b)) stB.missing
        else (a < b)
    }
  implicit val ltLI =
    new BinOp[LtOp, Long, Int, Boolean] {
      def apply(a: Long, b: Int) =
        if (stL.isMissing(a) || stI.isMissing(b)) stB.missing
        else (a < b)
    }
  implicit val ltID =
    new BinOp[LtOp, Int, Double, Boolean] {
      def apply(a: Int, b: Double) =
        if (stI.isMissing(a) || stD.isMissing(b)) stB.missing
        else (a < b)
    }
  implicit val ltII =
    new BinOp[LtOp, Int, Int, Boolean] {
      def apply(a: Int, b: Int) =
        if (stI.isMissing(a) || stI.isMissing(b)) stB.missing
        else (a < b)
    }
  implicit val ltBB =
    new BinOp[LtOp, Boolean, Boolean, Boolean] {
      def apply(a: Boolean, b: Boolean) =
        if (stB.isMissing(a) || stB.isMissing(b)) stB.missing
        else (a < b)
    }

  // ==

  implicit val eqDD =
    new BinOp[EqOp, Double, Double, Boolean] {
      def apply(a: Double, b: Double) =
        if (stD.isMissing(a) || stD.isMissing(b)) stB.missing
        else (a == b)
    }
  implicit val eqDL =
    new BinOp[EqOp, Double, Long, Boolean] {
      def apply(a: Double, b: Long) =
        if (stD.isMissing(a) || stL.isMissing(b)) stB.missing
        else (a == b)
    }
  implicit val eqDI =
    new BinOp[EqOp, Double, Int, Boolean] {
      def apply(a: Double, b: Int) =
        if (stD.isMissing(a) || stI.isMissing(b)) stB.missing
        else (a == b)
    }
  implicit val eqLD =
    new BinOp[EqOp, Long, Double, Boolean] {
      def apply(a: Long, b: Double) =
        if (stL.isMissing(a) || stD.isMissing(b)) stB.missing
        else (a == b)
    }
  implicit val eqLL =
    new BinOp[EqOp, Long, Long, Boolean] {
      def apply(a: Long, b: Long) =
        if (stL.isMissing(a) || stL.isMissing(b)) stB.missing
        else (a == b)
    }
  implicit val eqLI =
    new BinOp[EqOp, Long, Int, Boolean] {
      def apply(a: Long, b: Int) =
        if (stL.isMissing(a) || stI.isMissing(b)) stB.missing
        else (a == b)
    }
  implicit val eqID =
    new BinOp[EqOp, Int, Double, Boolean] {
      def apply(a: Int, b: Double) =
        if (stI.isMissing(a) || stD.isMissing(b)) stB.missing
        else (a == b)
    }
  implicit val eqII =
    new BinOp[EqOp, Int, Int, Boolean] {
      def apply(a: Int, b: Int) =
        if (stI.isMissing(a) || stI.isMissing(b)) stB.missing
        else (a == b)
    }
  implicit val eqBB =
    new BinOp[EqOp, Boolean, Boolean, Boolean] {
      def apply(a: Boolean, b: Boolean) =
        if (stB.isMissing(a) || stB.isMissing(b)) stB.missing
        else (a == b)
    }

  // !=

  implicit val neqDD =
    new BinOp[NeqOp, Double, Double, Boolean] {
      def apply(a: Double, b: Double) =
        if (stD.isMissing(a) || stD.isMissing(b)) stB.missing
        else (a != b)
    }
  implicit val neqDL =
    new BinOp[NeqOp, Double, Long, Boolean] {
      def apply(a: Double, b: Long) =
        if (stD.isMissing(a) || stL.isMissing(b)) stB.missing
        else (a != b)
    }
  implicit val neqDI =
    new BinOp[NeqOp, Double, Int, Boolean] {
      def apply(a: Double, b: Int) =
        if (stD.isMissing(a) || stI.isMissing(b)) stB.missing
        else (a != b)
    }
  implicit val neqLD =
    new BinOp[NeqOp, Long, Double, Boolean] {
      def apply(a: Long, b: Double) =
        if (stL.isMissing(a) || stD.isMissing(b)) stB.missing
        else (a != b)
    }
  implicit val neqLL =
    new BinOp[NeqOp, Long, Long, Boolean] {
      def apply(a: Long, b: Long) =
        if (stL.isMissing(a) || stL.isMissing(b)) stB.missing
        else (a != b)
    }
  implicit val neqLI =
    new BinOp[NeqOp, Long, Int, Boolean] {
      def apply(a: Long, b: Int) =
        if (stL.isMissing(a) || stI.isMissing(b)) stB.missing
        else (a != b)
    }
  implicit val neqID =
    new BinOp[NeqOp, Int, Double, Boolean] {
      def apply(a: Int, b: Double) =
        if (stI.isMissing(a) || stD.isMissing(b)) stB.missing
        else (a != b)
    }
  implicit val neqII =
    new BinOp[NeqOp, Int, Int, Boolean] {
      def apply(a: Int, b: Int) =
        if (stI.isMissing(a) || stI.isMissing(b)) stB.missing
        else (a != b)
    }
  implicit val neqBB =
    new BinOp[NeqOp, Boolean, Boolean, Boolean] {
      def apply(a: Boolean, b: Boolean) =
        if (stB.isMissing(a) || stB.isMissing(b)) stB.missing
        else (a != b)
    }

  // >=

  implicit val gteDD =
    new BinOp[GteOp, Double, Double, Boolean] {
      def apply(a: Double, b: Double) =
        if (stD.isMissing(a) || stD.isMissing(b)) stB.missing
        else (a >= b)
    }
  implicit val gteDL =
    new BinOp[GteOp, Double, Long, Boolean] {
      def apply(a: Double, b: Long) =
        if (stD.isMissing(a) || stL.isMissing(b)) stB.missing
        else (a >= b)
    }
  implicit val gteDI =
    new BinOp[GteOp, Double, Int, Boolean] {
      def apply(a: Double, b: Int) =
        if (stD.isMissing(a) || stI.isMissing(b)) stB.missing
        else (a >= b)
    }
  implicit val gteLD =
    new BinOp[GteOp, Long, Double, Boolean] {
      def apply(a: Long, b: Double) =
        if (stL.isMissing(a) || stD.isMissing(b)) stB.missing
        else (a >= b)
    }
  implicit val gteLL =
    new BinOp[GteOp, Long, Long, Boolean] {
      def apply(a: Long, b: Long) =
        if (stL.isMissing(a) || stL.isMissing(b)) stB.missing
        else (a >= b)
    }
  implicit val gteLI =
    new BinOp[GteOp, Long, Int, Boolean] {
      def apply(a: Long, b: Int) =
        if (stL.isMissing(a) || stI.isMissing(b)) stB.missing
        else (a >= b)
    }
  implicit val gteID =
    new BinOp[GteOp, Int, Double, Boolean] {
      def apply(a: Int, b: Double) =
        if (stI.isMissing(a) || stD.isMissing(b)) stB.missing
        else (a >= b)
    }
  implicit val gteII =
    new BinOp[GteOp, Int, Int, Boolean] {
      def apply(a: Int, b: Int) =
        if (stI.isMissing(a) || stI.isMissing(b)) stB.missing
        else (a >= b)
    }
  implicit val gteBB =
    new BinOp[GteOp, Boolean, Boolean, Boolean] {
      def apply(a: Boolean, b: Boolean) =
        if (stB.isMissing(a) || stB.isMissing(b)) stB.missing
        else (a >= b)
    }

  // <=

  implicit val lteDD =
    new BinOp[LteOp, Double, Double, Boolean] {
      def apply(a: Double, b: Double) =
        if (stD.isMissing(a) || stD.isMissing(b)) stB.missing
        else (a <= b)
    }
  implicit val lteDL =
    new BinOp[LteOp, Double, Long, Boolean] {
      def apply(a: Double, b: Long) =
        if (stD.isMissing(a) || stL.isMissing(b)) stB.missing
        else (a <= b)
    }
  implicit val lteDI =
    new BinOp[LteOp, Double, Int, Boolean] {
      def apply(a: Double, b: Int) =
        if (stD.isMissing(a) || stI.isMissing(b)) stB.missing
        else (a <= b)
    }
  implicit val lteLD =
    new BinOp[LteOp, Long, Double, Boolean] {
      def apply(a: Long, b: Double) =
        if (stL.isMissing(a) || stD.isMissing(b)) stB.missing
        else (a <= b)
    }
  implicit val lteLL =
    new BinOp[LteOp, Long, Long, Boolean] {
      def apply(a: Long, b: Long) =
        if (stL.isMissing(a) || stL.isMissing(b)) stB.missing
        else (a <= b)
    }
  implicit val lteLI =
    new BinOp[LteOp, Long, Int, Boolean] {
      def apply(a: Long, b: Int) =
        if (stL.isMissing(a) || stI.isMissing(b)) stB.missing
        else (a <= b)
    }
  implicit val lteID =
    new BinOp[LteOp, Int, Double, Boolean] {
      def apply(a: Int, b: Double) =
        if (stI.isMissing(a) || stD.isMissing(b)) stB.missing
        else (a <= b)
    }
  implicit val lteII =
    new BinOp[LteOp, Int, Int, Boolean] {
      def apply(a: Int, b: Int) =
        if (stI.isMissing(a) || stI.isMissing(b)) stB.missing
        else (a <= b)
    }
  implicit val lteBB =
    new BinOp[LteOp, Boolean, Boolean, Boolean] {
      def apply(a: Boolean, b: Boolean) =
        if (stB.isMissing(a) || stB.isMissing(b)) stB.missing
        else (a <= b)
    }
}
