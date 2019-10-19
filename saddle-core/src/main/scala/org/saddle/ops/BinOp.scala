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
import org.saddle.ST

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

/**
  * Contains implementations of primitive binary ops that are NA-aware
  *
  * Double primitive has NA bit pattern baked into its representation, but
  * for others we must check for the appropriate sentinel value.
  *
  * Note scala.Function2 is not specialized on Boolean inputs, only output
  */
object BinOp {
  private final class BinOpImpl[
      O <: OpType,
      @spec(Int, Long, Double) Q: ST,
      @spec(Int, Long, Double) R: ST,
      @spec(Boolean, Int, Long, Double) S: ST
  ](f: (Q, R) => S)
      extends BinOp[O, Q, R, S] {
    val sq = implicitly[ST[Q]]
    val sr = implicitly[ST[R]]
    val ss = implicitly[ST[S]]
    def apply(a: Q, b: R) =
      if (sq.isMissing(a) || sr.isMissing(b)) ss.missing else f(a, b)
  }

  private final class BinOpImplDL[O <: OpType, @spec(Int, Long) R: ST](
      f: (Double, R) => Double
  ) extends BinOp[O, Double, R, Double] {
    val sc = implicitly[ST[R]]
    def apply(a: Double, b: R) = if (sc.isMissing(b)) Double.NaN else f(a, b)
  }

  private final class BinOpImplLD[O <: OpType, @spec(Int, Long) Q: ST](
      f: (Q, Double) => Double
  ) extends BinOp[O, Q, Double, Double] {
    val sc = implicitly[ST[Q]]
    def apply(a: Q, b: Double) = if (sc.isMissing(a)) Double.NaN else f(a, b)
  }

  private final class BinOpImplDD[O <: OpType](f: (Double, Double) => Double)
      extends BinOp[O, Double, Double, Double] {
    def apply(a: Double, b: Double) = f(a, b)
  }

  // ********************************************************
  // ** Concrete implementations necessary for specialization
  // ********************************************************

  // (x, y) => Double ops

  type BDDD[T <: OpType] = BinOp[T, Double, Double, Double]
  implicit val powDD: BDDD[Power] =
    new BinOpImplDD[Power]((x, y) => math.pow(x, y))
  implicit val modDD: BDDD[Mod] = new BinOpImplDD[Mod](_ % _)
  implicit val addDD: BDDD[Add] = new BinOpImplDD[Add](_ + _)
  implicit val mulDD: BDDD[Multiply] = new BinOpImplDD[Multiply](_ * _)
  implicit val divDD: BDDD[Divide] = new BinOpImplDD[Divide](_ / _)
  implicit val subDD: BDDD[Subtract] = new BinOpImplDD[Subtract](_ - _)

  type BDLD[T <: OpType] = BinOp[T, Double, Long, Double]
  implicit val powDL: BDLD[Power] =
    new BinOpImplDL[Power, Long]((x, y) => math.pow(x, y))
  implicit val modDL: BDLD[Mod] = new BinOpImplDL[Mod, Long](_ % _)
  implicit val addDL: BDLD[Add] = new BinOpImplDL[Add, Long](_ + _)
  implicit val mulDL: BDLD[Multiply] = new BinOpImplDL[Multiply, Long](_ * _)
  implicit val divDL: BDLD[Divide] = new BinOpImplDL[Divide, Long](_ / _)
  implicit val subDL: BDLD[Subtract] = new BinOpImplDL[Subtract, Long](_ - _)

  type BLDD[T <: OpType] = BinOp[T, Long, Double, Double]
  implicit val powLD: BLDD[Power] =
    new BinOpImplLD[Power, Long]((x, y) => math.pow(x, y))
  implicit val modLD: BLDD[Mod] = new BinOpImplLD[Mod, Long](_ % _)
  implicit val addLD: BLDD[Add] = new BinOpImplLD[Add, Long](_ + _)
  implicit val mulLD: BLDD[Multiply] = new BinOpImplLD[Multiply, Long](_ * _)
  implicit val divLD: BLDD[Divide] = new BinOpImplLD[Divide, Long](_ / _)
  implicit val subLD: BLDD[Subtract] = new BinOpImplLD[Subtract, Long](_ - _)

  type BIDD[T <: OpType] = BinOp[T, Int, Double, Double]
  implicit val powDI: BIDD[Power] =
    new BinOpImplLD[Power, Int]((x, y) => math.pow(x, y))
  implicit val modDI: BIDD[Mod] = new BinOpImplLD[Mod, Int](_ % _)
  implicit val addDI: BIDD[Add] = new BinOpImplLD[Add, Int](_ + _)
  implicit val mulDI: BIDD[Multiply] = new BinOpImplLD[Multiply, Int](_ * _)
  implicit val divDI: BIDD[Divide] = new BinOpImplLD[Divide, Int](_ / _)
  implicit val subDI: BIDD[Subtract] = new BinOpImplLD[Subtract, Int](_ - _)

  type BDID[T <: OpType] = BinOp[T, Double, Int, Double]
  implicit val powID: BDID[Power] =
    new BinOpImplDL[Power, Int]((x, y) => math.pow(x, y))
  implicit val modID: BDID[Mod] = new BinOpImplDL[Mod, Int](_ % _)
  implicit val addID: BDID[Add] = new BinOpImplDL[Add, Int](_ + _)
  implicit val mulID: BDID[Multiply] = new BinOpImplDL[Multiply, Int](_ * _)
  implicit val divID: BDID[Divide] = new BinOpImplDL[Divide, Int](_ / _)
  implicit val subID: BDID[Subtract] = new BinOpImplDL[Subtract, Int](_ - _)

  // (x, y) => Long ops

  type BLLL[T <: OpType] = BinOp[T, Long, Long, Long]
  implicit val powLL: BLLL[Power] =
    new BinOpImpl[Power, Long, Long, Long]((x, y) => math.pow(x, y).toLong)
  implicit val modLL: BLLL[Mod] = new BinOpImpl[Mod, Long, Long, Long](_ % _)
  implicit val addLL: BLLL[Add] = new BinOpImpl[Add, Long, Long, Long](_ + _)
  implicit val mulLL: BLLL[Multiply] =
    new BinOpImpl[Multiply, Long, Long, Long](_ * _)
  implicit val divLL: BLLL[Divide] =
    new BinOpImpl[Divide, Long, Long, Long](_ / _)
  implicit val subLL: BLLL[Subtract] =
    new BinOpImpl[Subtract, Long, Long, Long](_ - _)
  implicit val andLL: BLLL[BitAnd] =
    new BinOpImpl[BitAnd, Long, Long, Long](_ & _)
  implicit val orLL: BLLL[BitOr] = new BinOpImpl[BitOr, Long, Long, Long](_ | _)
  implicit val xorLL: BLLL[BitXor] =
    new BinOpImpl[BitXor, Long, Long, Long](_ ^ _)

  type BILL[T <: OpType] = BinOp[T, Int, Long, Long]
  implicit val powLI: BLIL[Power] =
    new BinOpImpl[Power, Long, Int, Long]((x, y) => math.pow(x, y).toLong)
  implicit val modLI: BLIL[Mod] = new BinOpImpl[Mod, Long, Int, Long](_ % _)
  implicit val addLI: BLIL[Add] = new BinOpImpl[Add, Long, Int, Long](_ + _)
  implicit val mulLI: BLIL[Multiply] =
    new BinOpImpl[Multiply, Long, Int, Long](_ * _)
  implicit val divLI: BLIL[Divide] =
    new BinOpImpl[Divide, Long, Int, Long](_ / _)
  implicit val subLI: BLIL[Subtract] =
    new BinOpImpl[Subtract, Long, Int, Long](_ - _)
  implicit val andLI: BLIL[BitAnd] =
    new BinOpImpl[BitAnd, Long, Int, Long](_ & _)
  implicit val orLI: BLIL[BitOr] = new BinOpImpl[BitOr, Long, Int, Long](_ | _)
  implicit val xorLI: BLIL[BitXor] =
    new BinOpImpl[BitXor, Long, Int, Long](_ ^ _)
  implicit val shlLI: BLIL[BitShl] =
    new BinOpImpl[BitShl, Long, Int, Long](_ << _)
  implicit val shrLI: BLIL[BitShr] =
    new BinOpImpl[BitShr, Long, Int, Long](_ >> _)
  implicit val ushLI: BLIL[BitUShr] =
    new BinOpImpl[BitUShr, Long, Int, Long](_ >>> _)

  type BLIL[T <: OpType] = BinOp[T, Long, Int, Long]
  implicit val powIL: BILL[Power] =
    new BinOpImpl[Power, Int, Long, Long]((x, y) => math.pow(x, y).toLong)
  implicit val modIL: BILL[Mod] = new BinOpImpl[Mod, Int, Long, Long](_ % _)
  implicit val addIL: BILL[Add] = new BinOpImpl[Add, Int, Long, Long](_ + _)
  implicit val mulIL: BILL[Multiply] =
    new BinOpImpl[Multiply, Int, Long, Long](_ * _)
  implicit val divIL: BILL[Divide] =
    new BinOpImpl[Divide, Int, Long, Long](_ / _)
  implicit val subIL: BILL[Subtract] =
    new BinOpImpl[Subtract, Int, Long, Long](_ - _)
  implicit val andIL: BILL[BitAnd] =
    new BinOpImpl[BitAnd, Int, Long, Long](_ & _)
  implicit val orIL: BILL[BitOr] = new BinOpImpl[BitOr, Int, Long, Long](_ | _)
  implicit val xorIL: BILL[BitXor] =
    new BinOpImpl[BitXor, Int, Long, Long](_ ^ _)

  // (x, y) => Int ops

  type BIII[T <: OpType] = BinOp[T, Int, Int, Int]
  implicit val powII: BIII[Power] =
    new BinOpImpl[Power, Int, Int, Int]((x, y) => math.pow(x, y).toInt)
  implicit val modII: BIII[Mod] = new BinOpImpl[Mod, Int, Int, Int](_ % _)
  implicit val addII: BIII[Add] = new BinOpImpl[Add, Int, Int, Int](_ + _)
  implicit val mulII: BIII[Multiply] =
    new BinOpImpl[Multiply, Int, Int, Int](_ * _)
  implicit val divII: BIII[Divide] = new BinOpImpl[Divide, Int, Int, Int](_ / _)
  implicit val subII: BIII[Subtract] =
    new BinOpImpl[Subtract, Int, Int, Int](_ - _)
  implicit val andII: BIII[BitAnd] = new BinOpImpl[BitAnd, Int, Int, Int](_ & _)
  implicit val orII: BIII[BitOr] = new BinOpImpl[BitOr, Int, Int, Int](_ | _)
  implicit val xorII: BIII[BitXor] = new BinOpImpl[BitXor, Int, Int, Int](_ ^ _)
  implicit val shlII: BIII[BitShl] =
    new BinOpImpl[BitShl, Int, Int, Int](_ << _)
  implicit val shrII: BIII[BitShr] =
    new BinOpImpl[BitShr, Int, Int, Int](_ >> _)
  implicit val ushII: BIII[BitUShr] =
    new BinOpImpl[BitUShr, Int, Int, Int](_ >>> _)

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

  implicit val gtDD: BinOp[GtOp, Double, Double, Boolean] =
    new BinOpImpl[GtOp, Double, Double, Boolean](_ > _)
  implicit val gtDL: BinOp[GtOp, Double, Long, Boolean] =
    new BinOpImpl[GtOp, Double, Long, Boolean](_ > _)
  implicit val gtDI: BinOp[GtOp, Double, Int, Boolean] =
    new BinOpImpl[GtOp, Double, Int, Boolean](_ > _)
  implicit val gtLD: BinOp[GtOp, Long, Double, Boolean] =
    new BinOpImpl[GtOp, Long, Double, Boolean](_ > _)
  implicit val gtLL: BinOp[GtOp, Long, Long, Boolean] =
    new BinOpImpl[GtOp, Long, Long, Boolean](_ > _)
  implicit val gtLI: BinOp[GtOp, Long, Int, Boolean] =
    new BinOpImpl[GtOp, Long, Int, Boolean](_ > _)
  implicit val gtID: BinOp[GtOp, Int, Double, Boolean] =
    new BinOpImpl[GtOp, Int, Double, Boolean](_ > _)
  implicit val gtIL: BinOp[GtOp, Int, Long, Boolean] =
    new BinOpImpl[GtOp, Int, Long, Boolean](_ > _)
  implicit val gtII: BinOp[GtOp, Int, Int, Boolean] =
    new BinOpImpl[GtOp, Int, Int, Boolean](_ > _)
  implicit val gtBB: BinOp[GtOp, Boolean, Boolean, Boolean] =
    new BinOpImpl[GtOp, Boolean, Boolean, Boolean](_ > _)

  implicit val ltDD: BinOp[LtOp, Double, Double, Boolean] =
    new BinOpImpl[LtOp, Double, Double, Boolean](_ < _)
  implicit val ltDL: BinOp[LtOp, Double, Long, Boolean] =
    new BinOpImpl[LtOp, Double, Long, Boolean](_ < _)
  implicit val ltDI: BinOp[LtOp, Double, Int, Boolean] =
    new BinOpImpl[LtOp, Double, Int, Boolean](_ < _)
  implicit val ltLD: BinOp[LtOp, Long, Double, Boolean] =
    new BinOpImpl[LtOp, Long, Double, Boolean](_ < _)
  implicit val ltLL: BinOp[LtOp, Long, Long, Boolean] =
    new BinOpImpl[LtOp, Long, Long, Boolean](_ < _)
  implicit val ltLI: BinOp[LtOp, Long, Int, Boolean] =
    new BinOpImpl[LtOp, Long, Int, Boolean](_ < _)
  implicit val ltID: BinOp[LtOp, Int, Double, Boolean] =
    new BinOpImpl[LtOp, Int, Double, Boolean](_ < _)
  implicit val ltIL: BinOp[LtOp, Int, Long, Boolean] =
    new BinOpImpl[LtOp, Int, Long, Boolean](_ < _)
  implicit val ltII: BinOp[LtOp, Int, Int, Boolean] =
    new BinOpImpl[LtOp, Int, Int, Boolean](_ < _)
  implicit val ltBB: BinOp[LtOp, Boolean, Boolean, Boolean] =
    new BinOpImpl[LtOp, Boolean, Boolean, Boolean](_ < _)

  implicit val eqDD: BinOp[EqOp, Double, Double, Boolean] =
    new BinOpImpl[EqOp, Double, Double, Boolean](_ == _)
  implicit val eqDL: BinOp[EqOp, Double, Long, Boolean] =
    new BinOpImpl[EqOp, Double, Long, Boolean](_ == _)
  implicit val eqDI: BinOp[EqOp, Double, Int, Boolean] =
    new BinOpImpl[EqOp, Double, Int, Boolean](_ == _)
  implicit val eqLD: BinOp[EqOp, Long, Double, Boolean] =
    new BinOpImpl[EqOp, Long, Double, Boolean](_ == _)
  implicit val eqLL: BinOp[EqOp, Long, Long, Boolean] =
    new BinOpImpl[EqOp, Long, Long, Boolean](_ == _)
  implicit val eqLI: BinOp[EqOp, Long, Int, Boolean] =
    new BinOpImpl[EqOp, Long, Int, Boolean](_ == _)
  implicit val eqID: BinOp[EqOp, Int, Double, Boolean] =
    new BinOpImpl[EqOp, Int, Double, Boolean](_ == _)
  implicit val eqIL: BinOp[EqOp, Int, Long, Boolean] =
    new BinOpImpl[EqOp, Int, Long, Boolean](_ == _)
  implicit val eqII: BinOp[EqOp, Int, Int, Boolean] =
    new BinOpImpl[EqOp, Int, Int, Boolean](_ == _)
  implicit val eqBB: BinOp[EqOp, Boolean, Boolean, Boolean] =
    new BinOpImpl[EqOp, Boolean, Boolean, Boolean](_ == _)

  implicit val neqDD: BinOp[NeqOp, Double, Double, Boolean] =
    new BinOpImpl[NeqOp, Double, Double, Boolean](_ != _)
  implicit val neqDL: BinOp[NeqOp, Double, Long, Boolean] =
    new BinOpImpl[NeqOp, Double, Long, Boolean](_ != _)
  implicit val neqDI: BinOp[NeqOp, Double, Int, Boolean] =
    new BinOpImpl[NeqOp, Double, Int, Boolean](_ != _)
  implicit val neqLD: BinOp[NeqOp, Long, Double, Boolean] =
    new BinOpImpl[NeqOp, Long, Double, Boolean](_ != _)
  implicit val neqLL: BinOp[NeqOp, Long, Long, Boolean] =
    new BinOpImpl[NeqOp, Long, Long, Boolean](_ != _)
  implicit val neqLI: BinOp[NeqOp, Long, Int, Boolean] =
    new BinOpImpl[NeqOp, Long, Int, Boolean](_ != _)
  implicit val neqID: BinOp[NeqOp, Int, Double, Boolean] =
    new BinOpImpl[NeqOp, Int, Double, Boolean](_ != _)
  implicit val neqIL: BinOp[NeqOp, Int, Long, Boolean] =
    new BinOpImpl[NeqOp, Int, Long, Boolean](_ != _)
  implicit val neqII: BinOp[NeqOp, Int, Int, Boolean] =
    new BinOpImpl[NeqOp, Int, Int, Boolean](_ != _)
  implicit val neqBB: BinOp[NeqOp, Boolean, Boolean, Boolean] =
    new BinOpImpl[NeqOp, Boolean, Boolean, Boolean](_ != _)

  implicit val gteDD: BinOp[GteOp, Double, Double, Boolean] =
    new BinOpImpl[GteOp, Double, Double, Boolean](_ >= _)
  implicit val gteDL: BinOp[GteOp, Double, Long, Boolean] =
    new BinOpImpl[GteOp, Double, Long, Boolean](_ >= _)
  implicit val gteDI: BinOp[GteOp, Double, Int, Boolean] =
    new BinOpImpl[GteOp, Double, Int, Boolean](_ >= _)
  implicit val gteLD: BinOp[GteOp, Long, Double, Boolean] =
    new BinOpImpl[GteOp, Long, Double, Boolean](_ >= _)
  implicit val gteLL: BinOp[GteOp, Long, Long, Boolean] =
    new BinOpImpl[GteOp, Long, Long, Boolean](_ >= _)
  implicit val gteLI: BinOp[GteOp, Long, Int, Boolean] =
    new BinOpImpl[GteOp, Long, Int, Boolean](_ >= _)
  implicit val gteID: BinOp[GteOp, Int, Double, Boolean] =
    new BinOpImpl[GteOp, Int, Double, Boolean](_ >= _)
  implicit val gteIL: BinOp[GteOp, Int, Long, Boolean] =
    new BinOpImpl[GteOp, Int, Long, Boolean](_ >= _)
  implicit val gteII: BinOp[GteOp, Int, Int, Boolean] =
    new BinOpImpl[GteOp, Int, Int, Boolean](_ >= _)
  implicit val gteBB: BinOp[GteOp, Boolean, Boolean, Boolean] =
    new BinOpImpl[GteOp, Boolean, Boolean, Boolean](_ >= _)

  implicit val lteDD: BinOp[LteOp, Double, Double, Boolean] =
    new BinOpImpl[LteOp, Double, Double, Boolean](_ <= _)
  implicit val lteDL: BinOp[LteOp, Double, Long, Boolean] =
    new BinOpImpl[LteOp, Double, Long, Boolean](_ <= _)
  implicit val lteDI: BinOp[LteOp, Double, Int, Boolean] =
    new BinOpImpl[LteOp, Double, Int, Boolean](_ <= _)
  implicit val lteLD: BinOp[LteOp, Long, Double, Boolean] =
    new BinOpImpl[LteOp, Long, Double, Boolean](_ <= _)
  implicit val lteLL: BinOp[LteOp, Long, Long, Boolean] =
    new BinOpImpl[LteOp, Long, Long, Boolean](_ <= _)
  implicit val lteLI: BinOp[LteOp, Long, Int, Boolean] =
    new BinOpImpl[LteOp, Long, Int, Boolean](_ <= _)
  implicit val lteID: BinOp[LteOp, Int, Double, Boolean] =
    new BinOpImpl[LteOp, Int, Double, Boolean](_ <= _)
  implicit val lteIL: BinOp[LteOp, Int, Long, Boolean] =
    new BinOpImpl[LteOp, Int, Long, Boolean](_ <= _)
  implicit val lteII: BinOp[LteOp, Int, Int, Boolean] =
    new BinOpImpl[LteOp, Int, Int, Boolean](_ <= _)
  implicit val lteBB: BinOp[LteOp, Boolean, Boolean, Boolean] =
    new BinOpImpl[LteOp, Boolean, Boolean, Boolean](_ <= _)
}
