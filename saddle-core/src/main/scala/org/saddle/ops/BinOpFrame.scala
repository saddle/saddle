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
 */

package org.saddle.ops

import scala.{ specialized => spec }

import org.saddle._
import index._
import vec._
import java.util.Date

/**
 * Provides concrete implementations of binary operations for [[org.saddle.Frame]]; these
 * instances provide implicit support for [[org.saddle.ops.NumericOps]] methods which are
 * inherited by Frame.
 */
trait BinOpFrame {
  // ***************

  // Binary element-wise operation on one frame and one scalar
  final class FrScEOp[OP <: ScalarOp, X: ST: ORD, Y: ST: ORD, A, B, C: ST](
      opv: BinOp[OP, Vec[A], B, Vec[C]]) extends BinOp[OP, Frame[X, Y, A], B, Frame[X, Y, C]] {
    def apply(v1: Frame[X, Y, A], v2: B) = v1.mapVec(opv(_, v2))
  }

  // concrete implementations
  implicit def FrScEOpDDD[Op <: ScalarOp, X, Y](implicit cm: ST[X], cmp: ORD[X], cmy: ST[Y], cmpy: ORD[Y], opv: BinOp[Op, Vec[Double], Double, Vec[Double]]) = new FrScEOp[Op, X, Y, Double, Double, Double](opv)
  implicit def FrScEOpDLD[Op <: ScalarOp, X, Y](implicit cm: ST[X], cmp: ORD[X], cmy: ST[Y], cmpy: ORD[Y], opv: BinOp[Op, Vec[Double], Long, Vec[Double]]) = new FrScEOp[Op, X, Y, Double, Long, Double](opv)
  implicit def FrScEOpDID[Op <: ScalarOp, X, Y](implicit cm: ST[X], cmp: ORD[X], cmy: ST[Y], cmpy: ORD[Y], opv: BinOp[Op, Vec[Double], Int, Vec[Double]]) = new FrScEOp[Op, X, Y, Double, Int, Double](opv)

  implicit def FrScEOpLDD[Op <: ScalarOp, X, Y](implicit cm: ST[X], cmp: ORD[X], cmy: ST[Y], cmpy: ORD[Y], opv: BinOp[Op, Vec[Long], Double, Vec[Double]]) = new FrScEOp[Op, X, Y, Long, Double, Double](opv)
  implicit def FrScEOpLLL[Op <: ScalarOp, X, Y](implicit cm: ST[X], cmp: ORD[X], cmy: ST[Y], cmpy: ORD[Y], opv: BinOp[Op, Vec[Long], Long, Vec[Long]]) = new FrScEOp[Op, X, Y, Long, Long, Long](opv)
  implicit def FrScEOpLIL[Op <: ScalarOp, X, Y](implicit cm: ST[X], cmp: ORD[X], cmy: ST[Y], cmpy: ORD[Y], opv: BinOp[Op, Vec[Long], Int, Vec[Long]]) = new FrScEOp[Op, X, Y, Long, Int, Long](opv)

  implicit def FrScEOpIDD[Op <: ScalarOp, X, Y](implicit cm: ST[X], cmp: ORD[X], cmy: ST[Y], cmpy: ORD[Y], opv: BinOp[Op, Vec[Int], Double, Vec[Double]]) = new FrScEOp[Op, X, Y, Int, Double, Double](opv)
  implicit def FrScEOpILL[Op <: ScalarOp, X, Y](implicit cm: ST[X], cmp: ORD[X], cmy: ST[Y], cmpy: ORD[Y], opv: BinOp[Op, Vec[Int], Long, Vec[Long]]) = new FrScEOp[Op, X, Y, Int, Long, Long](opv)
  implicit def FrScEOpIII[Op <: ScalarOp, X, Y](implicit cm: ST[X], cmp: ORD[X], cmy: ST[Y], cmpy: ORD[Y], opv: BinOp[Op, Vec[Int], Int, Vec[Int]]) = new FrScEOp[Op, X, Y, Int, Int, Int](opv)

  // comparisons
  implicit def FrScEOpDDB[Op <: ScalarOp, X, Y](implicit cm: ST[X], cmp: ORD[X], cmy: ST[Y], cmpy: ORD[Y], opv: BinOp[Op, Vec[Double], Double, Vec[Boolean]]) = new FrScEOp[Op, X, Y, Double, Double, Boolean](opv)
  implicit def FrScEOpDLB[Op <: ScalarOp, X, Y](implicit cm: ST[X], cmp: ORD[X], cmy: ST[Y], cmpy: ORD[Y], opv: BinOp[Op, Vec[Double], Long, Vec[Boolean]]) = new FrScEOp[Op, X, Y, Double, Long, Boolean](opv)
  implicit def FrScEOpDIB[Op <: ScalarOp, X, Y](implicit cm: ST[X], cmp: ORD[X], cmy: ST[Y], cmpy: ORD[Y], opv: BinOp[Op, Vec[Double], Int, Vec[Boolean]]) = new FrScEOp[Op, X, Y, Double, Int, Boolean](opv)

  implicit def FrScEOpLDB[Op <: ScalarOp, X, Y](implicit cm: ST[X], cmp: ORD[X], cmy: ST[Y], cmpy: ORD[Y], opv: BinOp[Op, Vec[Long], Double, Vec[Boolean]]) = new FrScEOp[Op, X, Y, Long, Double, Boolean](opv)
  implicit def FrScEOpLLB[Op <: ScalarOp, X, Y](implicit cm: ST[X], cmp: ORD[X], cmy: ST[Y], cmpy: ORD[Y], opv: BinOp[Op, Vec[Long], Long, Vec[Boolean]]) = new FrScEOp[Op, X, Y, Long, Long, Boolean](opv)
  implicit def FrScEOpLIB[Op <: ScalarOp, X, Y](implicit cm: ST[X], cmp: ORD[X], cmy: ST[Y], cmpy: ORD[Y], opv: BinOp[Op, Vec[Long], Int, Vec[Boolean]]) = new FrScEOp[Op, X, Y, Long, Int, Boolean](opv)

  implicit def FrScEOpIDB[Op <: ScalarOp, X, Y](implicit cm: ST[X], cmp: ORD[X], cmy: ST[Y], cmpy: ORD[Y], opv: BinOp[Op, Vec[Int], Double, Vec[Boolean]]) = new FrScEOp[Op, X, Y, Int, Double, Boolean](opv)
  implicit def FrScEOpILB[Op <: ScalarOp, X, Y](implicit cm: ST[X], cmp: ORD[X], cmy: ST[Y], cmpy: ORD[Y], opv: BinOp[Op, Vec[Int], Long, Vec[Boolean]]) = new FrScEOp[Op, X, Y, Int, Long, Boolean](opv)
  implicit def FrScEOpIIB[Op <: ScalarOp, X, Y](implicit cm: ST[X], cmp: ORD[X], cmy: ST[Y], cmpy: ORD[Y], opv: BinOp[Op, Vec[Int], Int, Vec[Boolean]]) = new FrScEOp[Op, X, Y, Int, Int, Boolean](opv)

  // and, or ops
  implicit def FrScEOpBBB[Op <: ScalarOp, X, Y](implicit cm: ST[X], cmp: ORD[X], cmy: ST[Y], cmpy: ORD[Y], opv: BinOp[Op, Vec[Boolean], Boolean, Vec[Boolean]]) = new FrScEOp[Op, X, Y, Boolean, Boolean, Boolean](opv)

  // ***************

  // Binary element-wise operation on two frames
  final class FrFrEOp[OP <: ScalarOp, X: ST: ORD, Y: ST: ORD, A, B: ST, C: ST](
      opv: BinOp[OP, Vec[A], Vec[B], Vec[C]]) extends BinOp[OP, Frame[X, Y, A], Frame[X, Y, B], Frame[X, Y, C]] {
    def apply(f1: Frame[X, Y, A], f2: Frame[X, Y, B]) = {
      val (l, r) = f1.align(f2, OuterJoin, OuterJoin)
      val result = l.values.zip(r.values) map { case (v1, v2) => opv(v1, v2) }
      Frame(result, l.rowIx, l.colIx)
    }
  }

  // concrete implementations

  implicit def FrFrEOpDDD[Op <: ScalarOp, X, Y](implicit cm: ST[X], cmp: ORD[X], my: ST[Y], cmpY: ORD[Y], opv: BinOp[Op, Vec[Double], Vec[Double], Vec[Double]]) = new FrFrEOp[Op, X, Y, Double, Double, Double](opv)
  implicit def FrFrEOpDID[Op <: ScalarOp, X, Y](implicit cm: ST[X], cmp: ORD[X], my: ST[Y], cmpY: ORD[Y], opv: BinOp[Op, Vec[Double], Vec[Int], Vec[Double]]) = new FrFrEOp[Op, X, Y, Double, Int, Double](opv)
  implicit def FrFrEOpDLD[Op <: ScalarOp, X, Y](implicit cm: ST[X], cmp: ORD[X], my: ST[Y], cmpY: ORD[Y], opv: BinOp[Op, Vec[Double], Vec[Long], Vec[Double]]) = new FrFrEOp[Op, X, Y, Double, Long, Double](opv)

  implicit def FrFrEOpLDD[Op <: ScalarOp, X, Y](implicit cm: ST[X], cmp: ORD[X], my: ST[Y], cmpY: ORD[Y], opv: BinOp[Op, Vec[Long], Vec[Double], Vec[Double]]) = new FrFrEOp[Op, X, Y, Long, Double, Double](opv)
  implicit def FrFrEOpLLL[Op <: ScalarOp, X, Y](implicit cm: ST[X], cmp: ORD[X], my: ST[Y], cmpY: ORD[Y], opv: BinOp[Op, Vec[Long], Vec[Long], Vec[Long]]) = new FrFrEOp[Op, X, Y, Long, Long, Long](opv)
  implicit def FrFrEOpLIL[Op <: ScalarOp, X, Y](implicit cm: ST[X], cmp: ORD[X], my: ST[Y], cmpY: ORD[Y], opv: BinOp[Op, Vec[Long], Vec[Int], Vec[Long]]) = new FrFrEOp[Op, X, Y, Long, Int, Long](opv)

  implicit def FrFrEOpIDD[Op <: ScalarOp, X, Y](implicit cm: ST[X], cmp: ORD[X], my: ST[Y], cmpY: ORD[Y], opv: BinOp[Op, Vec[Int], Vec[Double], Vec[Double]]) = new FrFrEOp[Op, X, Y, Int, Double, Double](opv)
  implicit def FrFrEOpILL[Op <: ScalarOp, X, Y](implicit cm: ST[X], cmp: ORD[X], my: ST[Y], cmpY: ORD[Y], opv: BinOp[Op, Vec[Int], Vec[Long], Vec[Long]]) = new FrFrEOp[Op, X, Y, Int, Long, Long](opv)
  implicit def FrFrEOpIII[Op <: ScalarOp, X, Y](implicit cm: ST[X], cmp: ORD[X], my: ST[Y], cmpY: ORD[Y], opv: BinOp[Op, Vec[Int], Vec[Int], Vec[Int]]) = new FrFrEOp[Op, X, Y, Int, Int, Int](opv)

  // comparisons
  implicit def FrFrEOpDDB[Op <: ScalarOp, X, Y](implicit cm: ST[X], cmp: ORD[X], my: ST[Y], cmpY: ORD[Y], opv: BinOp[Op, Vec[Double], Vec[Double], Vec[Boolean]]) = new FrFrEOp[Op, X, Y, Double, Double, Boolean](opv)
  implicit def FrFrEOpDLB[Op <: ScalarOp, X, Y](implicit cm: ST[X], cmp: ORD[X], my: ST[Y], cmpY: ORD[Y], opv: BinOp[Op, Vec[Double], Vec[Long], Vec[Boolean]]) = new FrFrEOp[Op, X, Y, Double, Long, Boolean](opv)
  implicit def FrFrEOpDIB[Op <: ScalarOp, X, Y](implicit cm: ST[X], cmp: ORD[X], my: ST[Y], cmpY: ORD[Y], opv: BinOp[Op, Vec[Double], Vec[Int], Vec[Boolean]]) = new FrFrEOp[Op, X, Y, Double, Int, Boolean](opv)

  implicit def FrFrEOpLDB[Op <: ScalarOp, X, Y](implicit cm: ST[X], cmp: ORD[X], my: ST[Y], cmpY: ORD[Y], opv: BinOp[Op, Vec[Long], Vec[Double], Vec[Boolean]]) = new FrFrEOp[Op, X, Y, Long, Double, Boolean](opv)
  implicit def FrFrEOpLLB[Op <: ScalarOp, X, Y](implicit cm: ST[X], cmp: ORD[X], my: ST[Y], cmpY: ORD[Y], opv: BinOp[Op, Vec[Long], Vec[Long], Vec[Boolean]]) = new FrFrEOp[Op, X, Y, Long, Long, Boolean](opv)
  implicit def FrFrEOpLIB[Op <: ScalarOp, X, Y](implicit cm: ST[X], cmp: ORD[X], my: ST[Y], cmpY: ORD[Y], opv: BinOp[Op, Vec[Long], Vec[Int], Vec[Boolean]]) = new FrFrEOp[Op, X, Y, Long, Int, Boolean](opv)

  implicit def FrFrEOpIDB[Op <: ScalarOp, X, Y](implicit cm: ST[X], cmp: ORD[X], my: ST[Y], cmpY: ORD[Y], opv: BinOp[Op, Vec[Int], Vec[Double], Vec[Boolean]]) = new FrFrEOp[Op, X, Y, Int, Double, Boolean](opv)
  implicit def FrFrEOpILB[Op <: ScalarOp, X, Y](implicit cm: ST[X], cmp: ORD[X], my: ST[Y], cmpY: ORD[Y], opv: BinOp[Op, Vec[Int], Vec[Long], Vec[Boolean]]) = new FrFrEOp[Op, X, Y, Int, Long, Boolean](opv)
  implicit def FrFrEOpIIB[Op <: ScalarOp, X, Y](implicit cm: ST[X], cmp: ORD[X], my: ST[Y], cmpY: ORD[Y], opv: BinOp[Op, Vec[Int], Vec[Int], Vec[Boolean]]) = new FrFrEOp[Op, X, Y, Int, Int, Boolean](opv)

  // and, or ops
  implicit def FrFrEOpBBB[Op <: ScalarOp, X, Y](implicit cm: ST[X], cmp: ORD[X], my: ST[Y], cmpY: ORD[Y], opv: BinOp[Op, Vec[Boolean], Vec[Boolean], Vec[Boolean]]) = new FrFrEOp[Op, X, Y, Boolean, Boolean, Boolean](opv)

  // ***************
  // Dot Product Between 2 Frames
  // Number of Rows and Columns respectively in both Frames should be equal to each other
  final class FrFrDot[X: ST: ORD, Y: ST: ORD, @spec(Int, Long, Double) A, @spec(Int, Long, Double) B: ST, @spec(Int, Long, Double) C: ST: NUM](
    val opmul: BinOp[Multiply, Frame[X, Y, A], Frame[X, Y, B], Frame[X, Y, C]],
    val opadd: BinOp[Add, Series[X, C], Series[X, C], Series[X, C]])
      extends BinOp[InnerProd, Frame[X, Y, A], Frame[X, Y, B], Series[X, C]] {
    def apply(f1: Frame[X, Y, A], f2: Frame[X, Y, B]): Series[X, C] = {
      require(f1.colIx.length == f2.colIx.length, "Frames must have the same number of columns!")
      require(f1.rowIx.length == f2.rowIx.length, "Frames must have the same number of rows!")

      val (l, r) = f1.align(f2, OuterJoin, OuterJoin)
      val mul = opmul(l, r)
      val result = mul.toColSeq.foldLeft(Series(Vec(array.empty[C](mul.rowIx.length)), mul.rowIx))((prev, element) => opadd(prev, element._2))
      result
    }
  }

  implicit def FrFrDotDDD[X, Y](implicit cm: ST[X], cmp: ORD[X], my: ST[Y], cmpY: ORD[Y], opmul: BinOp[Multiply, Frame[X, Y, Double], Frame[X, Y, Double], Frame[X, Y, Double]], opadd: BinOp[Add, Series[X, Double], Series[X, Double], Series[X, Double]]) = new FrFrDot[X, Y, Double, Double, Double](opmul, opadd)
  implicit def FrFrDotIntIntDDD(implicit cm: ST[Int], cmp: ORD[Int], opmul: BinOp[Multiply, Frame[Int, Int, Double], Frame[Int, Int, Double], Frame[Int, Int, Double]], opadd: BinOp[Add, Series[Int, Double], Series[Int, Double], Series[Int, Double]]) = new FrFrDot[Int, Int, Double, Double, Double](opmul, opadd)
  implicit def FrFrDotIntIntLLL(implicit cm: ST[Int], cmp: ORD[Int], opmul: BinOp[Multiply, Frame[Int, Int, Long], Frame[Int, Int, Long], Frame[Int, Int, Long]], opadd: BinOp[Add, Series[Int, Long], Series[Int, Long], Series[Int, Long]]) = new FrFrDot[Int, Int, Long, Long, Long](opmul, opadd)
  implicit def FrFrDotIntIntIII(implicit cm: ST[Int], cmp: ORD[Int], opmul: BinOp[Multiply, Frame[Int, Int, Int], Frame[Int, Int, Int], Frame[Int, Int, Int]], opadd: BinOp[Add, Series[Int, Int], Series[Int, Int], Series[Int, Int]]) = new FrFrDot[Int, Int, Int, Int, Int](opmul, opadd)

  implicit def FrFrDotDateStringDDD(implicit cm: ST[Date], cmp: ORD[Date], my: ST[String], cmpString: ORD[String], opmul: BinOp[Multiply, Frame[Date, String, Double], Frame[Date, String, Double], Frame[Date, String, Double]], opadd: BinOp[Add, Series[Date, Double], Series[Date, Double], Series[Date, Double]]) = new FrFrDot[Date, String, Double, Double, Double](opmul, opadd)
}
