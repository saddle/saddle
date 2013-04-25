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

import org.saddle._
import index._

/**
 * Provides concrete implementations of binary operations for [[org.saddle.Series]]; these
 * instances provide implicit support for [[org.saddle.ops.NumericOps]] methods which are
 * inherited by Series.
 */
trait BinOpSeries {
  // ***************

  // Binary element-wise operation on one series and one scalar
  final class SrScEOp[OP <: ScalarOp, X: ST: ORD, A, B, C: ST](
                      op: BinOp[OP, Vec[A], B, Vec[C]]) extends BinOp[OP, Series[X, A], B, Series[X, C]] {
    def apply(v1: Series[X, A], v2: B) = Series(op(v1.values, v2), v1.index)
  }

  // concrete implementations
  implicit def SrScEOpDDD[Op <: ScalarOp, X](implicit cm: ST[X], cmp: ORD[X], opv: BinOp[Op, Vec[Double], Double, Vec[Double]]) = new SrScEOp[Op, X, Double, Double, Double](opv)
  implicit def SrScEOpDLD[Op <: ScalarOp, X](implicit cm: ST[X], cmp: ORD[X], opv: BinOp[Op, Vec[Double], Long,   Vec[Double]]) = new SrScEOp[Op, X, Double, Long,   Double](opv)
  implicit def SrScEOpDID[Op <: ScalarOp, X](implicit cm: ST[X], cmp: ORD[X], opv: BinOp[Op, Vec[Double], Int,    Vec[Double]]) = new SrScEOp[Op, X, Double, Int,    Double](opv)

  implicit def SrScEOpLDD[Op <: ScalarOp, X](implicit cm: ST[X], cmp: ORD[X], opv: BinOp[Op, Vec[Long],   Double, Vec[Double]]) = new SrScEOp[Op, X, Long, Double, Double](opv)
  implicit def SrScEOpLLL[Op <: ScalarOp, X](implicit cm: ST[X], cmp: ORD[X], opv: BinOp[Op, Vec[Long],   Long,   Vec[Long]])   = new SrScEOp[Op, X, Long, Long,   Long](opv)
  implicit def SrScEOpLIL[Op <: ScalarOp, X](implicit cm: ST[X], cmp: ORD[X], opv: BinOp[Op, Vec[Long],   Int,    Vec[Long]])   = new SrScEOp[Op, X, Long, Int,    Long](opv)

  implicit def SrScEOpIDD[Op <: ScalarOp, X](implicit cm: ST[X], cmp: ORD[X], opv: BinOp[Op, Vec[Int],    Double, Vec[Double]]) = new SrScEOp[Op, X, Int, Double, Double](opv)
  implicit def SrScEOpILL[Op <: ScalarOp, X](implicit cm: ST[X], cmp: ORD[X], opv: BinOp[Op, Vec[Int],    Long,   Vec[Long]])   = new SrScEOp[Op, X, Int, Long,   Long](opv)
  implicit def SrScEOpIII[Op <: ScalarOp, X](implicit cm: ST[X], cmp: ORD[X], opv: BinOp[Op, Vec[Int],    Int,    Vec[Int]])    = new SrScEOp[Op, X, Int, Int,    Int](opv)

  // comparisons
  implicit def SrScEOpDDB[Op <: ScalarOp, X](implicit cm: ST[X], cmp: ORD[X], opv: BinOp[Op, Vec[Double], Double, Vec[Boolean]]) = new SrScEOp[Op, X, Double, Double, Boolean](opv)
  implicit def SrScEOpDLB[Op <: ScalarOp, X](implicit cm: ST[X], cmp: ORD[X], opv: BinOp[Op, Vec[Double], Long,   Vec[Boolean]]) = new SrScEOp[Op, X, Double, Long,   Boolean](opv)
  implicit def SrScEOpDIB[Op <: ScalarOp, X](implicit cm: ST[X], cmp: ORD[X], opv: BinOp[Op, Vec[Double], Int,    Vec[Boolean]]) = new SrScEOp[Op, X, Double, Int,    Boolean](opv)

  implicit def SrScEOpLDB[Op <: ScalarOp, X](implicit cm: ST[X], cmp: ORD[X], opv: BinOp[Op, Vec[Long],   Double, Vec[Boolean]]) = new SrScEOp[Op, X, Long,   Double, Boolean](opv)
  implicit def SrScEOpLLB[Op <: ScalarOp, X](implicit cm: ST[X], cmp: ORD[X], opv: BinOp[Op, Vec[Long],   Long,   Vec[Boolean]]) = new SrScEOp[Op, X, Long,   Long,   Boolean](opv)
  implicit def SrScEOpLIB[Op <: ScalarOp, X](implicit cm: ST[X], cmp: ORD[X], opv: BinOp[Op, Vec[Long],   Int,    Vec[Boolean]]) = new SrScEOp[Op, X, Long,   Int,    Boolean](opv)

  implicit def SrScEOpIDB[Op <: ScalarOp, X](implicit cm: ST[X], cmp: ORD[X], opv: BinOp[Op, Vec[Int],    Double, Vec[Boolean]]) = new SrScEOp[Op, X, Int,    Double, Boolean](opv)
  implicit def SrScEOpILB[Op <: ScalarOp, X](implicit cm: ST[X], cmp: ORD[X], opv: BinOp[Op, Vec[Int],    Long,   Vec[Boolean]]) = new SrScEOp[Op, X, Int,    Long,   Boolean](opv)
  implicit def SrScEOpIIB[Op <: ScalarOp, X](implicit cm: ST[X], cmp: ORD[X], opv: BinOp[Op, Vec[Int],    Int,    Vec[Boolean]]) = new SrScEOp[Op, X, Int,    Int,    Boolean](opv)

  // and, or ops
  implicit def SrScEOpBBB[Op <: ScalarOp, X](implicit cm: ST[X], cmp: ORD[X], opv: BinOp[Op, Vec[Boolean], Boolean, Vec[Boolean]]) = new SrScEOp[Op, X, Boolean, Boolean, Boolean](opv)

  // ***************

  // Binary element-wise operation on two series
  final class SrSrEOp[OP <: ScalarOp, X: ST: ORD, A, B, C: ST](
                      opv: BinOp[OP, Vec[A], Vec[B], Vec[C]]) extends BinOp[OP, Series[X, A], Series[X, B], Series[X, C]] {
    def apply(v1: Series[X, A], v2: Series[X, B]) = {
      if (v1.index == v2.index) {
        Series(opv(v1.values, v2.values), v1.index)
      }
      else {
        val joined = v1.index.join(v2.index, OuterJoin)
        val lvec: Vec[A] = joined.lTake.map(locs => v1.values.take(locs)).getOrElse(v1.values)
        val rvec: Vec[B] = joined.rTake.map(locs => v2.values.take(locs)).getOrElse(v2.values)
        Series(opv(lvec, rvec), joined.index)
      }
    }
  }

  // concrete implementations

  implicit def SrSrEOpDDD[Op <: ScalarOp, X](implicit cm: ST[X], cmp: ORD[X], opv: BinOp[Op, Vec[Double], Vec[Double], Vec[Double]]) = new SrSrEOp[Op, X, Double, Double, Double](opv)
  implicit def SrSrEOpDID[Op <: ScalarOp, X](implicit cm: ST[X], cmp: ORD[X], opv: BinOp[Op, Vec[Double], Vec[Int],    Vec[Double]]) = new SrSrEOp[Op, X, Double, Int,    Double](opv)
  implicit def SrSrEOpDLD[Op <: ScalarOp, X](implicit cm: ST[X], cmp: ORD[X], opv: BinOp[Op, Vec[Double], Vec[Long],   Vec[Double]]) = new SrSrEOp[Op, X, Double, Long,   Double](opv)

  implicit def SrSrEOpLDD[Op <: ScalarOp, X](implicit cm: ST[X], cmp: ORD[X], opv: BinOp[Op, Vec[Long],   Vec[Double], Vec[Double]]) = new SrSrEOp[Op, X, Long, Double, Double](opv)
  implicit def SrSrEOpLLL[Op <: ScalarOp, X](implicit cm: ST[X], cmp: ORD[X], opv: BinOp[Op, Vec[Long],   Vec[Long],   Vec[Long]])   = new SrSrEOp[Op, X, Long, Long,   Long](opv)
  implicit def SrSrEOpLIL[Op <: ScalarOp, X](implicit cm: ST[X], cmp: ORD[X], opv: BinOp[Op, Vec[Long],   Vec[Int],    Vec[Long]])   = new SrSrEOp[Op, X, Long, Int,    Long](opv)

  implicit def SrSrEOpIDD[Op <: ScalarOp, X](implicit cm: ST[X], cmp: ORD[X], opv: BinOp[Op, Vec[Int],    Vec[Double], Vec[Double]]) = new SrSrEOp[Op, X, Int, Double, Double](opv)
  implicit def SrSrEOpILL[Op <: ScalarOp, X](implicit cm: ST[X], cmp: ORD[X], opv: BinOp[Op, Vec[Int],    Vec[Long],   Vec[Long]])   = new SrSrEOp[Op, X, Int, Long,   Long](opv)
  implicit def SrSrEOpIII[Op <: ScalarOp, X](implicit cm: ST[X], cmp: ORD[X], opv: BinOp[Op, Vec[Int],    Vec[Int],    Vec[Int]])    = new SrSrEOp[Op, X, Int, Int,    Int](opv)

  // comparisons
  implicit def SrSrEOpDDB[Op <: ScalarOp, X](implicit cm: ST[X], cmp: ORD[X], opv: BinOp[Op, Vec[Double], Vec[Double], Vec[Boolean]]) = new SrSrEOp[Op, X, Double, Double, Boolean](opv)
  implicit def SrSrEOpDLB[Op <: ScalarOp, X](implicit cm: ST[X], cmp: ORD[X], opv: BinOp[Op, Vec[Double], Vec[Long],   Vec[Boolean]]) = new SrSrEOp[Op, X, Double, Long,   Boolean](opv)
  implicit def SrSrEOpDIB[Op <: ScalarOp, X](implicit cm: ST[X], cmp: ORD[X], opv: BinOp[Op, Vec[Double], Vec[Int],    Vec[Boolean]]) = new SrSrEOp[Op, X, Double, Int,    Boolean](opv)

  implicit def SrSrEOpLDB[Op <: ScalarOp, X](implicit cm: ST[X], cmp: ORD[X], opv: BinOp[Op, Vec[Long],   Vec[Double], Vec[Boolean]]) = new SrSrEOp[Op, X, Long,   Double, Boolean](opv)
  implicit def SrSrEOpLLB[Op <: ScalarOp, X](implicit cm: ST[X], cmp: ORD[X], opv: BinOp[Op, Vec[Long],   Vec[Long],   Vec[Boolean]]) = new SrSrEOp[Op, X, Long,   Long,   Boolean](opv)
  implicit def SrSrEOpLIB[Op <: ScalarOp, X](implicit cm: ST[X], cmp: ORD[X], opv: BinOp[Op, Vec[Long],   Vec[Int],    Vec[Boolean]]) = new SrSrEOp[Op, X, Long,   Int,    Boolean](opv)

  implicit def SrSrEOpIDB[Op <: ScalarOp, X](implicit cm: ST[X], cmp: ORD[X], opv: BinOp[Op, Vec[Int],    Vec[Double], Vec[Boolean]]) = new SrSrEOp[Op, X, Int,    Double, Boolean](opv)
  implicit def SrSrEOpILB[Op <: ScalarOp, X](implicit cm: ST[X], cmp: ORD[X], opv: BinOp[Op, Vec[Int],    Vec[Long],   Vec[Boolean]]) = new SrSrEOp[Op, X, Int,    Long,   Boolean](opv)
  implicit def SrSrEOpIIB[Op <: ScalarOp, X](implicit cm: ST[X], cmp: ORD[X], opv: BinOp[Op, Vec[Int],    Vec[Int],    Vec[Boolean]]) = new SrSrEOp[Op, X, Int,    Int,    Boolean](opv)

  // and, or ops
  implicit def SrSrEOpBBB[Op <: ScalarOp, X](implicit cm: ST[X], cmp: ORD[X], opv: BinOp[Op, Vec[Boolean], Vec[Boolean], Vec[Boolean]]) = new SrSrEOp[Op, X, Boolean, Boolean, Boolean](opv)
}
