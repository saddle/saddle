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

import scala.{specialized => spec}

import org.saddle._

/**
 * Provides concrete implementations of binary operations for [[org.saddle.Vec]]; these
 * instances provide implicit support for [[org.saddle.ops.NumericOps]] methods which are
 * inherited by Vec.
 */
trait BinOpVec {
  // ***************

  // Binary element-wise operation on one Vec and one scalar
  final class VecSclrElemOp[OP <: ScalarOp, @spec(Boolean, Int, Long, Double) A, @spec(Boolean, Int, Long, Double) B, @spec(Boolean, Int, Long, Double) C: ST](
    val op: BinOp[OP, A, B, C]) extends BinOp[OP, Vec[A], B, Vec[C]] {
    def apply(v1: Vec[A], v2: B) = {
      val sz = v1.length
      val ar = new Array[C](sz)
      var i = 0
      while (i < sz) {
        ar(i) = op(v1(i), v2)
        i += 1
      }
      Vec(ar)
    }
  }

  // math ops
  implicit def VecSclrElmOpDDD[Op <: ScalarOp](implicit op: BinOp[Op, Double, Double, Double]) = new VecSclrElemOp[Op, Double, Double, Double](op)
  implicit def VecSclrElmOpDLD[Op <: ScalarOp](implicit op: BinOp[Op, Double, Long,   Double]) = new VecSclrElemOp[Op, Double, Long,   Double](op)
  implicit def VecSclrElmOpDID[Op <: ScalarOp](implicit op: BinOp[Op, Double, Int,    Double]) = new VecSclrElemOp[Op, Double, Int,    Double](op)

  implicit def VecSclrElmOpLDD[Op <: ScalarOp](implicit op: BinOp[Op, Long,   Double, Double]) = new VecSclrElemOp[Op, Long,   Double, Double](op)
  implicit def VecSclrElmOpLLL[Op <: ScalarOp](implicit op: BinOp[Op, Long,   Long,   Long])   = new VecSclrElemOp[Op, Long,   Long,   Long](op)
  implicit def VecSclrElmOpLIL[Op <: ScalarOp](implicit op: BinOp[Op, Long,   Int,    Long])   = new VecSclrElemOp[Op, Long,   Int,    Long](op)

  implicit def VecSclrElmOpIDD[Op <: ScalarOp](implicit op: BinOp[Op, Int,    Double, Double]) = new VecSclrElemOp[Op, Int,    Double, Double](op)
  implicit def VecSclrElmOpILL[Op <: ScalarOp](implicit op: BinOp[Op, Int,    Long,   Long])   = new VecSclrElemOp[Op, Int,    Long,   Long](op)
  implicit def VecSclrElmOpIII[Op <: ScalarOp](implicit op: BinOp[Op, Int,    Int,    Int])    = new VecSclrElemOp[Op, Int,    Int,    Int](op)

  // comparisons
  implicit def VecSclrElmOpDDB[Op <: ScalarOp](implicit op: BinOp[Op, Double, Double, Boolean]) = new VecSclrElemOp[Op, Double, Double, Boolean](op)
  implicit def VecSclrElmOpDLB[Op <: ScalarOp](implicit op: BinOp[Op, Double, Long,   Boolean]) = new VecSclrElemOp[Op, Double, Long,   Boolean](op)
  implicit def VecSclrElmOpDIB[Op <: ScalarOp](implicit op: BinOp[Op, Double, Int,    Boolean]) = new VecSclrElemOp[Op, Double, Int,    Boolean](op)

  implicit def VecSclrElmOpLDB[Op <: ScalarOp](implicit op: BinOp[Op, Long,   Double, Boolean]) = new VecSclrElemOp[Op, Long,   Double, Boolean](op)
  implicit def VecSclrElmOpLLB[Op <: ScalarOp](implicit op: BinOp[Op, Long,   Long,   Boolean]) = new VecSclrElemOp[Op, Long,   Long,   Boolean](op)
  implicit def VecSclrElmOpLIB[Op <: ScalarOp](implicit op: BinOp[Op, Long,   Int,    Boolean]) = new VecSclrElemOp[Op, Long,   Int,    Boolean](op)

  implicit def VecSclrElmOpIDB[Op <: ScalarOp](implicit op: BinOp[Op, Int,    Double, Boolean]) = new VecSclrElemOp[Op, Int,    Double, Boolean](op)
  implicit def VecSclrElmOpILB[Op <: ScalarOp](implicit op: BinOp[Op, Int,    Long,   Boolean]) = new VecSclrElemOp[Op, Int,    Long,   Boolean](op)
  implicit def VecSclrElmOpIIB[Op <: ScalarOp](implicit op: BinOp[Op, Int,    Int,    Boolean]) = new VecSclrElemOp[Op, Int,    Int,    Boolean](op)

  // and, or ops
  implicit def VecSclrElmOpBBB[Op <: ScalarOp](implicit op: BinOp[Op, Boolean, Boolean, Boolean]) = new VecSclrElemOp[Op, Boolean, Boolean, Boolean](op)

  // ***************

  // Binary element-wise operation on two Vecs
  final class VecVecElemOp[OP <: ScalarOp, @spec(Boolean, Int, Long, Double) A, @spec(Boolean, Int, Long, Double) B, @spec(Boolean, Int, Long, Double) C: ST](
    op: BinOp[OP, A, B, C]) extends BinOp[OP, Vec[A], Vec[B], Vec[C]] {

    def apply(v1: Vec[A], v2: Vec[B]) = {
      require(v1.length == v2.length, "Vecs must have the same size!")
      val sz = v1.length
      val ar = new Array[C](sz)
      var i = 0
      while (i < sz) {
        ar(i) = op(v1(i), v2(i))
        i += 1
      }
      Vec(ar)
    }
  }

  // math ops
  implicit def VecVelElemOpDDD[Op <: ScalarOp](implicit op: BinOp[Op, Double, Double, Double]) = new VecVecElemOp[Op, Double, Double, Double](op)
  implicit def VecVelElemOpDLD[Op <: ScalarOp](implicit op: BinOp[Op, Double, Long,   Double]) = new VecVecElemOp[Op, Double, Long,   Double](op)
  implicit def VecVelElemOpDID[Op <: ScalarOp](implicit op: BinOp[Op, Double, Int,    Double]) = new VecVecElemOp[Op, Double, Int,    Double](op)

  implicit def VecVelElemOpLDD[Op <: ScalarOp](implicit op: BinOp[Op, Long,   Double, Double]) = new VecVecElemOp[Op, Long,   Double, Double](op)
  implicit def VecVelElemOpLLL[Op <: ScalarOp](implicit op: BinOp[Op, Long,   Long,   Long])   = new VecVecElemOp[Op, Long,   Long,   Long](op)
  implicit def VecVelElemOpLIL[Op <: ScalarOp](implicit op: BinOp[Op, Long,   Int,    Long])   = new VecVecElemOp[Op, Long,   Int,    Long](op)

  implicit def VecVelElemOpIDD[Op <: ScalarOp](implicit op: BinOp[Op, Int,    Double, Double]) = new VecVecElemOp[Op, Int,    Double, Double](op)
  implicit def VecVelElemOpILL[Op <: ScalarOp](implicit op: BinOp[Op, Int,    Long,   Long])   = new VecVecElemOp[Op, Int,    Long,   Long](op)
  implicit def VecVelElemOpIII[Op <: ScalarOp](implicit op: BinOp[Op, Int,    Int,    Int])    = new VecVecElemOp[Op, Int,    Int,    Int](op)

  // comparisons
  implicit def VecVecElemOpDDB[Op <: ScalarOp](implicit op: BinOp[Op, Double, Double, Boolean]) = new VecVecElemOp[Op, Double, Double, Boolean](op)
  implicit def VecVecElemOpDLB[Op <: ScalarOp](implicit op: BinOp[Op, Double, Long,   Boolean]) = new VecVecElemOp[Op, Double, Long,   Boolean](op)
  implicit def VecVecElemOpDIB[Op <: ScalarOp](implicit op: BinOp[Op, Double, Int,    Boolean]) = new VecVecElemOp[Op, Double, Int,    Boolean](op)

  implicit def VecVecElemOpLDB[Op <: ScalarOp](implicit op: BinOp[Op, Long,   Double, Boolean]) = new VecVecElemOp[Op, Long,   Double, Boolean](op)
  implicit def VecVecElemOpLLB[Op <: ScalarOp](implicit op: BinOp[Op, Long,   Long,   Boolean]) = new VecVecElemOp[Op, Long,   Long,   Boolean](op)
  implicit def VecVecElemOpLIB[Op <: ScalarOp](implicit op: BinOp[Op, Long,   Int,    Boolean]) = new VecVecElemOp[Op, Long,   Int,    Boolean](op)

  implicit def VecVecElemOpIDB[Op <: ScalarOp](implicit op: BinOp[Op, Int,    Double, Boolean]) = new VecVecElemOp[Op, Int,    Double, Boolean](op)
  implicit def VecVecElemOpILB[Op <: ScalarOp](implicit op: BinOp[Op, Int,    Long,   Boolean]) = new VecVecElemOp[Op, Int,    Long,   Boolean](op)
  implicit def VecVecElemOpIIB[Op <: ScalarOp](implicit op: BinOp[Op, Int,    Int,    Boolean]) = new VecVecElemOp[Op, Int,    Int,    Boolean](op)

  // and, or ops
  implicit def VecVecElemOpBBB[Op <: ScalarOp](implicit op: BinOp[Op, Boolean, Boolean, Boolean]) = new VecVecElemOp[Op, Boolean, Boolean, Boolean](op)

  // ***************

  // Binary dot product of two vectors

  final class VecVecDot[@spec(Int, Long, Double) A, @spec(Int, Long, Double) B, @spec(Int, Long, Double) C: ST: NUM](
    opadd: BinOp[Add, C, C, C], opmul: BinOp[Multiply, A, B, C]) extends BinOp[InnerProd, Vec[A], Vec[B], C] {

    def apply(v1: Vec[A], v2: Vec[B]) = {
      require(v1.length == v2.length, "Vecs must have the same size!")
      val sz = v1.length
      val ar = new Array[C](sz)
      var i = 0
      while (i < sz) {
        ar(i) = opmul(v1(i), v2(i))
        i += 1
      }
      Vec(ar).foldLeft(implicitly[ST[C]].zero)(opadd.apply)
    }
  }

  implicit def VecVecDotOpDDD(implicit opA: BinOp[Add, Double, Double, Double], opM: BinOp[Multiply, Double, Double, Double]) = new VecVecDot[Double, Double, Double](opA, opM)
  implicit def VecVecDotOpDLD(implicit opA: BinOp[Add, Double, Double, Double], opM: BinOp[Multiply, Double, Long,   Double]) = new VecVecDot[Double, Long,   Double](opA, opM)
  implicit def VecVecDotOpDID(implicit opA: BinOp[Add, Double, Double, Double], opM: BinOp[Multiply, Double, Int,    Double]) = new VecVecDot[Double, Int,    Double](opA, opM)

  implicit def VecVecDotOpLDD(implicit opA: BinOp[Add, Double, Double, Double], opM: BinOp[Multiply, Long,   Double, Double]) = new VecVecDot[Long,   Double, Double](opA, opM)
  implicit def VecVecDotOpLLL(implicit opA: BinOp[Add, Long,   Long,   Long]  , opM: BinOp[Multiply, Long,   Long,   Long])   = new VecVecDot[Long,   Long,   Long  ](opA, opM)
  implicit def VecVecDotOpLIL(implicit opA: BinOp[Add, Long,   Long,   Long]  , opM: BinOp[Multiply, Long,   Int,    Long])   = new VecVecDot[Long,   Int,    Long  ](opA, opM)

  implicit def VecVecDotOpIDD(implicit opA: BinOp[Add, Double, Double, Double], opM: BinOp[Multiply, Int,    Double, Double]) = new VecVecDot[Int,    Double, Double](opA, opM)
  implicit def VecVecDotOpILL(implicit opA: BinOp[Add, Long,   Long,   Long]  , opM: BinOp[Multiply, Int,    Long,   Long])   = new VecVecDot[Int,    Long,   Long  ](opA, opM)
  implicit def VecVecDotOpIII(implicit opA: BinOp[Add, Int,    Int,    Int]   , opM: BinOp[Multiply, Int,    Int,    Int])    = new VecVecDot[Int,    Int,    Int   ](opA, opM)

  // Binary outer product of two vectors

  final class VecVecOuter[@spec(Int, Long, Double) A, @spec(Int, Long, Double) B, @spec(Int, Long, Double) C: ST](
    opmul: BinOp[Multiply, A, B, C]) extends BinOp[OuterProd, Vec[A], Vec[B], Mat[C]] {

    def apply(v1: Vec[A], v2: Vec[B]) = {
      val rc = v1.length
      val cc = v2.length
      val values = Array.ofDim[C](rc * cc)

      var i = 0
      while (i < rc) {
        var j = 0
        while(j < cc) {
          values(i * cc + j) = opmul(v1(i), v2(j))
          j += 1
        }
        i += 1
      }

      Mat[C](rc, cc, values)
    }
  }

  implicit def VecVecOuterOpDDD(implicit opM: BinOp[Multiply, Double, Double, Double]) = new VecVecOuter[Double, Double, Double](opM)
  implicit def VecVecOuterOpDLD(implicit opM: BinOp[Multiply, Double, Long,   Double]) = new VecVecOuter[Double, Long,   Double](opM)
  implicit def VecVecOuterOpDID(implicit opM: BinOp[Multiply, Double, Int,    Double]) = new VecVecOuter[Double, Int,    Double](opM)

  implicit def VecVecOuterOpLDD(implicit opM: BinOp[Multiply, Long,   Double, Double]) = new VecVecOuter[Long,   Double, Double](opM)
  implicit def VecVecOuterOpLLL(implicit opM: BinOp[Multiply, Long,   Long,   Long])   = new VecVecOuter[Long,   Long,   Long  ](opM)
  implicit def VecVecOuterOpLIL(implicit opM: BinOp[Multiply, Long,   Int,    Long])   = new VecVecOuter[Long,   Int,    Long  ](opM)

  implicit def VecVecOuterOpIDD(implicit opM: BinOp[Multiply, Int,    Double, Double]) = new VecVecOuter[Int,    Double, Double](opM)
  implicit def VecVecOuterOpILL(implicit opM: BinOp[Multiply, Int,    Long,   Long])   = new VecVecOuter[Int,    Long,   Long  ](opM)
  implicit def VecVecOuterOpIII(implicit opM: BinOp[Multiply, Int,    Int,    Int])    = new VecVecOuter[Int,    Int,    Int   ](opM)
}
