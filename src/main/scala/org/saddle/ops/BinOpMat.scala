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

import scala.{ specialized => spec }

import org.saddle._

/**
 * Provides concrete implementations of binary operations for [[org.saddle.Mat]]; these
 * instances provide implicit support for [[org.saddle.ops.NumericOps]] methods which are
 * inherited by Mat.
 */
trait BinOpMat {
  // ***************

  // Binary element-wise operation on one Mat and one scalar
  final class MatSclrElemOp[OP <: ScalarOp, @spec(Int, Long, Double) A, @spec(Int, Long, Double) B, @spec(Int, Long, Double) C: ST](
    val op: BinOp[OP, A, B, C])extends BinOp[OP, Mat[A], B, Mat[C]] {
    def apply(v1: Mat[A], v2: B) = {
      val sz = v1.length
      val ar = new Array[C](sz)
      var i = 0
      while (i < sz) {
        ar(i) = op(v1.raw(i), v2)
        i += 1
      }
      Mat(v1.numRows, v1.numCols, ar)
    }
  }

  // concrete implementations
  implicit def MatSclrElmOpDDD[Op <: ScalarOp](implicit op: BinOp[Op, Double, Double, Double]) = new MatSclrElemOp[Op, Double, Double, Double](op)
  implicit def MatSclrElmOpDLD[Op <: ScalarOp](implicit op: BinOp[Op, Double, Long,   Double]) = new MatSclrElemOp[Op, Double, Long,   Double](op)
  implicit def MatSclrElmOpDID[Op <: ScalarOp](implicit op: BinOp[Op, Double, Int,    Double]) = new MatSclrElemOp[Op, Double, Int,    Double](op)

  implicit def MatSclrElmOpLDD[Op <: ScalarOp](implicit op: BinOp[Op, Long,   Double, Double]) = new MatSclrElemOp[Op, Long,   Double, Double](op)
  implicit def MatSclrElmOpLLL[Op <: ScalarOp](implicit op: BinOp[Op, Long,   Long,   Long])   = new MatSclrElemOp[Op, Long,   Long,   Long](op)
  implicit def MatSclrElmOpLIL[Op <: ScalarOp](implicit op: BinOp[Op, Long,   Int,    Long])   = new MatSclrElemOp[Op, Long,   Int,    Long](op)

  implicit def MatSclrElmOpIDD[Op <: ScalarOp](implicit op: BinOp[Op, Int,    Double, Double]) = new MatSclrElemOp[Op, Int,    Double, Double](op)
  implicit def MatSclrElmOpILL[Op <: ScalarOp](implicit op: BinOp[Op, Int,    Long,   Long])   = new MatSclrElemOp[Op, Int,    Long,   Long](op)
  implicit def MatSclrElmOpIII[Op <: ScalarOp](implicit op: BinOp[Op, Int,    Int,    Int])    = new MatSclrElemOp[Op, Int,    Int,    Int](op)

  // ***************

  // Binary element-wise operation on two Mats                                                              scala
  final class MatMatElemOp[OP <: ScalarOp, @spec(Int, Long, Double) A, @spec(Int, Long, Double) B, @spec(Int, Long, Double) C: ST](
    op: BinOp[OP, A, B, C]) extends BinOp[OP, Mat[A], Mat[B], Mat[C]] {

    def apply(v1: Mat[A], v2: Mat[B]) = {
      require(v1.numRows == v2.numRows && v1.numCols == v2.numCols, "Mats must have the same size!")
      val sz = v1.length
      val ar = new Array[C](sz)
      var i = 0
      while (i < sz) {
        ar(i) = op(v1.raw(i), v2.raw(i))
        i += 1
      }
      Mat(v1.numRows, v1.numCols, ar)
    }
  }

  // concrete implementations
  implicit def MatMatElemOpDDD[Op <: ScalarOp](implicit op: BinOp[Op, Double, Double, Double]) = new MatMatElemOp[Op, Double, Double, Double](op)
  implicit def MatMatElemOpDLD[Op <: ScalarOp](implicit op: BinOp[Op, Double, Long,   Double]) = new MatMatElemOp[Op, Double, Long,   Double](op)
  implicit def MatMatElemOpDID[Op <: ScalarOp](implicit op: BinOp[Op, Double, Int,    Double]) = new MatMatElemOp[Op, Double, Int,    Double](op)

  implicit def MatMatElemOpLDD[Op <: ScalarOp](implicit op: BinOp[Op, Long,   Double, Double]) = new MatMatElemOp[Op, Long,   Double, Double](op)
  implicit def MatMatElemOpLLL[Op <: ScalarOp](implicit op: BinOp[Op, Long,   Long,   Long])   = new MatMatElemOp[Op, Long,   Long,   Long](op)
  implicit def MatMatElemOpLIL[Op <: ScalarOp](implicit op: BinOp[Op, Long,   Int,    Long])   = new MatMatElemOp[Op, Long,   Int,    Long](op)

  implicit def MatMatElemOpIDD[Op <: ScalarOp](implicit op: BinOp[Op, Int,    Double, Double]) = new MatMatElemOp[Op, Int,    Double, Double](op)
  implicit def MatMatElemOpILL[Op <: ScalarOp](implicit op: BinOp[Op, Int,    Long,   Long])   = new MatMatElemOp[Op, Int,    Long,   Long](op)
  implicit def MatMatElemOpIII[Op <: ScalarOp](implicit op: BinOp[Op, Int,    Int,    Int])    = new MatMatElemOp[Op, Int,    Int,    Int](op)

  // ***************

  // Binary linear alg operation on two Mats (inner product)

  // Binary op: matrix/vector multiplication
  implicit def matmulOpWithVector[A, B, OP <: InnerProd](
    implicit cb: ST[B], na: NUM[A], nb: NUM[B]) =
    new BinOp[InnerProd, Mat[A], Vec[B], Mat[Double]] {
      def apply(m1: Mat[A], m2: Vec[B]): Mat[Double] = {
        m1.mult(Mat(m2))
      }
    }

  // Binary op: vector/matrix multiplication
  implicit def vecmulOpWithMatrix[A, B, OP <: InnerProd](
    implicit cb: ST[A], na: NUM[A], nb: NUM[B]) =
    new BinOp[InnerProd, Vec[A], Mat[B], Mat[Double]] {
      def apply(m1: Vec[A], m2: Mat[B]): Mat[Double] = {
        Mat(m1).transpose.mult(m2)
      }
    }

  // Binary op: matrix/matrix multiplication
  implicit def matmulOpWithMatrix[A, B, OP <: InnerProd](
    implicit cb: ST[B], na: NUM[A], nb: NUM[B]) =
    new BinOp[InnerProd, Mat[A], Mat[B], Mat[Double]] {
      def apply(m1: Mat[A], m2: Mat[B]): Mat[Double] = {
        m1.mult(m2)
      }
    }
}