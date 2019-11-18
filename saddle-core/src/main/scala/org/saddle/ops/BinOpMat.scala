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

import org.saddle.{ST, Mat}

/**
  * Provides concrete implementations of binary operations for [[org.saddle.Mat]]; these
  * instances provide implicit support for [[org.saddle.ops.NumericOps]] methods which are
  * inherited by Mat.
  */
trait BinOpMat {
  // ***************

  // Binary element-wise operation on one Mat and one scalar
  final class MatSclrElemOp[
      OP <: ScalarOp,
      @spec(Boolean, Int, Long, Double) A,
      @spec(Boolean, Int, Long, Double) B,
      @spec(Boolean, Int, Long, Double) C: ST
  ](val op: BinOp[OP, A, B, C])
      extends BinOp[OP, Mat[A], B, Mat[C]] {
    def apply(v1: Mat[A], v2: B) = {
      val sz = v1.length
      val ar = new Array[C](sz)
      val v1a = v1.toArray
      var i = 0
      while (i < sz) {
        ar(i) = op(v1a(i), v2)
        i += 1
      }
      Mat(v1.numRows, v1.numCols, ar)
    }
  }

  // concrete implementations
  implicit def MatSclrElmOpDDD[Op <: ScalarOp](
      implicit op: BinOp[Op, Double, Double, Double]
  ) = new MatSclrElemOp[Op, Double, Double, Double](op)
  implicit def MatSclrElmOpDLD[Op <: ScalarOp](
      implicit op: BinOp[Op, Double, Long, Double]
  ) = new MatSclrElemOp[Op, Double, Long, Double](op)
  implicit def MatSclrElmOpDID[Op <: ScalarOp](
      implicit op: BinOp[Op, Double, Int, Double]
  ) = new MatSclrElemOp[Op, Double, Int, Double](op)

  implicit def MatSclrElmOpLDD[Op <: ScalarOp](
      implicit op: BinOp[Op, Long, Double, Double]
  ) = new MatSclrElemOp[Op, Long, Double, Double](op)
  implicit def MatSclrElmOpLLL[Op <: ScalarOp](
      implicit op: BinOp[Op, Long, Long, Long]
  ) = new MatSclrElemOp[Op, Long, Long, Long](op)
  implicit def MatSclrElmOpLIL[Op <: ScalarOp](
      implicit op: BinOp[Op, Long, Int, Long]
  ) = new MatSclrElemOp[Op, Long, Int, Long](op)

  implicit def MatSclrElmOpIDD[Op <: ScalarOp](
      implicit op: BinOp[Op, Int, Double, Double]
  ) = new MatSclrElemOp[Op, Int, Double, Double](op)
  implicit def MatSclrElmOpILL[Op <: ScalarOp](
      implicit op: BinOp[Op, Int, Long, Long]
  ) = new MatSclrElemOp[Op, Int, Long, Long](op)
  implicit def MatSclrElmOpIII[Op <: ScalarOp](
      implicit op: BinOp[Op, Int, Int, Int]
  ) = new MatSclrElemOp[Op, Int, Int, Int](op)

  implicit def MatSclrElmOpBBB[Op <: ScalarOp](
      implicit op: BinOp[Op, Boolean, Boolean, Boolean]
  ) = new MatSclrElemOp[Op, Boolean, Boolean, Boolean](op)

  implicit def MatSclrElmOpDDB[Op <: ScalarOp](
      implicit op: BinOp[Op, Double, Double, Boolean]
  ) = new MatSclrElemOp[Op, Double, Double, Boolean](op)

  implicit def MatSclrElmOpDLB[Op <: ScalarOp](
      implicit op: BinOp[Op, Double, Long, Boolean]
  ) = new MatSclrElemOp[Op, Double, Long, Boolean](op)

  implicit def MatSclrElmOpLDB[Op <: ScalarOp](
      implicit op: BinOp[Op, Long, Double, Boolean]
  ) = new MatSclrElemOp[Op, Long, Double, Boolean](op)

  implicit def MatSclrElmOpIDB[Op <: ScalarOp](
      implicit op: BinOp[Op, Int, Double, Boolean]
  ) = new MatSclrElemOp[Op, Int, Double, Boolean](op)
  implicit def MatSclrElmOpDIB[Op <: ScalarOp](
      implicit op: BinOp[Op, Double, Int, Boolean]
  ) = new MatSclrElemOp[Op, Double, Int, Boolean](op)
  implicit def MatSclrElmOpIIB[Op <: ScalarOp](
      implicit op: BinOp[Op, Int, Int, Boolean]
  ) = new MatSclrElemOp[Op, Int, Int, Boolean](op)
  implicit def MatSclrElmOpLLB[Op <: ScalarOp](
      implicit op: BinOp[Op, Long, Long, Boolean]
  ) = new MatSclrElemOp[Op, Long, Long, Boolean](op)
  implicit def MatSclrElmOpLIB[Op <: ScalarOp](
      implicit op: BinOp[Op, Long, Int, Boolean]
  ) = new MatSclrElemOp[Op, Long, Int, Boolean](op)
  // ***************

  /**Binary element-wise operation on two Mats
    *
    * Broadcasts according to the following rules (same as numpy).
    * If any of the pairs of dimensions are the same, or one of the pair is size 1, then
    * the dimensions are compatible.
    * If the compatible dimension is size 1, then it is virtually expanded to match the size of the
    * corresponding dimension of the other operand.
    */
  final class MatMatElemOp[
      OP <: ScalarOp,
      @spec(Int, Long, Double) A,
      @spec(Int, Long, Double) B,
      @spec(Int, Long, Double) C: ST
  ](op: BinOp[OP, A, B, C])
      extends BinOp[OP, Mat[A], Mat[B], Mat[C]] {

    def apply(v1: Mat[A], v2: Mat[B]) =
      if (v1.numRows == v2.numRows && v1.numCols == v2.numCols) {
        val sz = v1.length
        val ar = new Array[C](sz)
        var i = 0
        val v1a = v1.toArray
        val v2a = v2.toArray
        while (i < sz) {
          ar(i) = op(v1a(i), v2a(i))
          i += 1
        }
        Mat(v1.numRows, v1.numCols, ar)
      } else if ((v1.numRows == v2.numRows || v1.numRows == 1 || v2.numRows == 1) &&
                 (v1.numCols == v2.numCols || v1.numCols == 1 || v2.numCols == 1)) {
        //   Broadcasting
        val nR = math.max(v1.numRows, v2.numRows)
        val nC = math.max(v1.numCols, v2.numCols)
        val nR1 = v1.numRows
        val nR2 = v2.numRows
        val nC1 = v1.numCols
        val nC2 = v2.numCols
        val v1a = v1.toArray
        val v2a = v2.toArray
        val sz = nR * nC
        val ar = new Array[C](sz)
        var i = 0
        while (i < sz) {
          val r = i / nC
          val c = i % nC
          val r1 = if (nR1 == 1) 0 else r
          val r2 = if (nR2 == 1) 0 else r
          val c1 = if (nC1 == 1) 0 else c
          val c2 = if (nC2 == 1) 0 else c
          ar(i) = op(v1a(r1 * nC1 + c1), v2a(r2 * nC2 + c2))
          i += 1
        }
        Mat(nR, nC, ar)
      } else throw new RuntimeException("Mats must have compatible size!")

  }

  // concrete implementations
  implicit def MatMatElemOpDDD[Op <: ScalarOp](
      implicit op: BinOp[Op, Double, Double, Double]
  ) = new MatMatElemOp[Op, Double, Double, Double](op)
  implicit def MatMatElemOpDLD[Op <: ScalarOp](
      implicit op: BinOp[Op, Double, Long, Double]
  ) = new MatMatElemOp[Op, Double, Long, Double](op)
  implicit def MatMatElemOpDID[Op <: ScalarOp](
      implicit op: BinOp[Op, Double, Int, Double]
  ) = new MatMatElemOp[Op, Double, Int, Double](op)

  implicit def MatMatElemOpLDD[Op <: ScalarOp](
      implicit op: BinOp[Op, Long, Double, Double]
  ) = new MatMatElemOp[Op, Long, Double, Double](op)
  implicit def MatMatElemOpLLL[Op <: ScalarOp](
      implicit op: BinOp[Op, Long, Long, Long]
  ) = new MatMatElemOp[Op, Long, Long, Long](op)
  implicit def MatMatElemOpLIL[Op <: ScalarOp](
      implicit op: BinOp[Op, Long, Int, Long]
  ) = new MatMatElemOp[Op, Long, Int, Long](op)

  implicit def MatMatElemOpIDD[Op <: ScalarOp](
      implicit op: BinOp[Op, Int, Double, Double]
  ) = new MatMatElemOp[Op, Int, Double, Double](op)
  implicit def MatMatElemOpILL[Op <: ScalarOp](
      implicit op: BinOp[Op, Int, Long, Long]
  ) = new MatMatElemOp[Op, Int, Long, Long](op)
  implicit def MatMatElemOpIII[Op <: ScalarOp](
      implicit op: BinOp[Op, Int, Int, Int]
  ) = new MatMatElemOp[Op, Int, Int, Int](op)

}

trait BinOpMatInPlace {
  // ***************

  // Binary element-wise operation on one Mat and one scalar
  final class MatSclrElemOpIp[
      OP <: ScalarOp,
      @spec(Int, Long, Double) A,
      @spec(Int, Long, Double) B
  ](val op: BinOp[OP, A, B, A])
      extends BinOpInPlace[OP, Mat[A], B] {
    def apply(v1: Mat[A], v2: B) = {
      val sz = v1.length
      val v1a = v1.toArray
      var i = 0
      while (i < sz) {
        v1a(i) = op(v1a(i), v2)
        i += 1
      }
    }
  }

  // concrete implementations
  implicit def MatSclrElmOpIpDDD[Op <: ScalarOp](
      implicit op: BinOp[Op, Double, Double, Double]
  ) = new MatSclrElemOpIp[Op, Double, Double](op)
  implicit def MatSclrElmOpIpDLD[Op <: ScalarOp](
      implicit op: BinOp[Op, Double, Long, Double]
  ) = new MatSclrElemOpIp[Op, Double, Long](op)
  implicit def MatSclrElmOpIpDID[Op <: ScalarOp](
      implicit op: BinOp[Op, Double, Int, Double]
  ) = new MatSclrElemOpIp[Op, Double, Int](op)

  implicit def MatSclrElmOpIpLLL[Op <: ScalarOp](
      implicit op: BinOp[Op, Long, Long, Long]
  ) = new MatSclrElemOpIp[Op, Long, Long](op)
  implicit def MatSclrElmOpIpLIL[Op <: ScalarOp](
      implicit op: BinOp[Op, Long, Int, Long]
  ) = new MatSclrElemOpIp[Op, Long, Int](op)

  implicit def MatSclrElmOpIpIII[Op <: ScalarOp](
      implicit op: BinOp[Op, Int, Int, Int]
  ) = new MatSclrElemOpIp[Op, Int, Int](op)

  // ***************

  // Binary element-wise in place operation on two Mats                                                              scala
  final class MatMatElemOpIp[
      OP <: ScalarOp,
      @spec(Int, Long, Double) A,
      @spec(Int, Long, Double) B
  ](op: BinOp[OP, A, B, A])
      extends BinOpInPlace[OP, Mat[A], Mat[B]] {

    def apply(v1: Mat[A], v2: Mat[B]) =
      if (v1.numRows == v2.numRows && v1.numCols == v2.numCols) {
        val sz = v1.length
        var i = 0
        val v1a = v1.toArray
        val v2a = v2.toArray
        while (i < sz) {
          v1a(i) = op(v1a(i), v2a(i))
          i += 1
        }
      } else if ((v1.numRows == v2.numRows || v2.numRows == 1) &&
                 (v1.numCols == v2.numCols || v2.numCols == 1)) {
        //   Broadcasting
        val nR2 = v2.numRows
        val nC1 = v1.numCols
        val nC2 = v2.numCols
        val v1a = v1.toArray
        val v2a = v2.toArray
        val sz = v1a.size
        var i = 0
        while (i < sz) {
          val r1 = i / nC1
          val r2 = if (nR2 == 1) 0 else r1
          val c1 = i % nC1
          val c2 = if (nC2 == 1) 0 else c1
          v1a(i) = op(v1a(r1 * nC1 + c1), v2a(r2 * nC2 + c2))
          i += 1
        }
      } else throw new RuntimeException("Incompatible sizes")
  }

  // concrete implementations
  implicit def MatMatElemOpIpDDD[Op <: ScalarOp](
      implicit op: BinOp[Op, Double, Double, Double]
  ) = new MatMatElemOpIp[Op, Double, Double](op)
  implicit def MatMatElemOpIpDLD[Op <: ScalarOp](
      implicit op: BinOp[Op, Double, Long, Double]
  ) = new MatMatElemOpIp[Op, Double, Long](op)
  implicit def MatMatElemOpIpDID[Op <: ScalarOp](
      implicit op: BinOp[Op, Double, Int, Double]
  ) = new MatMatElemOpIp[Op, Double, Int](op)

  implicit def MatMatElemOpIpLLL[Op <: ScalarOp](
      implicit op: BinOp[Op, Long, Long, Long]
  ) = new MatMatElemOpIp[Op, Long, Long](op)
  implicit def MatMatElemOpIpLIL[Op <: ScalarOp](
      implicit op: BinOp[Op, Long, Int, Long]
  ) = new MatMatElemOpIp[Op, Long, Int](op)

  implicit def MatMatElemOpIpIII[Op <: ScalarOp](
      implicit op: BinOp[Op, Int, Int, Int]
  ) = new MatMatElemOpIp[Op, Int, Int](op)

}
