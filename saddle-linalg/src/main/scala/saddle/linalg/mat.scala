package org.saddle.linalg

import org.saddle._
import annotation.implicitNotFound

class MatPimp(val self: Mat[Double]) extends MatLinalgOps

@implicitNotFound(msg = "${O} not found")
trait MatBinOp[O, Res] {
  def apply(a: Mat[Double], b: Mat[Double]): Res
}

@implicitNotFound(msg = "${O} not found")
trait MatGemmOp[O, Res] {
  def apply(
      a: Mat[Double],
      b: Mat[Double],
      c: Mat[Double],
      alpha: Double,
      beta: Double
  ): Res
}

@implicitNotFound(msg = "${O} not found")
trait MatGemmSelfOp[O, Res] {
  def apply(a: Mat[Double], c: Mat[Double], alpha: Double, beta: Double): Res
}

@implicitNotFound(msg = "${O} not found")
trait MatUnaryOp[O, Res] {
  def apply(a: Mat[Double]): Res
}

@implicitNotFound(msg = "${O} not found")
trait MatUnaryOp1ScalarTarget[O, T] {
  def apply(a: Mat[Double], s: T, t: Array[Double]): Unit
}

@implicitNotFound(msg = "${O} not found")
trait MatUnaryOp1Scalar[O, T, Res] {
  def apply(a: Mat[Double], s: T): Res
}

trait MatLinalgOps {
  val self: Mat[Double]
  type B = Mat[Double]

  def linalg = this

  def invert(implicit op: MatUnaryOp[InvertWithLU, B]): B = op(self)

  def invertPD(
      implicit op: MatUnaryOp[InvertPDCholesky, Option[B]]
  ): Option[B] =
    op(self)

  /* DGEMV */
  def mv(other: Vec[Double])(
      implicit op: MatUnaryOp1Scalar[AxV, Vec[Double], Vec[Double]]
  ): Vec[Double] =
    op(self, other)

  def tmv(other: Vec[Double])(
      implicit op: MatUnaryOp1Scalar[AtxV, Vec[Double], Vec[Double]]
  ): Vec[Double] =
    op(self, other)

  /* DGEMV with preallocated output*/
  def mvW(other: Vec[Double], target: Array[Double])(
      implicit op: MatUnaryOp1ScalarTarget[AxV, Vec[Double]]
  ): Vec[Double] = {
    op(self, other, target)
    Vec(target)
  }

  def tmvW(other: Vec[Double], target: Array[Double])(
      implicit op: MatUnaryOp1ScalarTarget[AtxV, Vec[Double]]
  ): Vec[Double] = {
    op(self, other, target)
    Vec(target)
  }

  /**
    * Simple DGEMM
    */
  /* A x B */
  def mm(other: B)(implicit op: MatBinOp[AxB, B]): B =
    op(self, other)

  /* t(A) x B */
  def tmm(other: B)(implicit op: MatBinOp[AtxB, B]): B =
    op(self, other)

  /* A x t(B) */
  def mmt(other: B)(implicit op: MatBinOp[AxBt, B]): B =
    op(self, other)

  /* t(A) x t(B) */
  def tmmt(other: B)(implicit op: MatBinOp[AtxBt, B]): B =
    op(self, other)

  /**
    * Full DGEMM
    */
  /* alhpa A x B + beta * C */
  def mmc(other: B, c: B, alpha: Double = 1.0, beta: Double = 1.0)(
      implicit op: MatGemmOp[aAxBpbC, B]
  ): B =
    op(self, other, c, alpha, beta)

  /* alhpa t(A) x B + beta * C */
  def tmmc(other: B, c: B, alpha: Double = 1.0, beta: Double = 1.0)(
      implicit op: MatGemmOp[aAtxBpbC, B]
  ): B =
    op(self, other, c, alpha, beta)

  /* alpha A x t(B) + beta * C */
  def mmtc(other: B, c: B, alpha: Double = 1.0, beta: Double = 1.0)(
      implicit op: MatGemmOp[aAxBtpbC, B]
  ): B =
    op(self, other, c, alpha, beta)

  /* alpha t(A) x t(B) + beta * C */
  def tmmtc(other: B, c: B, alpha: Double = 1.0, beta: Double = 1.0)(
      implicit op: MatGemmOp[aAtxBtpbC, B]
  ): B =
    op(self, other, c, alpha, beta)

  /* t(A) x A */
  def innerM(implicit op: MatUnaryOp[AtxA, Mat[Double]]): B = op(self)

  def innerMpC(alpha: Double, beta: Double, c: Mat[Double])(
      implicit op: MatGemmSelfOp[aAtxApbC, Mat[Double]]
  ): B =
    op(self, c, alpha, beta)

  /* A x t(A) */
  def outerM(implicit op: MatUnaryOp[AxAt, Mat[Double]]): B = op(self)

  /* diag( t(A) x A ) */
  def diagInnerM(implicit op: MatUnaryOp[DiagAtxA, Vec[Double]]): B =
    Mat(op(self))

  /* diag( A x t(A) ) */
  def diagOuterM(implicit op: MatUnaryOp[DiagAxAt, Vec[Double]]): B =
    Mat(op(self))

  def colSums(implicit op: MatUnaryOp[ColSums, Vec[Double]]): B = Mat(op(self))
  def rowSums(implicit op: MatUnaryOp[RowSums, Vec[Double]]): B = Mat(op(self))

  def outerMpC(alpha: Double, beta: Double, c: Mat[Double])(
      implicit op: MatGemmSelfOp[aAxAtpbC, Mat[Double]]
  ): B =
    op(self, c, alpha, beta)

  def mDiagFromLeft(
      diag: Vec[Double]
  )(implicit op: MatUnaryOp1Scalar[DiagxA, Vec[Double], B]): B =
    op(self, diag)

  def mDiagFromRight(
      diag: Vec[Double]
  )(implicit op: MatUnaryOp1Scalar[AxDiag, Vec[Double], B]): B =
    op(self, diag)

  def svd(implicit op: MatUnaryOp[GeneralSVD, SVDResult]): SVDResult = op(self)

  def svd(max: Int)(
      implicit op: MatUnaryOp1Scalar[GeneralSVDTrunc, Int, SVDResult]
  ): SVDResult = op(self, max)

  def trace(implicit op: MatUnaryOp[Trace, Double]): Double = op(self)

  def diag(implicit op: MatUnaryOp[Diag, Vec[Double]]): Vec[Double] = op(self)

  def isPositiveDefinite(implicit op: MatUnaryOp[TestPD, Boolean]): Boolean =
    op(self)

  def eigNonSymm(
      implicit op: MatUnaryOp[EigNS, EigenDecompositionNonSymmetric]
  ): EigenDecompositionNonSymmetric =
    op(self)

  def eigSymm(
      implicit op: MatUnaryOp[EigS, EigenDecompositionSymmetric]
  ): EigenDecompositionSymmetric =
    op(self)

  def eigSymm(
      i: Int
  )(implicit op: MatUnaryOp1Scalar[EigSTrunc, Int, EigenDecompositionSymmetric])
      : EigenDecompositionSymmetric =
    op(self, i)

  /* diag(other x inv(self) x t(other)) */
  def diagInverseSandwich(other: Mat[Double])(
      implicit op: MatBinOp[DiagXAInverseXt, Option[Vec[Double]]]
  ): Option[Vec[Double]] =
    op(other, self)

  def singularValues(max: Int)(
      implicit op: MatUnaryOp1Scalar[SingularValues, Int, Vec[Double]]
  ): Vec[Double] = op(self, max)

  def eigenValuesSymm(max: Int)(
      implicit op: MatUnaryOp1Scalar[EigValSymTrunc, Int, Vec[Double]]
  ): Vec[Double] = op(self, max)

  /* Lower triangular Cholesky factor. X = L x L'
   * Leaves upper triangle untouched: does NOT zero out upper triangle!
   */
  def choleskyLower(
      implicit op: MatUnaryOp[Cholesky, Option[Mat[Double]]]
  ): Option[Mat[Double]] = op(self)

  /* Computes the log10(determinant) of a positive definite matrix
   * Computes the Cholesky factors and sums their log
   */
  def determinantPD(
      implicit op: MatUnaryOp[Cholesky, Option[Mat[Double]]]
  ): Option[Double] = {
    op(self).map { mat =>
      mat.diag.map(math.log10).sum * 2
    }
  }

  /* Solves A x t(X) = t(B) for X
   * A is lower triangular
   * Note that the right hand side and X are transposed
   */
  def solveLowerTriangularForTransposed(rightHandSide: Mat[Double])(
      implicit op: MatBinOp[SolveLowerTriangular, Option[Mat[Double]]]
  ): Option[Mat[Double]] =
    op(self, rightHandSide)

  /* Solves A x t(X) = t(B) for X
   * A is upper triangular
   * Note that the right hand side and X are transposed
   */
  def solveUpperTriangularForTransposed(rightHandSide: Mat[Double])(
      implicit op: MatBinOp[SolveUpperTriangular, Option[Mat[Double]]]
  ): Option[Mat[Double]] =
    op(self, rightHandSide)

  /* Solves A x X = B for X
   * A is general
   * This transposes all three matrices to conform to Lapack's packing order.
   */
  def solve(rightHandSide: Mat[Double])(
      implicit op: MatBinOp[GeneralSolve, Option[Mat[Double]]]
  ): Option[Mat[Double]] =
    op(self, rightHandSide)

  def \(rightHandSide: Mat[Double])(
      implicit op: MatBinOp[GeneralSolve, Option[Mat[Double]]]
  ): Option[Mat[Double]] =
    op(self, rightHandSide)

}
