/**
  * Copyright (c) 2019 Saddle Development Team
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
package org.saddle.linalg

import org.saddle.{Vec, Mat, array}

class DPotrfException(i: Int) extends Exception(s"""|dpotrf error, info=$i
              |*  INFO    (output) INTEGER
|*          = 0:  successful exit
|*          < 0:  if INFO = -i, the i-th argument had an illegal value
|*          > 0:  if INFO = i, the leading minor of order i is not
|*                positive definite, and the factorization could not be
|*                completed.""".stripMargin)

case class SVDResult(u: Mat[Double], sigma: Vec[Double], vt: Mat[Double])

case class EigenDecompositionNonSymmetric(
    q: Mat[Double],
    lambdaReal: Vec[Double],
    lambdaImag: Vec[Double]
)

case class EigenDecompositionSymmetric(q: Mat[Double], lambdaReal: Vec[Double])

trait OpImpl {

  lazy val BLAS = com.github.fommil.netlib.BLAS.getInstance

  lazy val LAPACK = com.github.fommil.netlib.LAPACK.getInstance

  implicit val ddot =
    new VecBinOp[Vec[Double], Double] {
      def apply(a: Vec[Double], b: Vec[Double]): Double = {
        assert(a.length == b.length)
        assert(a.length > 0)
        BLAS.ddot(a.length, a.toArray, 1, b.toArray, 1)

      }
    }

  implicit val dgemv =
    new MatUnaryOp1Scalar[AxV, Vec[Double], Vec[Double]] {
      def apply(a: Mat[Double], b: Vec[Double]): Vec[Double] = {
        assert(a.numCols == b.length)
        assert(a.numCols > 0)
        assert(a.numRows > 0)
        val result = Array.ofDim[Double](a.numRows)

        BLAS.dgemv(
          "T",
          a.numCols,
          a.numRows,
          1.0,
          a.toArray,
          a.numCols,
          b.toArray,
          1,
          0.0,
          result,
          1
        )
        Vec(result)
      }
    }

  implicit val dgemvT =
    new MatUnaryOp1Scalar[AtxV, Vec[Double], Vec[Double]] {
      def apply(a: Mat[Double], b: Vec[Double]): Vec[Double] = {
        assert(a.numRows == b.length)
        assert(a.numCols > 0)
        assert(a.numRows > 0)
        val result = Array.ofDim[Double](a.numCols)

        BLAS.dgemv(
          "N",
          a.numCols,
          a.numRows,
          1.0,
          a.toArray,
          a.numCols,
          b.toArray,
          1,
          0.0,
          result,
          1
        )
        Vec(result)
      }
    }

  implicit val dgemvTarget =
    new MatUnaryOp1ScalarTarget[AxV, Vec[Double]] {
      def apply(a: Mat[Double], b: Vec[Double], result: Array[Double]): Unit = {
        assert(a.numCols == b.length)
        assert(a.numCols > 0)
        assert(a.numRows > 0)
        assert(result.size == a.numRows)

        BLAS.dgemv(
          "T",
          a.numCols,
          a.numRows,
          1.0,
          a.toArray,
          a.numCols,
          b.toArray,
          1,
          0.0,
          result,
          1
        )
        ()
      }
    }

  implicit val dgemvTTarget =
    new MatUnaryOp1ScalarTarget[AtxV, Vec[Double]] {
      def apply(a: Mat[Double], b: Vec[Double], result: Array[Double]): Unit = {
        assert(a.numRows == b.length)
        assert(a.numCols > 0)
        assert(a.numRows > 0)
        assert(result.size == a.numCols)

        BLAS.dgemv(
          "N",
          a.numCols,
          a.numRows,
          1.0,
          a.toArray,
          a.numCols,
          b.toArray,
          1,
          0.0,
          result,
          1
        )
        ()
      }
    }

  implicit val symmEigenValueTrunc =
    new MatUnaryOp1Scalar[EigValSymTrunc, Int, Vec[Double]] {
      def apply(m: Mat[Double], k: Int): Vec[Double] = {
        assert(m.numRows == m.numCols)
        assert(m.numCols > 0)
        assert(m.numRows > 0)
        val K = math.min(m.numRows, k)
        val a = m.toArray.clone

        val vl = Array.ofDim[Double](1)
        val wr = Array.ofDim[Double](K)

        val workQuery = Array.ofDim[Double](1)
        val info = new org.netlib.util.intW(0)
        val outw = new org.netlib.util.intW(0)
        val ifail = Array.ofDim[Int](K)
        val iwork = Array.ofDim[Int](5 * m.numRows)

        LAPACK.dsyevx(
          "N",
          "I",
          "U",
          m.numRows,
          a,
          m.numRows,
          0d,
          0d,
          1,
          K,
          0d,
          outw,
          wr,
          vl,
          m.numRows,
          workQuery,
          -1,
          iwork,
          ifail,
          info
        )

        val work = Array.ofDim[Double](workQuery(0).toInt)

        LAPACK.dsyevx(
          "N",
          "I",
          "U",
          m.numRows,
          a,
          m.numRows,
          0d,
          0d,
          m.numRows - K + 1,
          m.numRows,
          0d,
          outw,
          wr,
          vl,
          m.numRows,
          work,
          work.size,
          iwork,
          ifail,
          info
        )

        val success = info.`val` == 0

        if (!success) throw new RuntimeException("Eigen decomposition failed")

        val wr2: Vec[Double] = Vec(wr).reversed
        wr2
      }
    }

  implicit val symmEigenTrunc =
    new MatUnaryOp1Scalar[EigSTrunc, Int, EigenDecompositionSymmetric] {
      def apply(m: Mat[Double], k: Int): EigenDecompositionSymmetric = {
        assert(m.numRows == m.numCols)
        assert(m.numCols > 0)
        assert(m.numRows > 0)
        val K = math.min(m.numRows, k)
        val a = m.toArray.clone

        val vl = Array.ofDim[Double](m.numRows * K)
        val wr = Array.ofDim[Double](K)

        val workQuery = Array.ofDim[Double](1)
        val info = new org.netlib.util.intW(0)
        val outw = new org.netlib.util.intW(0)
        val ifail = Array.ofDim[Int](K)
        val iwork = Array.ofDim[Int](5 * m.numRows)

        LAPACK.dsyevx(
          "V",
          "I",
          "U",
          m.numRows,
          a,
          m.numRows,
          0d,
          0d,
          1,
          K,
          0d,
          outw,
          wr,
          vl,
          m.numRows,
          workQuery,
          -1,
          iwork,
          ifail,
          info
        )

        val work = Array.ofDim[Double](workQuery(0).toInt)

        LAPACK.dsyevx(
          "V",
          "I",
          "U",
          m.numRows,
          a,
          m.numRows,
          0d,
          0d,
          m.numRows - K + 1,
          m.numRows,
          0d,
          outw,
          wr,
          vl,
          m.numRows,
          work,
          work.size,
          iwork,
          ifail,
          info
        )

        val success = info.`val` == 0

        if (!success) throw new RuntimeException("Eigen decomposition failed")

        val wr2: Vec[Double] = Vec(wr).reversed
        val vl2 = Mat(K, m.numRows, vl)
          .takeRows((0 until K).reverse.toArray)

        EigenDecompositionSymmetric(vl2.T, wr2)
      }
    }

  implicit val symmEigen =
    new MatUnaryOp[EigS, EigenDecompositionSymmetric] {
      def apply(m: Mat[Double]): EigenDecompositionSymmetric = {
        assert(m.numRows == m.numCols)
        assert(m.numCols > 0)
        assert(m.numRows > 0)
        val a = m.toArray.clone

        val wr = Array.ofDim[Double](m.numRows)

        val workQuery = Array.ofDim[Double](1)
        val info = new org.netlib.util.intW(0)

        LAPACK
          .dsyev("V", "U", m.numRows, a, m.numRows, wr, workQuery, -1, info)

        val work = Array.ofDim[Double](workQuery(0).toInt)

        LAPACK
          .dsyev("V", "U", m.numRows, a, m.numRows, wr, work, work.size, info)

        val success = info.`val` == 0

        if (!success) throw new RuntimeException("Eigen decomposition failed")

        val wr2: Vec[Double] = Vec(wr).reversed
        val vl2 = Mat(m.numRows, m.numRows, a)
          .takeRows((0 until m.numRows).reverse.toArray)

        EigenDecompositionSymmetric(vl2.T, wr2)
      }
    }

  implicit val nonsymmEigen =
    new MatUnaryOp[EigNS, EigenDecompositionNonSymmetric] {
      def apply(m: Mat[Double]): EigenDecompositionNonSymmetric = {
        assert(m.numRows == m.numCols)
        assert(m.numCols > 0)
        assert(m.numRows > 0)
        val a = m.toArray.clone

        val vl = Array.ofDim[Double](m.numRows * m.numRows)
        val wr = Array.ofDim[Double](m.numRows)
        val wi = Array.ofDim[Double](m.numRows)

        val workQuery = Array.ofDim[Double](1)
        val info = new org.netlib.util.intW(0)

        LAPACK.dgeev(
          "V",
          "N",
          m.numRows,
          a,
          m.numRows,
          wr,
          wi,
          vl,
          m.numRows,
          null,
          1,
          workQuery,
          -1,
          info
        )

        val work = Array.ofDim[Double](workQuery(0).toInt)

        LAPACK.dgeev(
          "V",
          "N",
          m.numRows,
          a,
          m.numRows,
          wr,
          wi,
          vl,
          m.numRows,
          null,
          1,
          work,
          work.size,
          info
        )

        val success = info.`val` == 0

        if (!success) throw new RuntimeException("Eigen decomposition failed")

        val reindex = array.argsort(wr).reverse
        val wr2: Vec[Double] = Vec(wr).apply(reindex)
        val wi2: Vec[Double] = Vec(wi).apply(reindex)
        val vl2 = Mat(m.numRows, m.numRows, vl).takeRows(reindex)

        EigenDecompositionNonSymmetric(vl2.T, wr2, wi2)
      }
    }

  // implicit val svdtrunc =
  //   new MatUnaryOp1Scalar[GeneralSVDTrunc, Int, SVDResult] {
  //     def apply(m: Mat[Double], k: Int): SVDResult = {
  //       val K = math.min(k, math.min(m.numRows, m.numCols))
  //
  //       /** Lapack gives us the SVD of the transpose
  //         * t(a) = v t(s) t(u)
  //         *   a  = u s t(v)
  //         */
  //       val cop = m.toArray.clone
  //       val s = Array.ofDim[Double](K)
  //       val u = Array.ofDim[Double](m.numCols * K)
  //       val vt = Array.ofDim[Double](m.numRows * K)
  //       val lworkQuery = Array.ofDim[Double](1)
  //       val info = new org.netlib.util.intW(0)
  //       val ns = new org.netlib.util.intW(0)
  //
  //       // 1. Workspace query
  //       LAPACKE.dgesvdx(
  //         "A", // JOBU,
  //         "A", // JOBVT,
  //         "I", // RANGE
  //         m.numCols, // M,
  //         m.numRows, // N,
  //         cop, // A,
  //         m.numCols, // LDA,
  //         0d, //VL (not referenced)
  //         0d, //VU (not referenced)
  //         1, // IL,
  //         K, // IU
  //         ns, // NS
  //         s, // S,
  //         u, // U,
  //         m.numCols, // LDU,
  //         vt, // VT,
  //         m.numRows, // LDVT,
  //         lworkQuery, // WORK,
  //         -1, // LWORK,
  //         info // INFO
  //       )
  //
  //       val lwork = lworkQuery(0).toInt
  //       val work = Array.ofDim[Double](lwork)
  //
  //       LAPACKE.dgesvdx(
  //         "A", // JOBU,
  //         "A", // JOBVT,
  //         "I", // RANGE
  //         m.numCols, // M,
  //         m.numRows, // N,
  //         cop, // A,
  //         m.numCols, // LDA,
  //         0d, //VL (not referenced)
  //         0d, //VU (not referenced)
  //         1, // IL,
  //         K, // IU
  //         ns, // NS
  //         s, // S,
  //         u, // U,
  //         m.numCols, // LDU,
  //         vt, // VT,
  //         m.numRows, // LDVT,
  //         work, // WORK,
  //         lwork, // LWORK,
  //         info // INFO
  //       )
  //
  //       if (lapackInfoMethod.get(info) == 0) {
  //         val ut: Mat[Double] = Mat(m.numRows, K, vt)
  //         val v: Mat[Double] = Mat(K, m.numCols, u)
  //         val sigma: Vec[Double] = Vec(s: _*)
  //
  //         SVDResult(
  //           u = ut,
  //           sigma = sigma,
  //           vt = v
  //         )
  //       } else throw new RuntimeException("SVD Failed")
  //
  //     }
  //   }

  implicit val svd = new MatUnaryOp[GeneralSVD, SVDResult] {
    def apply(m: Mat[Double]): SVDResult = {
      assert(m.numCols > 0)
      assert(m.numRows > 0)

      /* Lapack gives us the SVD of the transpose
       * t(a) = v t(s) t(u)
       *   a  = u s t(v)
       */
      val cop = m.toArray.clone
      val s = Array.ofDim[Double](math.min(m.numRows, m.numCols))
      val u = Array.ofDim[Double](m.numCols * m.numCols)
      val vt = Array.ofDim[Double](m.numRows * m.numRows)
      val lworkQuery = Array.ofDim[Double](1)
      val info = new org.netlib.util.intW(0)

      // 1. Workspace query
      LAPACK.dgesvd(
        "A", // JOBU,
        "A", // JOBVT,
        m.numCols, // M,
        m.numRows, // N,
        cop, // A,
        m.numCols, // LDA,
        s, // S,
        u, // U,
        m.numCols, // LDU,
        vt, // VT,
        m.numRows, // LDVT,
        lworkQuery, // WORK,
        -1, // LWORK,
        info // INFO
      )

      val lwork = lworkQuery(0).toInt
      val work = Array.ofDim[Double](lwork)

      LAPACK.dgesvd(
        "A", // JOBU,
        "A", // JOBVT,
        m.numCols, // M,
        m.numRows, // N,
        cop, // A,
        m.numCols, // LDA,
        s, // S,
        u, // U,
        m.numCols, // LDU,
        vt, // VT,
        m.numRows, // LDVT,
        work, // WORK,
        lwork, // LWORK,
        info // INFO
      )

      if (info.`val` == 0) {
        val ut: Mat[Double] = Mat(m.numRows, m.numRows, vt)
        val v: Mat[Double] = Mat(m.numCols, m.numCols, u)
        val sigma: Vec[Double] = Vec(s)

        SVDResult(
          u = ut,
          sigma = sigma,
          vt = v
        )
      } else throw new RuntimeException("SVD Failed")

    }
  }

  implicit val invertGeneralLU = new MatUnaryOp[InvertWithLU, Mat[Double]] {
    def apply(m: Mat[Double]): Mat[Double] = {
      assert(m.numCols > 0)
      assert(m.numRows > 0)
      val marray = m.toArray
      val array = marray.clone

      val ipiv = Array.ofDim[Int](math.max(1, math.min(m.numCols, m.numRows)))

      LAPACK.dgetrf(
        m.numCols,
        m.numRows,
        array,
        m.numCols,
        ipiv,
        new org.netlib.util.intW(0)
      )

      val lworkQuery = Array.ofDim[Double](1)

      LAPACK.dgetri(
        m.numCols,
        array,
        m.numCols,
        ipiv,
        lworkQuery,
        -1,
        new org.netlib.util.intW(0)
      )

      val work = Array.ofDim[Double](lworkQuery(0).toInt + 1)
      LAPACK.dgetri(
        m.numCols,
        array,
        m.numCols,
        ipiv,
        work,
        lworkQuery(0).toInt + 1,
        new org.netlib.util.intW(0)
      )

      Mat(m.numCols, m.numCols, array)

    }
  }

  implicit val invertPD =
    new MatUnaryOp[InvertPDCholesky, Option[Mat[Double]]] {
      def apply(m: Mat[Double]): Option[Mat[Double]] = {
        assert(m.numCols > 0)
        assert(m.numRows > 0)
        val marray = m.toArray
        val array = marray.clone
        val info = new org.netlib.util.intW(0)
        val info2 = new org.netlib.util.intW(0)

        LAPACK.dpotrf("L", m.numCols, array, m.numCols, info)
        LAPACK.dpotri("L", m.numCols, array, m.numCols, info2)

        if (info.`val` == 0 && info2.`val` == 0) {

          var i = 0
          var j = 0
          while (i < m.numCols) {
            while (j < i) {
              array(i * m.numCols + j) = array(j * m.numCols + i)
              j += 1
            }
            j = 0
            i += 1
          }

          Some(Mat(m.numCols, m.numCols, array))

        } else if (info.`val` != 0) {
          if (info.`val` > 0) None
          else throw new DPotrfException(info.`val`)
        } else {
          throw new RuntimeException(
            "ERROR in dpotri info=" + info2.`val` + """
                      |lapack says:
                      |      INFO    (output) INTEGER
                    |= 0:  successful exit
                    |< 0:  if INFO = -i, the i-th argument had an illegal
                    |value
                    |> 0:  if INFO = i, the (i,i) element of the factor U
                    |or L is zero, and the inverse could not be computed.""".stripMargin + ", matrix: " + m.toString
          )
        }

      }
    }

  implicit val rowSums =
    new MatUnaryOp[RowSums, Vec[Double]] {
      def apply(x: Mat[Double]): Vec[Double] = {
        assert(x.numCols > 0)
        assert(x.numRows > 0)

        val output = Array.ofDim[Double](x.numRows)
        var i = 0
        var j = 0
        while (i < x.numRows) {
          while (j < x.numCols) {
            val v = x.raw(i, j)
            output(i) += v
            j += 1
          }
          j = 0
          i += 1
        }

        Vec(output)

      }
    }

  implicit val diagAxAt =
    new MatUnaryOp[DiagAxAt, Vec[Double]] {
      def apply(x: Mat[Double]): Vec[Double] = {
        assert(x.numCols > 0)
        assert(x.numRows > 0)

        /* diag(AA') = rowSums(A * A elementwise) */
        val output = Array.ofDim[Double](x.numRows)
        var i = 0
        var j = 0
        while (i < x.numRows) {
          while (j < x.numCols) {
            val v = x.raw(i, j)
            output(i) += v * v
            j += 1
          }
          j = 0
          i += 1
        }

        Vec(output)

      }
    }

  implicit val diagAtxA =
    new MatUnaryOp[DiagAtxA, Vec[Double]] {
      def apply(x: Mat[Double]): Vec[Double] = {
        assert(x.numCols > 0)
        assert(x.numRows > 0)

        /* diag(A'A) = colSums(A * A elementwise) */
        val output = Array.ofDim[Double](x.numCols)
        var i = 0
        var j = 0
        while (j < x.numCols) {
          while (i < x.numRows) {
            val v = x.raw(i, j)
            output(j) += v * v
            i += 1
          }
          i = 0
          j += 1
        }

        Vec(output)

      }
    }

  implicit val colSums =
    new MatUnaryOp[ColSums, Vec[Double]] {
      def apply(x: Mat[Double]): Vec[Double] = {
        assert(x.numCols > 0)
        assert(x.numRows > 0)

        val output = Array.ofDim[Double](x.numCols)
        var i = 0
        var j = 0
        while (j < x.numCols) {
          while (i < x.numRows) {
            val v = x.raw(i, j)
            output(j) += v
            i += 1
          }
          i = 0
          j += 1
        }

        Vec(output)

      }
    }

  implicit val cholesky =
    new MatUnaryOp[Cholesky, Option[Mat[Double]]] {
      def apply(x: Mat[Double]): Option[Mat[Double]] = {
        assert(x.numCols > 0)
        assert(x.numRows > 0)

        val xarray = x.toArray.clone
        val info = new org.netlib.util.intW(0)

        /* Cholesky X = L' x L; lower triangular L is stored in X */
        LAPACK.dpotrf("U", x.numCols, xarray, x.numCols, info)

        if (info.`val` == 0) {

          Some(Mat(x.numRows, x.numCols, xarray))

        } else {
          if (info.`val` > 0) None
          else throw new DPotrfException(info.`val`)
        }

      }
    }

  implicit val forwardSolveForTransposed =
    new MatBinOp[SolveLowerTriangular, Option[Mat[Double]]] {
      def apply(a: Mat[Double], b: Mat[Double]): Option[Mat[Double]] = {
        assert(b.numCols > 0)
        assert(b.numRows > 0)
        assert(a.numCols > 0)
        assert(a.numRows > 0)
        assert(a.numCols == b.numCols)

        val barray = b.toArray.clone
        val aarray = a.toArray
        val info = new org.netlib.util.intW(0)

        /*
         * solve triangular system for X: A x X = B
         * B is transposed implicitly because Mat[_] is row major and lapack is col major
         * A is transposed by lapack
         */
        LAPACK.dtrtrs(
          "U",
          "T",
          "N",
          a.numCols,
          b.numRows,
          aarray,
          b.numCols,
          barray,
          b.numCols,
          info
        )

        if (info.`val` == 0) {

          Some(Mat(b.numRows, b.numCols, barray))

        } else {
          if (info.`val` > 0) None
          else throw new DPotrfException(info.`val`)
        }

      }
    }

  implicit val forwardSolveUpperDiagonalForTransposed =
    new MatBinOp[SolveUpperTriangular, Option[Mat[Double]]] {
      def apply(a: Mat[Double], b: Mat[Double]): Option[Mat[Double]] = {
        assert(b.numCols > 0)
        assert(b.numRows > 0)
        assert(a.numCols > 0)
        assert(a.numRows > 0)
        assert(a.numCols == b.numCols)

        val barray = b.toArray.clone
        val aarray = a.toArray
        val info = new org.netlib.util.intW(0)

        /*
         * solve triangular system for X: A x X = B
         * B is transposed implicitly because Mat[_] is row major and lapack is col major
         * A is transposed by lapack
         */
        LAPACK.dtrtrs(
          "L",
          "T",
          "N",
          a.numCols,
          b.numRows,
          aarray,
          b.numCols,
          barray,
          b.numCols,
          info
        )

        if (info.`val` == 0) {

          Some(Mat(b.numRows, b.numCols, barray))

        } else {
          if (info.`val` > 0) None
          else throw new DPotrfException(info.`val`)
        }

      }
    }

  implicit val solve =
    new MatBinOp[GeneralSolve, Option[Mat[Double]]] {
      def apply(a: Mat[Double], b: Mat[Double]): Option[Mat[Double]] = {
        assert(b.numCols > 0)
        assert(b.numRows > 0)
        assert(a.numCols > 0)
        assert(a.numRows > 0)
        assert(a.numRows == a.numCols)
        assert(a.numCols == b.numRows)

        val barray = b.T.toArray.clone
        val aarray = a.T.toArray.clone
        val ipiv = Array.ofDim[Int](a.numRows)
        val info = new org.netlib.util.intW(0)

        LAPACK.dgesv(
          a.numCols,
          b.numCols,
          aarray,
          a.numRows,
          ipiv,
          barray,
          b.numRows,
          info
        )

        if (info.`val` == 0) {

          Some(Mat(b.numCols, b.numRows, barray).T)

        } else {
          if (info.`val` > 0) None
          else throw new DPotrfException(info.`val`)
        }

      }
    }

  implicit val diagXAInverseXt =
    new MatBinOp[DiagXAInverseXt, Option[Vec[Double]]] {
      def apply(x: Mat[Double], a: Mat[Double]): Option[Vec[Double]] = {
        assert(x.numCols > 0)
        assert(x.numRows > 0)
        assert(a.numCols > 0)
        assert(a.numRows > 0)
        assert(x.numCols == a.numRows)
        assert(a.numRows == a.numCols)

        val xarray = x.toArray.clone
        val aarray = a.toArray.clone
        val info = new org.netlib.util.intW(0)
        val info2 = new org.netlib.util.intW(0)

        /* Cholesky A = U' x U; upper triangular U is stored in A */
        LAPACK.dpotrf("U", a.numCols, aarray, a.numCols, info)

        /*
         * solve triangular system for Z: U' x Z = X'
         * X is transposed implicitly because Mat[_] is row major and lapack is col major
         * U is transposed by lapack
         */
        LAPACK.dtrtrs(
          "U",
          "T",
          "N",
          a.numCols,
          x.numRows,
          aarray,
          a.numCols,
          xarray,
          x.numCols,
          info2
        )

        if (info.`val` == 0 && info2.`val` == 0) {

          /* diag(Z'Z) = colSums(Z * Z elementwise) */
          val output = Array.ofDim[Double](x.numRows)
          var i = 0
          var j = 0
          while (i < x.numRows) {
            while (j < x.numCols) {
              output(i) += xarray(i * x.numCols + j) * xarray(i * x.numCols + j)
              j += 1
            }
            j = 0
            i += 1
          }

          Some(Vec(output))

        } else if (info.`val` != 0) {
          if (info.`val` > 0) None
          else throw new DPotrfException(info.`val`)
        } else {
          throw new RuntimeException(
            "ERROR in dtrtrs info=" + info2.`val` + """
                      | INFO is INTEGER
          |= 0:  successful exit
          |< 0:  if INFO = -i, the i-th argument had an illegal value
          |> 0:  if INFO = i, the i-th diagonal element of A is zero,
                indicating that the matrix is singular and the
                solutions X have not been computed.""".stripMargin + ", matrix: " + x.toString
          )
        }

      }
    }

  implicit val mult1 =
    new MatBinOp[AxB, Mat[Double]] {
      def apply(a: Mat[Double], b: Mat[Double]): Mat[Double] = {
        assert(b.numCols > 0)
        assert(b.numRows > 0)
        assert(a.numRows > 0)
        assert(a.numCols > 0)
        assert(a.numCols == b.numRows)

        val result = Array.ofDim[Double](a.numRows * b.numCols)

        BLAS.dgemm(
          "N",
          "N",
          b.numCols, // M
          a.numRows, // N
          b.numRows, // K
          1.0, // alpha
          b.toArray, // op(a) data
          b.numCols, // lda
          a.toArray, // op(b) data
          b.numRows, //ldb
          0.0, // c
          result, // c data
          b.numCols
        ) // ldc
        Mat(a.numRows, b.numCols, result)
      }
    }

  implicit val mult1c =
    new MatGemmOp[aAxBpbC, Mat[Double]] {
      def apply(
          a: Mat[Double],
          b: Mat[Double],
          c: Mat[Double],
          alpha: Double,
          beta: Double
      ): Mat[Double] = {
        assert(
          a.numCols == b.numRows,
          s"Incorrect dimensions ${a.numCols} ${b.numRows}"
        )
        assert(c.numRows == a.numRows && c.numCols == b.numCols)
        assert(b.numCols > 0)
        assert(b.numRows > 0)
        assert(a.numRows > 0)
        assert(a.numCols > 0)

        val result = c.toArray.clone

        BLAS.dgemm(
          "N",
          "N",
          b.numCols, // M
          a.numRows, // N
          b.numRows, // K
          alpha, // alpha
          b.toArray, // op(a) data
          b.numCols, // lda
          a.toArray, // op(b) data
          b.numRows, //ldb
          beta, // c
          result, // c data
          b.numCols
        ) // ldc
        Mat(a.numRows, b.numCols, result)
      }
    }

  implicit val mult2 =
    new MatBinOp[AtxB, Mat[Double]] {
      def apply(a: Mat[Double], b: Mat[Double]): Mat[Double] = {
        assert(a.numRows == b.numRows)
        assert(b.numCols > 0)
        assert(b.numRows > 0)
        assert(a.numRows > 0)
        assert(a.numCols > 0)

        val result = Array.ofDim[Double](a.numCols * b.numCols)

        BLAS.dgemm(
          "N", // op a
          "T", // op b
          b.numCols, // M rows of op(a)
          a.numCols, // N cols of op(b)
          b.numRows, // K cols of op(a)
          1.0, // alpha
          b.toArray, // op(a) data
          b.numCols, // lda
          a.toArray, // op(b) data
          a.numCols, //ldb
          0.0, // c
          result, // c data
          b.numCols
        ) // ldc
        Mat(a.numCols, b.numCols, result)
      }
    }

  implicit val mult2c =
    new MatGemmOp[aAtxBpbC, Mat[Double]] {
      def apply(
          a: Mat[Double],
          b: Mat[Double],
          c: Mat[Double],
          alpha: Double,
          beta: Double
      ): Mat[Double] = {
        assert(a.numRows == b.numRows)
        assert(c.numRows == a.numCols && c.numCols == b.numCols)
        assert(b.numCols > 0)
        assert(b.numRows > 0)
        assert(a.numRows > 0)
        assert(a.numCols > 0)

        val result = c.toArray.clone

        BLAS.dgemm(
          "N", // op a
          "T", // op b
          b.numCols, // M rows of op(a)
          a.numCols, // N cols of op(b)
          b.numRows, // K cols of op(a)
          alpha, // alpha
          b.toArray, // op(a) data
          b.numCols, // lda
          a.toArray, // op(b) data
          a.numCols, //ldb
          beta, // c
          result, // c data
          b.numCols
        ) // ldc
        Mat(a.numCols, b.numCols, result)
      }
    }

  implicit val mult2self =
    new MatUnaryOp[AtxA, Mat[Double]] {
      def apply(a: Mat[Double]): Mat[Double] = {

        val result = Array.ofDim[Double](a.numCols * a.numCols)
        assert(a.numRows > 0)
        assert(a.numCols > 0)

        BLAS.dgemm(
          "N", // op a
          "T", // op b
          a.numCols, // M rows of op(a)
          a.numCols, // N cols of op(b)
          a.numRows, // K cols of op(a)
          1.0, // alpha
          a.toArray, // op(a) data
          a.numCols, // lda
          a.toArray, // op(b) data
          a.numCols, //ldb
          0.0, // c
          result, // c data
          a.numCols
        ) // ldc
        Mat(a.numCols, a.numCols, result)
      }
    }

  implicit val mult2cself =
    new MatGemmSelfOp[aAtxApbC, Mat[Double]] {
      def apply(
          a: Mat[Double],
          c: Mat[Double],
          alpha: Double,
          beta: Double
      ): Mat[Double] = {
        assert(c.numRows == a.numCols && c.numCols == a.numCols)

        assert(c.numCols > 0)
        assert(c.numRows > 0)
        assert(a.numRows > 0)
        assert(a.numCols > 0)

        val result = c.toArray.clone

        BLAS.dgemm(
          "N", // op a
          "T", // op b
          a.numCols, // M rows of op(a)
          a.numCols, // N cols of op(b)
          a.numRows, // K cols of op(a)
          alpha, // alpha
          a.toArray, // op(a) data
          a.numCols, // lda
          a.toArray, // op(b) data
          a.numCols, //ldb
          beta, // c
          result, // c data
          a.numCols
        ) // ldc
        Mat(a.numCols, a.numCols, result)
      }
    }

  implicit val mult3 =
    new MatBinOp[AxBt, Mat[Double]] {
      def apply(a: Mat[Double], b: Mat[Double]): Mat[Double] = {
        assert(a.numCols == b.numCols)

        assert(b.numCols > 0)
        assert(b.numRows > 0)
        assert(a.numRows > 0)
        assert(a.numCols > 0)

        val result = Array.ofDim[Double](a.numRows * b.numRows)

        BLAS.dgemm(
          "T",
          "N",
          b.numRows, // M
          a.numRows, // N
          b.numCols, // K
          1.0, // alpha
          b.toArray, // op(a) data
          b.numCols, // lda
          a.toArray, // op(b) data
          b.numCols, //ldb
          0.0, // c
          result, // c data
          b.numRows
        ) // ldc
        Mat(a.numRows, b.numRows, result)
      }
    }

  implicit val mult3self =
    new MatUnaryOp[AxAt, Mat[Double]] {
      def apply(a: Mat[Double]): Mat[Double] = {

        assert(a.numRows > 0)
        assert(a.numCols > 0)

        val result = Array.ofDim[Double](a.numRows * a.numRows)

        BLAS.dgemm(
          "T",
          "N",
          a.numRows, // M
          a.numRows, // N
          a.numCols, // K
          1.0, // alpha
          a.toArray, // op(a) data
          a.numCols, // lda
          a.toArray, // op(b) data
          a.numCols, //ldb
          0.0, // c
          result, // c data
          a.numRows
        ) // ldc
        Mat(a.numRows, a.numRows, result)
      }
    }

  implicit val mult3c =
    new MatGemmOp[aAxBtpbC, Mat[Double]] {
      def apply(
          a: Mat[Double],
          b: Mat[Double],
          c: Mat[Double],
          alpha: Double,
          beta: Double
      ): Mat[Double] = {
        assert(a.numCols == b.numCols)
        assert(c.numRows == a.numRows && c.numCols == b.numRows)
        assert(b.numCols > 0)
        assert(b.numRows > 0)
        assert(a.numRows > 0)
        assert(a.numCols > 0)

        val result = c.toArray.clone

        BLAS.dgemm(
          "T",
          "N",
          b.numRows, // M
          a.numRows, // N
          b.numCols, // K
          alpha, // alpha
          b.toArray, // op(a) data
          b.numCols, // lda
          a.toArray, // op(b) data
          b.numCols, //ldb
          beta, // c
          result, // c data
          b.numRows
        ) // ldc
        Mat(a.numRows, b.numRows, result)
      }
    }

  implicit val mult3selfplus =
    new MatGemmSelfOp[aAxAtpbC, Mat[Double]] {
      def apply(
          a: Mat[Double],
          c: Mat[Double],
          alpha: Double,
          beta: Double
      ): Mat[Double] = {
        assert(c.numRows == a.numRows && c.numCols == a.numRows)

        assert(a.numRows > 0)
        assert(a.numCols > 0)

        val result = c.toArray.clone

        BLAS.dgemm(
          "T",
          "N",
          a.numRows, // M
          a.numRows, // N
          a.numCols, // K
          alpha, // alpha
          a.toArray, // op(a) data
          a.numCols, // lda
          a.toArray, // op(b) data
          a.numCols, //ldb
          beta, // c
          result, // c data
          a.numRows
        ) // ldc
        Mat(a.numRows, a.numRows, result)
      }
    }

  implicit val mult4 =
    new MatBinOp[AtxBt, Mat[Double]] {
      def apply(a: Mat[Double], b: Mat[Double]): Mat[Double] = {
        assert(a.numRows == b.numCols)

        assert(b.numCols > 0)
        assert(b.numRows > 0)
        assert(a.numRows > 0)
        assert(a.numCols > 0)

        val result = Array.ofDim[Double](a.numCols * b.numRows)

        BLAS.dgemm(
          "T", // op a
          "T", // op b
          b.numRows, // M rows of op(a)
          a.numCols, // N cols of op(b)
          b.numCols, // K cols of op(a)
          1.0, // alpha
          b.toArray, // op(a) data
          b.numCols, // lda
          a.toArray, // op(b) data
          a.numCols, //ldb
          0.0, // c
          result, // c data
          b.numRows
        ) // ldc
        Mat(a.numCols, b.numRows, result)
      }
    }

  implicit val mult4c =
    new MatGemmOp[aAtxBtpbC, Mat[Double]] {
      def apply(
          a: Mat[Double],
          b: Mat[Double],
          c: Mat[Double],
          alpha: Double,
          beta: Double
      ): Mat[Double] = {
        assert(a.numRows == b.numCols)
        assert(c.numRows == a.numCols && c.numCols == b.numRows)
        assert(b.numCols > 0)
        assert(b.numRows > 0)
        assert(a.numRows > 0)
        assert(a.numCols > 0)
        val result = c.toArray.clone

        BLAS.dgemm(
          "T", // op a
          "T", // op b
          b.numRows, // M rows of op(a)
          a.numCols, // N cols of op(b)
          b.numCols, // K cols of op(a)
          alpha, // alpha
          b.toArray, // op(a) data
          b.numCols, // lda
          a.toArray, // op(b) data
          a.numCols, //ldb
          beta, // c
          result, // c data
          b.numRows
        ) // ldc
        Mat(a.numCols, b.numRows, result)
      }
    }

  implicit val trace =
    new MatUnaryOp[Trace, Double] {
      def apply(a: Mat[Double]): Double = {
        assert(a.numRows == a.numCols)
        assert(a.numRows > 0)
        var s = 0.0
        var i = 0
        val d = a.toArray
        while (i < a.numRows) {
          s += d(i * a.numRows + i)
          i += 1
        }
        s
      }
    }

  implicit val diag =
    new MatUnaryOp[Diag, Vec[Double]] {
      def apply(a: Mat[Double]): Vec[Double] = {
        val b = Array.ofDim[Double](math.min(a.numRows, a.numCols))
        var i = 0
        val d = a.toArray
        while (i < b.size) {
          b(i) = d(i * a.numRows + i)
          i += 1
        }
        Vec(b)
      }
    }

  implicit val ispd = new MatUnaryOp[TestPD, Boolean] {
    def apply(m: Mat[Double]): Boolean = {
      assert(m.numCols > 0)
      assert(m.numRows > 0)
      val marray = m.toArray
      val array = marray.clone
      val info = new org.netlib.util.intW(0)
      LAPACK.dpotrf("L", m.numCols, array, m.numCols, info)
      info.`val` == 0
    }
  }

  implicit val svdtrunc =
    new MatUnaryOp1Scalar[GeneralSVDTrunc, Int, SVDResult] {
      def apply(m: Mat[Double], k: Int): SVDResult = {
        assert(m.numCols > 0)
        assert(m.numRows > 0)
        val K = math.min(k, math.min(m.numRows, m.numCols))

        if (m.numRows <= m.numCols) {
          val xxt = m.outerM
          val EigenDecompositionSymmetric(u, lambda) = xxt.eigSymm(K)
          val sigma = lambda.map(math.sqrt)
          val sigmainv = sigma.map(x => 1d / x)
          // inv(u) = t(u)
          val utm = u tmm m

          // inv(diag(sigma)) * utm
          val vt = Mat(utm.rows.zip(sigmainv.toSeq).map(x => x._1 * x._2): _*).T
          SVDResult(u, sigma, vt)
        } else {
          val xtx = m.innerM
          val EigenDecompositionSymmetric(v, lambda) = xtx.eigSymm(K)
          val sigma = lambda.map(math.sqrt)
          val sigmainv = sigma.map(x => 1d / x)

          val mv = m mm v
          // mv * inv(diag(sigma))
          val u = Mat(mv.cols.zip(sigmainv.toSeq).map(x => x._1 * x._2): _*)

          SVDResult(u, sigma, v.T)
        }

      }
    }

  implicit val singularValues =
    new MatUnaryOp1Scalar[SingularValues, Int, Vec[Double]] {
      def apply(m: Mat[Double], k: Int): Vec[Double] = {
        assert(m.numCols > 0)
        assert(m.numRows > 0)
        val K = math.min(k, math.min(m.numRows, m.numCols))

        if (m.numRows <= m.numCols) {
          val xxt = m.outerM
          xxt.eigenValuesSymm(K).map(math.sqrt)
        } else {
          val xtx = m.innerM
          xtx.eigenValuesSymm(K).map(math.sqrt)
        }

      }
    }

  /* D * M */
  implicit val multDiagFromLeft =
    new MatUnaryOp1Scalar[DiagxA, Vec[Double], Mat[Double]] {
      def apply(a: Mat[Double], b: Vec[Double]): Mat[Double] = {
        // assert(a.numRows == b.numCols,
        //        s"Incorrect dimensions ${a.numRows} ${b.numCols}")

        val result = Array.ofDim[Double](a.numCols * b.length)

        val I = b.length
        val J = a.numCols
        var i = 0
        var j = 0
        while (i < I) {
          while (j < J) {
            result(i * J + j) = a.raw(i * J + j) * b.raw(i)
            j += 1
          }
          j = 0
          i += 1
        }

        Mat(b.length, a.numCols, result)
      }
    }

  /* M * D */
  implicit val multDiagFromRight =
    new MatUnaryOp1Scalar[AxDiag, Vec[Double], Mat[Double]] {
      def apply(a: Mat[Double], b: Vec[Double]): Mat[Double] = {

        val result = Array.ofDim[Double](a.numRows * b.length)

        val I = a.numRows
        val J = b.length
        var i = 0
        var j = 0
        while (i < I) {
          while (j < J) {
            result(i * J + j) = a.raw(i * a.numCols + j) * b.raw(j)
            j += 1
          }
          j = 0
          i += 1
        }

        Mat(a.numRows, b.length, result)
      }
    }

}
