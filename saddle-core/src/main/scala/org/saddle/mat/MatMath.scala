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
package org.saddle.mat

import scala.{specialized => spec}
import org.saddle.{Mat, ST}

/**
  * Matrix mathematical helper routines.
  */
object MatMath {

  /**
    * Yields covariance matrix from input matrix whose columns are variable observations
    *
    * @param mat Input matrix of observations, with a variable per column
    * @param corr If true, return correlation rather than covariance
    */
  def cov(mat: Mat[Double], corr: Boolean = false): Mat[Double] = {
    // we do cov calc on columns; but as rows for efficiency
    val numCols = mat.numCols
    val numRows = mat.numRows

    if (numRows < 2 || numCols < 2)
      throw new IllegalArgumentException(
        "Matrix dimension must be at least [2 x 2]"
      )

    val input = mat.transpose.toArray.clone()
    val output = Array.ofDim[Double](numCols * numCols)

    // demean columns (in-place)
    demean(input, numCols, numRows)

    // compute pairwise moments
    var i = 0
    while (i < numCols) {
      val ri0 = i * numRows

      var j = 0
      while (j <= i) {
        val rj0 = j * numRows

        val tmp =
          if (corr && i == j)
            1.0
          else
            covariance(input, ri0, rj0, numRows, corr = corr)

        output(j * numCols + i) = tmp
        output(i * numCols + j) = tmp

        j += 1
      }
      i += 1
    }

    Mat[Double](numCols, numCols, output)
  }

  /**
    * Return a matrix whose rows are demeaned
    * @param mat The matrix to demean
    */
  def demeaned(mat: Mat[Double]): Mat[Double] = {
    val data: Array[Double] = mat.contents
    demean(data, mat.numRows, mat.numCols)
    Mat(mat.numRows, mat.numCols, data)
  }

  // demeans matrix columns, helper function to cov()
  private def demean(m: Array[Double], rows: Int, cols: Int): Unit = {
    // for each row
    var i = 0
    while (i < rows) {
      var j = 0
      // calculate the (na-friendly) mean
      var mean = 0.0
      var count = 0
      while (j < cols) {
        val idx = i * cols + j
        val mval = m(idx)
        if (!mval.isNaN) {
          mean += mval
          count += 1
        }
        j += 1
      }
      mean /= count
      // subtract mean from row
      j = 0
      while (j < cols) {
        val idx = i * cols + j
        m(idx) -= mean
        j += 1
      }
      i += 1
    }
  }

  // parameters:
  // values : one-d array of matrix values
  // ixA    : starting index of vector a,
  // ixB    : starting index of vector b
  // n      : length of vector
  // corr   : do correlation computation
  private def covariance(
      values: Array[Double],
      ixA: Int,
      ixB: Int,
      n: Int,
      corr: Boolean
  ): Double = {
    var va = 0.0
    var vb = 0.0

    var aa = 0.0 // sum of squares
    var bb = 0.0
    var ab = 0.0 // sum of products
    var i = 0

    var count = n
    while (i < n) {
      va = values(ixA + i)
      vb = values(ixB + i)
      if (va != va || vb != vb) {
        count -= 1
      } else {
        if (corr) {
          aa += va * va
          bb += vb * vb
        }
        ab += va * vb
      }
      i += 1
    }
    if (corr) // corr or cov?
      ab / math.sqrt(aa * bb)
    else
      ab / (count - 1)
  }

  /** Efficient block-based non-square matrix transpose that is sensitive to cache line
    * effects (destructive to out matrix)
    */
  private[saddle] def blockTranspose[@spec(Int, Long, Double) S](
      inR: Int,
      inC: Int,
      in: Array[S],
      out: Array[S]
  ) = {
    val XOVER = 60

    var r = 0
    val rsz = inR
    val csz = inC
    while (r < rsz) {
      val blockHeight = if (XOVER < rsz - r) XOVER else rsz - r
      var inRow = r * csz // first element of current row
      var outCol = r // first element of current col
      var c = 0
      while (c < csz) {
        val blockWidth = if (XOVER < csz - c) XOVER else csz - c
        val rowEnd = inRow + blockWidth
        while (inRow < rowEnd) {
          var rowSrc = inRow
          var colDst = outCol
          val colEnd = colDst + blockHeight
          while (colDst < colEnd) {
            out(colDst) = in(rowSrc)
            colDst += 1
            rowSrc += csz
          }
          outCol += rsz
          inRow += 1
        }
        c += XOVER
      }
      r += XOVER
    }
  }

  /** Efficient square matrix transpose (destructive)
    */
  private[saddle] def squareTranspose[@spec(Int, Long, Double) S: ST](
      sz: Int,
      out: Array[S]
  ) = {
    val csz = sz
    val rsz = sz

    var i = 0
    var idx1 = 1
    var cols = csz
    while (i < rsz) {
      var idx2 = (i + 1) * csz + i
      while (idx1 < cols) {
        val v = out(idx1)
        out(idx1) = out(idx2)
        out(idx2) = v
        idx1 += 1
        idx2 += csz
      }
      i += 1
      idx1 += (i + 1)
      cols += csz
    }
  }
}
