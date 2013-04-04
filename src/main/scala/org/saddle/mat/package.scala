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

package org.saddle

import scala.{specialized => spec}

/*
 * Some matrix utilities
 */

package object mat {
  /**
   * Generate a uniform random Mat[Double] of a certain size
   * @param rows Number of rows
   * @param cols Number of rows
   */
  def rand(rows: Int, cols: Int): Mat[Double] =
    Mat(rows, cols, array.randDouble(rows * cols))

  /**
   * Generate a uniform random positive Mat[Double] of a certain size
   * @param rows Number of rows
   * @param cols Number of rows
   */
  def randp(rows: Int, cols: Int): Mat[Double] =
    Mat(rows, cols, array.randDoublePos(rows * cols))

  /**
   * Generate a uniform random Mat[Long] of a certain size
   * @param rows Number of rows
   * @param cols Number of rows
   */
  def randl(rows: Int, cols: Int): Mat[Long] =
    Mat(rows, cols, array.randLong(rows * cols))

  /**
   * Generate a uniform random positive Mat[Long] of a certain size
   * @param rows Number of rows
   * @param cols Number of rows
   */
  def randpl(rows: Int, cols: Int): Mat[Long] =
    Mat(rows, cols, array.randLongPos(rows * cols))

  /**
   * Generate a uniform random Mat[Int] of a certain size
   * @param rows Number of rows
   * @param cols Number of rows
   */
  def randI(rows: Int, cols: Int): Mat[Int] =
    Mat(rows, cols, array.randInt(rows * cols))

  /**
   * Generate a uniform random positive Mat[Int] of a certain size
   * @param rows Number of rows
   * @param cols Number of rows
   */
  def randpi(rows: Int, cols: Int): Mat[Int] =
    Mat(rows, cols, array.randIntPos(rows * cols))

  /**
   * Generate a gaussian(0, 1) random Mat[Double] of a certain size
   * @param rows Number of rows
   * @param cols Number of rows
   */
  def randn(rows: Int, cols: Int): Mat[Double] =
    Mat(rows, cols, array.randNormal(rows * cols))

  /**
   * Generate a gaussian(mu, sigma) random Mat[Double] of a certain size
   * @param rows Number of rows
   * @param cols Number of rows
   * @param mu Mean of distribution
   * @param sigma Stdev of distribution
   */
  def randn2(rows: Int, cols: Int, mu: Double, sigma: Double): Mat[Double] =
    Mat(rows, cols, array.randNormal2(rows * cols, mu, sigma))

  def ones(rows: Int, cols: Int): Mat[Double] = {
    val tmp = Array.ofDim[Double](rows * cols)
    array.fill(tmp, 1.0)
    Mat(rows, cols, tmp)
  }

  def zeros(rows: Int, cols: Int): Mat[Double] =
    Mat(rows, cols, array.empty[Double](rows * cols))

  /**
   * Create a square identity matrix of dimension n x n
   * @param n The number of rows/columns of the square matrix
   */
  def ident(n: Int): Mat[Double] = {
    if (n <= 0)
      Mat.empty[Double]
    else {
      val tmp = Array.ofDim[Double](n * n)
      var i = 0
      while (i < n) {
        tmp(n*i + i) = 1
        i += 1
      }
      Mat(n, n, tmp)
    }
  }

  /**
   * Given a vector, create a matrix whose diagonal entries equal the vector, with zeros off-diagonal.
   * @param v The vector of source data
   */
  def diag(v: Vec[Double]): Mat[Double] = {
    val l = v.length
    val d = array.empty[Double](l * l)

    var i = 0
    while (i < l) {
      d(i * l + i) = v.raw(i)
      i += 1
    }

    Mat(l, l, d)
  }

  /**
   * Repeats an array in a particular direction to create a 2D matrix
   *
   * @param v array of values to repeat
   * @param n number of repetitions
   * @param asRows if true, returns row-tiling; default is column-tiling
   * @tparam T type of elements in array
   */
  def repeat[@spec(Boolean, Int, Long, Double) T: ST](
    v: Array[T], n: Int, asRows: Boolean = false): Mat[T] = {

    if (asRows) {
      val tmp = array.flatten(for (i <- 1 to n) yield v)
      Mat(n, v.length, tmp)
    }
    else {
      val tmp = for (i <- array.range(0, n)) yield v
      Mat(tmp)
    }
  }
}
