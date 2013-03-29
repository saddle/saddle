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

import scala.Int
import scala.{specialized => spec}
import org.saddle._

/**
 * Houses specialized method implementations for code reuse in Mat subclasses
 */
private[saddle] object MatImpl {
  def map[@spec(Int, Long, Double) A, @spec(Int, Long, Double) B: CLM](mat: Mat[A])(f: A => B): Mat[B] = {
    val buf = Array.ofDim[B](mat.length)
    var i = 0
    while(i < mat.length) {
      val m = mat(i)
      buf(i) = f(m)
      i += 1
    }
    Mat(mat.numRows, mat.numCols, buf)
  }

  def foldLeft[@spec(Int, Long, Double) A, @spec(Int, Long, Double) B](mat: Mat[A])(init: B)(f: (B, A) => B): B = {
    var acc = init
    var i = 0
    while(i < mat.length) {
      val m = mat(i)
      acc = f(acc, m)
      i += 1
    }
    acc
  }

  def withoutRows[@spec(Int, Long, Double) A: CLM](m: Mat[A], locset: Set[Int]): Mat[A] = {
    if (m.length == 0) Mat.empty[A]
    else {
      val buf = Buffer[A](m.length)
      var r = 0
      var nRows = 0
      while (r < m.numRows) {
        if (!locset.contains(r)) {
          nRows += 1
          var c = 0
          while (c < m.numCols) {
            buf.add(m(r, c))
            c += 1
          }
        }
        r += 1
      }
      if (nRows == 0) Mat.empty[A]
      else Mat(nRows, m.numCols, buf)
    }
  }

  def takeRows[@spec(Int, Long, Double) A: CLM](m: Mat[A], locset: Set[Int]): Mat[A] =
    withoutRows(m, Range(0, m.numRows).toSet.diff(locset))
}
