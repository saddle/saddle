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
  def map[@spec(Boolean, Int, Long, Double) A: ST,
          @spec(Boolean, Int, Long, Double) B: ST](mat: Mat[A])(f: A => B): Mat[B] = {
    val sca = implicitly[ST[A]]
    val scb = implicitly[ST[B]]
    val buf = Array.ofDim[B](mat.length)
    var i = 0
    while(i < mat.length) {
      val v = mat(i)
      if (sca.isMissing(v))
        buf(i) = scb.missing
      else
        buf(i) = f(v)
      i += 1
    }
    Mat[B](mat.numRows, mat.numCols, buf)
  }

  def withoutRows[@spec(Boolean, Int, Long, Double) A: ST](m: Mat[A], locs: Array[Int]): Mat[A] = {
    if (m.length == 0) Mat.empty[A]
    else {
      val locset = locs.toSet
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
      if (nRows == 0)
        Mat.empty[A]
      else
        Mat(nRows, m.numCols, buf)
    }
  }

  def takeRows[@spec(Boolean, Int, Long, Double) A: ST](m: Mat[A], locs: Array[Int]): Mat[A] = {
    if (m.length == 0) Mat.empty[A]
    else {
      val buf = Buffer[A](m.length)
      var r = 0
      while (r < locs.length) {
        val currRow = locs(r)
        var c = 0
        while (c < m.numCols) {
          buf.add(m(currRow, c))
          c += 1
        }
        r += 1
      }
      Mat(r, m.numCols, buf.toArray)
    }
  }
}
