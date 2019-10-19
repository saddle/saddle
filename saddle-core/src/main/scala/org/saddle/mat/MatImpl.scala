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
import org.saddle.{ST, Mat}

/**
  * Houses specialized method implementations for code reuse in Mat subclasses
  */
private[saddle] object MatImpl {
  def map[
      @spec(Boolean, Int, Long, Double) A: ST,
      @spec(Boolean, Int, Long, Double) B: ST
  ](mat: Mat[A])(f: A => B): Mat[B] = {
    val sca = implicitly[ST[A]]
    val scb = implicitly[ST[B]]
    val buf = Array.ofDim[B](mat.length)
    var i = 0
    while (i < mat.length) {
      val v = mat.raw(i)
      if (sca.isMissing(v))
        buf(i) = scb.missing
      else
        buf(i) = f(v)
      i += 1
    }
    Mat[B](mat.numRows, mat.numCols, buf)
  }

  def withoutRows[@spec(Boolean, Int, Long, Double) A: ST](
      m: Mat[A],
      locs: Array[Int]
  ): Mat[A] = {
    if (m.length == 0) Mat.empty[A]
    else {
      val N = m.numRows
      val M = m.numCols
      val locset = locs.filter(i => i >= 0 && i < N).toSet
      val nRows = locset.size
      val K = N - nRows
      val buf = new Array[A](M * K)
      var r = 0
      var j = 0
      val src = m.toArray
      while (r < N) {
        if (!locset.contains(r)) {
          Array.copy(src, r * M, buf, j * M, M)
          j += 1
        }
        r += 1
      }

      if (K == 0)
        Mat.empty[A]
      else
        Mat(K, M, buf)
    }
  }

  def takeRows[@spec(Boolean, Int, Long, Double) A: ST](
      m: Mat[A],
      locs: Array[Int]
  ): Mat[A] = {
    if (m.length == 0) Mat.empty[A]
    else {
      val M = m.numCols
      val buf = new Array[A](locs.size * M)
      var r = 0
      val src = m.toArray
      val n = locs.length
      while (r < n) {
        val currRow = locs(r)
        Array.copy(src, currRow * M, buf, r * M, M)
        r += 1
      }
      Mat(r, M, buf)
    }
  }
}
