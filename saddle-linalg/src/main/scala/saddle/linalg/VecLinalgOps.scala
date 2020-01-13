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

import org.saddle.Vec
import NetLib.BLAS

class VecPimp(val self: Vec[Double]) {
  type B = Vec[Double]

  def linalg = this

  def vv(other: Vec[Double]): Double =
    if (self.length > 5000) vv_blas(other) else vv_java(other)

  def vv_blas(other: Vec[Double]): Double = {
    val a = self
    val b = other
    assert(a.length == b.length)
    assert(a.length > 0)
    BLAS.ddot(a.length, a.toArray, 1, b.toArray, 1)

  }

  def vv_java(other: Vec[Double]): Double = {
    var i = 0
    var s = 0d
    val M = self.length

    val m = M % 5
    while (i < m) {
      s += self.raw(i) * other.raw(i)
      i += 1
    }

    while (i < M) {
      val v1 = self.raw(i) * other.raw(i)
      val v2 = self.raw(i + 1) * other.raw(i + 1)
      val v3 = self.raw(i + 2) * other.raw(i + 2)
      val v4 = self.raw(i + 3) * other.raw(i + 3)
      val v5 = self.raw(i + 4) * other.raw(i + 4)

      s += v1 + v2 + v3 + v4 + v5
      i += 5
    }
    s
  }

}
