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
import org.saddle.{Vec, Mat}

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

object NetLib {
  lazy val BLAS = com.github.fommil.netlib.BLAS.getInstance

  lazy val LAPACK = com.github.fommil.netlib.LAPACK.getInstance
}
