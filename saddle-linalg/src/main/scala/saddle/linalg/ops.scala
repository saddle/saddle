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

trait AxB
trait AtxB
trait AxBt
trait AtxBt

trait aAxBpbC
trait aAtxBpbC
trait aAxBtpbC
trait aAtxBtpbC

trait AxAt
trait AtxA
trait aAxAtpbC
trait aAtxApbC

trait AxV // matrix x vector
trait AtxV
trait aAxVpbC
trait aAtxVpbC

trait InvertWithLU
trait InvertPDCholesky

trait GeneralSVD
trait GeneralSVDTrunc

trait SingularValues

trait Trace

trait Diag

trait TestPD

trait EigS
trait EigSTrunc
trait EigNS
trait EigNSTrunc
trait EigValSymTrunc

trait GeneralSolve
trait DiagXAInverseXt

trait DiagxA
trait AxDiag

trait DiagAtxA
trait DiagAxAt

trait Cholesky
trait SolveLowerTriangular
trait SolveUpperTriangular

trait RowSums
trait ColSums

// from lapack:
// inverse of A'A from A (QR) (http://scicomp.stackexchange.com/questions/3188/dealing-with-the-inverse-of-a-positive-definite-symmetric-covariance-matrix)
// eigen (symmetric + nonsymmetric)
