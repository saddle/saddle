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

package org.saddle.scalar

import org.saddle._
import org.saddle.vec.VecString
import org.saddle.mat.MatString

/**
 * String ScalarTag
 */
object ScalarTagString extends ScalarTagAny[String] {
  override def makeVec(arr: Array[String]): Vec[String] = VecString(arr)
  override def makeMat(r: Int, c: Int, arr: Array[String]): Mat[String] = MatString(r, c, arr.toSeq)

  /**
   * Alternative constructor necessary to avoid unboxing & boxing strings for Mat[String]
   * construction via Vec[String] instances.
   */
  override protected def altMatConstructor(nr: Int, nc: Int, arr: Array[Vec[String]])(
    implicit st: ST[String]): Mat[String] = {
    val newVec = VecString.concat(arr)
    val offMat = Mat(nr, nc, newVec.offsets)
    val lenMat = Mat(nr, nc, newVec.lengths)
    new MatString(newVec.data, offMat.T, lenMat.T)
  }

  override def toString = "ScalarTagString"

  override def concat(vecs: IndexedSeq[Vec[String]]): Vec[String] = VecString.concat(vecs)
}