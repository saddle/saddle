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

/**
 * The package org.saddle.scalar provides a factory method for generating a scalar tag,
 * which contains important meta-data regarding a Scalar type, and which is often implicitly
 * required when dealing with objects in Saddle.
 */
package object scalar {

  // todo: use typeclass pattern here for compile-time ad hoc polymorphism?

  /**
   * Generate a [[org.saddle.scalar.ScalarTag]] instance corresponding to a Scalar of
   * the same type as the type parameter.
   *
   * @tparam T The type with which to parametrize the ScalarTag
   */
  def getScalarTag[T: CLM]: ScalarTag[T] = {
    val m = implicitly[CLM[T]]
    val p = implicitly[CLM[Product]]

    val cc = classOf[Char]
    val cy = classOf[Byte]
    val cb = classOf[Boolean]
    val cs = classOf[Short]
    val ci = classOf[Int]
    val cf = classOf[Float]
    val cl = classOf[Long]
    val cd = classOf[Double]

    val s = m.erasure match {
      // must pass along T's class tag to scalar or it gets lost
      case c if c == cc => ScalarTagChar
      case c if c == cy => ScalarTagByte
      case c if c == cb => ScalarTagBool
      case c if c == cs => ScalarTagShort
      case c if c == ci => ScalarTagInt
      case c if c == cf => ScalarTagFloat
      case c if c == cl => ScalarTagLong
      case c if c == cd => ScalarTagDouble
      case _ if m <:< p => ScalarTagProduct(m.asInstanceOf[CLM[Product]])
      case _            => ScalarTagAny[T](m)
    }

    s.asInstanceOf[ScalarTag[T]]
  }
}
