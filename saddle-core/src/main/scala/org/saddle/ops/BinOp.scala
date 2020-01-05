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
package org.saddle.ops

import annotation.implicitNotFound

import scala.{specialized => spec}

/**
  * Concrete implementations of BinOp provide primitive-specialized support for performing
  * binary operations on elements of the following objects, as well as the objects themselves:
  *
  *   - [[org.saddle.Vec]]
  *   - [[org.saddle.Series]]
  *   - [[org.saddle.Mat]]
  *   - [[org.saddle.Frame]]
  *
  * Appropriate BinOp instances have to be made available in an implicit context in order for the
  * [[org.saddle.ops.NumericOps]] methods inherited by the structures above to operate in a
  * seamless fashion.
  *
  * For example:
  *
  * {{{
  *   Vec(1,2,3) * Vec(4,5,6) == Vec(4,10,18)
  * }}}
  *
  * The multiplication above relies on two BinOp implementations: the first is BinOp[Multiply, Vec, Vec, Vec],
  * whose implementation in turn relies on BinOp[Multiply, Int, Int, Int].
  */
@implicitNotFound(
  msg =
    "No BinOp ${O} instance available to operate on values of type ${X} and ${Y}"
)
trait BinOp[
    O <: OpType,
    @spec(Boolean, Int, Long, Double) -X,
    @spec(Boolean, Int, Long, Double) -Y,
    @spec(Boolean, Int, Long, Double) +Z
] {
  def apply(a: X, b: Y): Z
}

trait BinOpInPlace[
    O <: OpType,
    @spec(Boolean, Int, Long, Double) -X,
    @spec(Boolean, Int, Long, Double) -Y
] {
  def apply(a: X, b: Y): Unit
}
