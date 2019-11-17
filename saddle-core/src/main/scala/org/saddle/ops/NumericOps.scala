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

/**
  * NumericOps provides a mix-in trait for mathematical objects, which in Saddle
  * include:
  *
  *  - [[org.saddle.Vec]]
  *  - [[org.saddle.Mat]]
  *  - [[org.saddle.Series]]
  *  - [[org.saddle.Frame]]
  *
  *  The methods of this trait allow certain element-wise operations involving
  *  these objects to be expressed more naturally as mathematical expressions.
  *
  *  For instance:
  *
  *  {{{
  *    Vec(1,2,4) << 1 == Vec(2,4,8)
  *    Vec(1,2,4) + Vec(3,5,7) == Vec(4,7,11)
  *    Vec(1,2,4) dot Vec(3,5,7) == 41
  *  }}}
  *
  * @tparam This The type of the class inheriting the NumericOps trait
  */
trait NumericOps[+This] { repr: This =>

  // (element-wise) math ops

  /**
    * Addition
    * @param other other operand instance
    * @param op implicit evidence for operation between this and other
    * @tparam B type of the other operand
    * @tparam That result type of operation
    */
  def +[B, That](other: B)(implicit op: BinOp[Add, This, B, That]): That =
    op(repr, other)

  /**
    * Subtraction
    * @param other other operand instance
    * @param op implicit evidence for operation between this and other
    * @tparam B type of the other operand
    * @tparam That result type of operation
    */
  def -[B, That](other: B)(implicit op: BinOp[Subtract, This, B, That]): That =
    op(repr, other)

  /**
    * Division
    * @param other other operand instance (divisor)
    * @param op implicit evidence for operation between this and other
    * @tparam B type of the other operand
    * @tparam That result type of operation
    */
  def /[B, That](other: B)(implicit op: BinOp[Divide, This, B, That]): That =
    op(repr, other)

  /**
    * Multiplication
    * @param other other operand instance
    * @param op implicit evidence for operation between this and other
    * @tparam B type of the other operand
    * @tparam That result type of operation
    */
  def *[B, That](other: B)(implicit op: BinOp[Multiply, This, B, That]): That =
    op(repr, other)

  /**
    * Integer modulus of division
    * @param other other operand instance (divisor)
    * @param op implicit evidence for operation between this and other
    * @tparam B type of the other operand
    * @tparam That result type of operation
    */
  def %[B, That](other: B)(implicit op: BinOp[Mod, This, B, That]): That =
    op(repr, other)

  /**
    * Exponentiation
    * @param other other operand instance (exponent)
    * @param op implicit evidence for operation between this and other
    * @tparam B type of the other operand
    * @tparam That result type of operation
    */
  def **[B, That](other: B)(implicit op: BinOp[Power, This, B, That]): That =
    op(repr, other)

  // bit-wise ops

  /**
    * Bit-wise AND
    * @param other other operand instance
    * @param op implicit evidence for operation between this and other
    * @tparam B type of the other operand
    * @tparam That result type of operation
    */
  def &[B, That](other: B)(implicit op: BinOp[BitAnd, This, B, That]): That =
    op(repr, other)

  /**
    * Bit-wise OR
    * @param other other operand instance
    * @param op implicit evidence for operation between this and other
    * @tparam B type of the other operand
    * @tparam That result type of operation
    */
  def |[B, That](other: B)(implicit op: BinOp[BitOr, This, B, That]): That =
    op(repr, other)

  /**
    * Bit-wise EXCLUSIVE OR
    * @param other other operand instance
    * @param op implicit evidence for operation between this and other
    * @tparam B type of the other operand
    * @tparam That result type of operation
    */
  def ^[B, That](other: B)(implicit op: BinOp[BitXor, This, B, That]): That =
    op(repr, other)

  /**
    * Bit-shift left
    * @param other other operand instance
    * @param op implicit evidence for operation between this and other
    * @tparam B type of the other operand
    * @tparam That result type of operation
    */
  def <<[B, That](other: B)(implicit op: BinOp[BitShl, This, B, That]): That =
    op(repr, other)

  /**
    * Bit-shift right (arithmetic)
    * @param other other operand instance
    * @param op implicit evidence for operation between this and other
    * @tparam B type of the other operand
    * @tparam That result type of operation
    */
  def >>[B, That](other: B)(implicit op: BinOp[BitShr, This, B, That]): That =
    op(repr, other)

  /**
    * Bit-shift right (logical)
    * @param other other operand instance
    * @param op implicit evidence for operation between this and other
    * @tparam B type of the other operand
    * @tparam That result type of operation
    */
  def >>>[B, That](other: B)(implicit op: BinOp[BitUShr, This, B, That]): That =
    op(repr, other)

  // boolean-result ops

  /**
    * Less-than comparison operator
    * @param other other operand instance
    * @param op implicit evidence for operation between this and other
    * @tparam B type of the other operand
    * @tparam That result type of operation
    */
  def <[B, That](other: B)(implicit op: BinOp[LtOp, This, B, That]): That =
    op(repr, other)

  /**
    * Less-than-or-equal-to comparison operator
    * @param other other operand instance
    * @param op implicit evidence for operation between this and other
    * @tparam B type of the other operand
    * @tparam That result type of operation
    */
  def <=[B, That](other: B)(implicit op: BinOp[LteOp, This, B, That]): That =
    op(repr, other)

  /**
    * Greater-than comparison operator
    * @param other other operand instance
    * @param op implicit evidence for operation between this and other
    * @tparam B type of the other operand
    * @tparam That result type of operation
    */
  def >[B, That](other: B)(implicit op: BinOp[GtOp, This, B, That]): That =
    op(repr, other)

  /**
    * Greater-than-or-equal-to comparison operator
    * @param other other operand instance
    * @param op implicit evidence for operation between this and other
    * @tparam B type of the other operand
    * @tparam That result type of operation
    */
  def >=[B, That](other: B)(implicit op: BinOp[GteOp, This, B, That]): That =
    op(repr, other)

  /**
    * Element-wise equality operator
    * @param other other operand instance
    * @param op implicit evidence for operation between this and other
    * @tparam B type of the other operand
    * @tparam That result type of operation
    */
  def =?[B, That](other: B)(implicit op: BinOp[EqOp, This, B, That]): That =
    op(repr, other)

  /**
    * Element-wise inequality operator
    * @param other other operand instance
    * @param op implicit evidence for operation between this and other
    * @tparam B type of the other operand
    * @tparam That result type of operation
    */
  def <>[B, That](other: B)(implicit op: BinOp[NeqOp, This, B, That]): That =
    op(repr, other)

  // on boolean inputs

  /**
    * Logical OR
    * @param other other operand instance
    * @param op implicit evidence for operation between this and other
    * @tparam B type of the other operand
    * @tparam That result type of operation
    */
  def ||[B, That](other: B)(implicit op: BinOp[OrOp, This, B, That]): That =
    op(repr, other)

  /**
    * Logical AND
    * @param other other operand instance
    * @param op implicit evidence for operation between this and other
    * @tparam B type of the other operand
    * @tparam That result type of operation
    */
  def &&[B, That](other: B)(implicit op: BinOp[AndOp, This, B, That]): That =
    op(repr, other)

  /**
    * Logical EXCLUSIVE OR
    * @param other other operand instance
    * @param op implicit evidence for operation between this and other
    * @tparam B type of the other operand
    * @tparam That result type of operation
    */
  def xor[B, That](other: B)(implicit op: BinOp[XorOp, This, B, That]): That =
    op(repr, other)

  // linear algebra

  /**
    * Dot (inner) product
    * @param other other operand instance
    * @param op implicit evidence for operation between this and other
    * @tparam B type of the other operand
    * @tparam That result type of operation
    */
  def dot[B, That](other: B)(
      implicit op: BinOp[InnerProd, This, B, That]
  ): That = op(repr, other)

  /**
    * Outer product
    * @param other other operand instance
    * @param op implicit evidence for operation between this and other
    * @tparam B type of the other operand
    * @tparam That result type of operation
    */
  def outer[B, That](other: B)(
      implicit op: BinOp[OuterProd, This, B, That]
  ): That = op(repr, other)

  // In place operations

  def +=[B](
      other: B
  )(implicit op: BinOpInPlace[Add, This, B]): Unit =
    op(this, other)
  def -=[B](
      other: B
  )(implicit op: BinOpInPlace[Subtract, This, B]): Unit =
    op(this, other)
  def *=[B](
      other: B
  )(implicit op: BinOpInPlace[Multiply, This, B]): Unit =
    op(this, other)
  def /=[B](
      other: B
  )(implicit op: BinOpInPlace[Divide, This, B]): Unit =
    op(this, other)
  def %=[B](
      other: B
  )(implicit op: BinOpInPlace[Mod, This, B]): Unit =
    op(this, other)
  def **=[B](
      other: B
  )(implicit op: BinOpInPlace[Power, This, B]): Unit =
    op(this, other)
}
