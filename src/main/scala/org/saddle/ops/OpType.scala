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
 * All operations on Saddle objects are defined via instances of subtypes
 * of the trait OpType. The instances further derive from ScalarOp or MatrixOp,
 * depending on whether the second operand is a scalar or higher-dimensional
 * object.
 */
sealed trait OpType

trait ScalarOp extends OpType
trait MatrixOp extends OpType

// basic ops
trait Divide extends ScalarOp
trait Multiply extends ScalarOp
trait Add extends ScalarOp
trait Subtract extends ScalarOp
trait Power extends ScalarOp
trait Mod extends ScalarOp

// bitwise ops
trait BitAnd extends ScalarOp
trait BitOr extends ScalarOp
trait BitXor extends ScalarOp
trait BitShl extends ScalarOp
trait BitShr extends ScalarOp
trait BitUShr extends ScalarOp

// comparison operators
trait GtOp extends ScalarOp
trait GteOp extends ScalarOp
trait LtOp extends ScalarOp
trait LteOp extends ScalarOp
trait NeqOp extends ScalarOp
trait EqOp extends ScalarOp

// boolean ops
trait AndOp extends ScalarOp
trait OrOp extends ScalarOp
trait XorOp extends ScalarOp

// linear algebra
trait InnerProd extends MatrixOp
trait OuterProd extends MatrixOp