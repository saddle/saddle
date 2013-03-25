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
 * Provides type aliases for a few basic operations
 */
package object ops {
  type SubOp[T] = BinOp[Subtract, T, T, T]
  type AddOp[T] = BinOp[Add, T, T, T]
  type MulOp[T] = BinOp[Multiply, T, T, T]
  type DivOp[T] = BinOp[Divide, T, T, T]
}
