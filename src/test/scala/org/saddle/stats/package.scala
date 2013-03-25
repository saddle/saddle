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

import org.specs2.mutable.Specification

/**
 * Helpful matchers
 */
package object stats extends Specification {
  def areClose(x: Double, y: Double, delta: Double = 1e-9) = {
    def isNaN  = (x.isNaN && y.isNaN) must beTrue
    def isEq   = x must_== y
    def isZero = (x.abs must be<=(1e-15)) and (y.abs must be<=(1e-15))
    def isNear = (x / y) must beCloseTo(1d +/- delta)

    isEq or (isNaN or (isZero or isNear))
  }
}
