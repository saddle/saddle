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
package org.saddle.macros

import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.saddle._

/**
  * Test Mat
  */
class BinOpMat2Check extends Specification with ScalaCheck {
  implicit val arbMat = Arbitrary(MatArbitraries.matDouble)
  "scalar operation in place works" in {
    forAll { (m: Mat[Double]) =>
      import org.saddle.ops.BinOps._
      val m2 = m + 2d
      val m1 = m.copy
      val op = BinOps.matSclr_DD_Add
      op(m1, 2d)

      m1 must_== m2
    }
  }
}
