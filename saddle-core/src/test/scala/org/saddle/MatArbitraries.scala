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

import org.scalacheck.Gen

object MatArbitraries {

  // Generates Mat instance up to 10x10 with entries between -1e3/+1e3 and no NAs
  def matDouble: Gen[Mat[Double]] =
    for {
      r <- Gen.choose(0, 10)
      c <- Gen.choose(0, 10)
      lst <- Gen.listOfN(r * c, Gen.chooseNum(-1e3, 1e3))
    } yield Mat(r, c, lst.toArray)

  // Same, but with 10% NAs
  def matDoubleWithNA: Gen[Mat[Double]] =
    for {
      r <- Gen.choose(0, 10)
      c <- Gen.choose(0, 10)
      lst <- Gen.listOfN(
        r * c,
        Gen.frequency((9, Gen.chooseNum(-1e3, 1e3)), (1, na.to[Double]))
      )
    } yield Mat(r, c, lst.toArray)
}
