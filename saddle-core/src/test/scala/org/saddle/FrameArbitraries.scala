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

object FrameArbitraries {

  // Generates frame of size of up to 20x10
  //  with 90% of entries between -1e3/+1e3 and 10% NA

  def frameDoubleWithNA: Gen[Frame[Int, Int, Double]] =
    for {
      n <- Gen.choose(0, 20)
      m <- Gen.choose(0, 10)
      lst <- Gen.listOfN(
        n * m,
        Gen.frequency((9, Gen.chooseNum(-1e3, 1e3)), (1, na.to[Double]))
      )
    } yield Frame(Mat(n, m, lst.toArray))

}
