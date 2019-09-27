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

object IndexArbitraries {

  // Generates int index with duplicates

  def indexIntWithDups: Gen[Index[Int]] =
    for {
      l <- Gen.choose(0, 20)
      lst <- Gen.listOfN(l, Gen.chooseNum(0, l))
    } yield lst.toIndex

  def indexIntNoDups: Gen[Index[Int]] =
    for {
      l <- Gen.choose(0, 20)
      lst <- Gen.listOfN(l, Gen.chooseNum(0, l))
    } yield lst.toSet[Int].toSeq.toIndex

  def intPair: Gen[Index[(Int, Int)]] =
    for {
      l <- Gen.choose(0, 20)
      lst <- Gen.listOfN(l, Gen.chooseNum(0, l))
      lst2 <- Gen.listOfN(l, Gen.chooseNum(0, l))
    } yield (lst zip lst2).toIndex

}
