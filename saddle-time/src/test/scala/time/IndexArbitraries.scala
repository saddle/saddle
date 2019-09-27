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
import org.joda.time._
import org.saddle.time._

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

  val zone = DateTimeZone.forID("America/New_York")

  def getDate: Gen[DateTime] =
    for {
      m <- Gen.choose(1, 12)
      d <- Gen.choose(1, 28)
      y <- Gen.choose(2012, 2013)
    } yield new DateTime(y, m, d, 0, 0, 0, 0, zone)

  def indexTimeWithDups: Gen[Index[DateTime]] =
    for {
      l <- Gen.choose(0, 100)
      lst <- Gen.listOfN(l, getDate)
    } yield lst.toIndex

  def indexTimeNoDups: Gen[Index[DateTime]] =
    for {
      l <- Gen.choose(0, 100)
      lst <- Gen.listOfN(l, getDate)
    } yield lst.toSet[DateTime].toSeq.toIndex
}
