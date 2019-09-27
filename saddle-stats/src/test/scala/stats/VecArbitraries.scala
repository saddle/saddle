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

/**
  * Arbitrary vector generators
  */
object VecArbitraries {

  // Generates vec of length of up to 20 entries
  //  with 90% of entries between -1e3/+1e3 and 10% NA

  def vecDoubleWithNA: Gen[Vec[Double]] =
    for {
      n <- Gen.choose(0, 10)
      lst <- Gen.listOfN(
        n,
        Gen.frequency[Double]((9, Gen.chooseNum(-1e3, 1e3)), (1, na.to[Double]))
      )
    } yield Vec(lst: _*)

  // Generates vec of length of up to 20 entries w/o NA's
  def vecDoubleWithoutNA: Gen[Vec[Double]] =
    for {
      n <- Gen.choose(0, 20)
      lst <- Gen.listOfN(n, Gen.chooseNum(-1e3, 1e3))
    } yield Vec(lst: _*)

  def vecDoublePWithNA: Gen[Vec[Double]] =
    for {
      n <- Gen.choose(0, 20)
      lst <- Gen.listOfN(
        n,
        Gen.frequency((9, Gen.chooseNum(-1e3, 1e3)), (1, na.to[Double]))
      )
    } yield Vec(lst.filter(_ > 0): _*)

  def vecDoublePWithoutNA: Gen[Vec[Double]] =
    for {
      n <- Gen.choose(0, 20)
      lst <- Gen.listOfN(n, Gen.chooseNum(-1e3, 1e3))
    } yield Vec(lst.filter(_ > 0): _*)

  // Arbitrary float vecs

  def vecFloatWithNA: Gen[Vec[Float]] =
    for {
      n <- Gen.choose(0, 20)
      lst <- Gen.listOfN(
        n,
        Gen.frequency[Float]((9, Gen.chooseNum(-1e3f, 1e3f)), (1, na.to[Float]))
      )
    } yield Vec(lst: _*)

  def vecFloatWithoutNA: Gen[Vec[Float]] =
    for {
      n <- Gen.choose(0, 20)
      lst <- Gen.listOfN(n, Gen.chooseNum(-1e3f, 1e3f))
    } yield Vec(lst: _*)

  def vecFloatPWithNA: Gen[Vec[Float]] =
    for {
      n <- Gen.choose(0, 20)
      lst <- Gen.listOfN(
        n,
        Gen.frequency((9, Gen.chooseNum(-1e3f, 1e3f)), (1, na.to[Float]))
      )
    } yield Vec(lst.filter(_ > 0): _*)

  def vecFloatPWithoutNA: Gen[Vec[Float]] =
    for {
      n <- Gen.choose(0, 20)
      lst <- Gen.listOfN(n, Gen.chooseNum(-1e3f, 1e3f))
    } yield Vec(lst.filter(_ > 0): _*)

  // Arbitrary long vecs

  def vecLongWithNA: Gen[Vec[Long]] =
    for {
      n <- Gen.choose(0, 20)
      lst <- Gen.listOfN(
        n,
        Gen.frequency((9, Gen.chooseNum(-1000L, 1000L)), (1, na.to[Long]))
      )
    } yield Vec(lst: _*)

  def vecLongWithoutNA: Gen[Vec[Long]] =
    for {
      n <- Gen.choose(0, 20)
      lst <- Gen.listOfN(n, Gen.chooseNum(-1000L, 1000L))
    } yield Vec(lst: _*)

  def vecLongPWithNA: Gen[Vec[Long]] =
    for {
      n <- Gen.choose(0, 20)
      lst <- Gen.listOfN(
        n,
        Gen.frequency((9, Gen.chooseNum(-1000L, 1000L)), (1, na.to[Long]))
      )
    } yield Vec(lst.filter(_ > 0): _*)

  def vecLongPWithoutNA: Gen[Vec[Long]] =
    for {
      n <- Gen.choose(0, 20)
      lst <- Gen.listOfN(n, Gen.chooseNum(-1000L, 1000L))
    } yield Vec(lst.filter(_ > 0): _*)

  // Arbitrary int vecs

  def vecIntWithNA: Gen[Vec[Int]] =
    for {
      n <- Gen.choose(0, 20)
      lst <- Gen.listOfN(
        n,
        Gen.frequency((9, Gen.chooseNum(-1000, 1000)), (1, na.to[Int]))
      )
    } yield Vec(lst: _*)

  def vecIntWithoutNA: Gen[Vec[Int]] =
    for {
      n <- Gen.choose(0, 20)
      lst <- Gen.listOfN(n, Gen.chooseNum(-1000, 1000))
    } yield Vec(lst: _*)

  def vecIntPWithNA: Gen[Vec[Int]] =
    for {
      n <- Gen.choose(0, 20)
      lst <- Gen.listOfN(
        n,
        Gen.frequency((9, Gen.chooseNum(-1000, 1000)), (1, na.to[Int]))
      )
    } yield Vec(lst.filter(_ > 0): _*)

  def vecIntPWithoutNA: Gen[Vec[Int]] =
    for {
      n <- Gen.choose(0, 20)
      lst <- Gen.listOfN(n, Gen.chooseNum(-1000, 1000))
    } yield Vec(lst.filter(_ > 0): _*)

}
