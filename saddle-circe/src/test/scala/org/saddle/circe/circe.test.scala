/**
  * Copyright (c) 2019 Saddle Development Team
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
  */
package org.saddle.circe
import org.saddle._
import _root_.io.circe.syntax._
import _root_.io.circe._
import org.scalatest.funsuite.AnyFunSuite

class BinarySuite extends AnyFunSuite {
  test("2x3") {
    val frame = Frame(
      Mat(Vec(1d, 2d), Vec(3d, 4d), Vec(5d, 6d)),
      Index("r1", "r2"),
      Index("c1", "c2", "c3")
    )
    assert(
      implicitly[Decoder[Frame[String, String, Double]]]
        .decodeJson(frame.asJson)
        .right
        .get == frame
    )
    assert(
      implicitly[Decoder[Series[String, Double]]]
        .decodeJson(frame.rowAt(0).asJson)
        .right
        .get == frame.rowAt(0)
    )
    assert(
      implicitly[Decoder[Mat[Double]]]
        .decodeJson(frame.toMat.asJson)
        .right
        .get == frame.toMat
    )
  }
}
