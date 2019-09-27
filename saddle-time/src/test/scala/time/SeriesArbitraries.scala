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

object SeriesArbitraries {

  // Generates series of length of up to 20 entries
  //  with 90% of entries between -1e3/+1e3 and 10% NA

  // rename to seriesintdouble...
  def seriesDoubleWithNA: Gen[Series[Int, Double]] =
    for {
      n <- Gen.choose(0, 20)
      lst <- Gen.listOfN(
        n,
        Gen.frequency((9, Gen.chooseNum(-1e3, 1e3)), (1, na.to[Double]))
      )
    } yield Series(Vec(lst: _*))

  // As above, but with arbitrary duplicates in (unsorted) index

  // rename to dupseriesintdouble...
  def dupSeriesDoubleWithNA: Gen[Series[Int, Double]] =
    for {
      n <- Gen.choose(0, 20)
      lst <- Gen.listOfN(
        n,
        Gen.frequency((9, Gen.chooseNum(-1e3, 1e3)), (1, na.to[Double]))
      )
      idx <- Gen.listOfN(n, Gen.choose(0, 5))
    } yield Series(Vec(lst: _*), Index(idx: _*))

  // rename to seriesdatetimedouble...
  def seriesDateTimeDoubleNoDup: Gen[Series[DateTime, Double]] =
    for {
      n <- Gen.choose(0, 40)
      ix <- Gen.listOfN(n, IndexArbitraries.getDate)
      uq = Index(ix.toSet.toSeq: _*)
      lst <- Gen.listOfN(uq.length, Gen.chooseNum(-1e3, 1e3))
    } yield Series(Vec(lst: _*), uq)

  // rename to dupseriesdatetimedouble...
  def seriesDateTimeDoubleWithNA: Gen[Series[DateTime, Double]] =
    for {
      n <- Gen.choose(0, 20)
      lst <- Gen.listOfN(
        n,
        Gen.frequency((9, Gen.chooseNum(-1e3, 1e3)), (1, na.to[Double]))
      )
      ix <- Gen.listOfN(n, IndexArbitraries.getDate)
    } yield Series(Vec(lst: _*), Index(ix: _*))

}
