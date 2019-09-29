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
import org.specs2.ScalaCheck
import org.scalacheck.{Gen, Arbitrary}
import org.scalacheck.Prop._

class FrameCheck extends Specification with ScalaCheck {

  "Frame Tests" in {
    implicit val frame = Arbitrary(FrameArbitraries.frameDoubleWithNA)

    "frame equality" in {
      forAll { (f: Frame[Int, Int, Double]) =>
        (f must_== f.col(*)) and (f must_== f)
      }
    }

    "squeeze" in {
      forAll { (f: Frame[Int, Int, Double]) =>
        f.squeeze must_== f.toColSeq.filterNot(_._2.toVec.toSeq.forall(_.toScalar.isNA)).toFrame
      }
    }

    "rsqueeze" in {
      forAll { (f: Frame[Int, Int, Double]) =>
        f.rsqueeze must_== f.T.toColSeq.filterNot(_._2.toVec.toSeq.forall(_.toScalar.isNA)).toFrame.T
      }
    }

    "frame sortedRowsBy" in {
      forAll { (f: Frame[Int, Int, Double]) =>
        if (f.numCols > 0) {
          val res = f.sortedRowsBy { x =>
            x.at(0)
          }
          val ord = array.argsort(f.colAt(0).toVec)
          val exp = f.rowAt(ord)
          res must_== exp
        } else
          f must_== Frame.empty[Int, Int, Double]
      }
    }

    "frame colSplitAt works" in {
      forAll { (f: Frame[Int, Int, Double]) =>
        (f.numCols > 0) ==> {
          val idx = Gen.choose(0, f.numCols - 1)
          forAll(idx) { i =>
            val (l, r) = f.colSplitAt(i)
            l.numCols must_== i
            r.numCols must_== f.numCols - i
            (l rconcat r) must_== f
          }
        }
      }
    }

    "frame rowSplitAt works" in {
      forAll { (f: Frame[Int, Int, Double]) =>
        (f.numRows > 0) ==> {
          val idx = Gen.choose(0, f.numRows - 1)
          forAll(idx) { i =>
            val (l, r) = f.rowSplitAt(i)
            l.numRows must_== i
            r.numRows must_== f.numRows - i
            (l concat r) must_== f
          }
        }
      }
    }

    "Stringify works for one col, zero rows" in {
      val f = Frame(Array(Vec.empty[Double]): _*)
      f.toString must throwAn[RuntimeException].not
    }

    "Transpose must work for a string frame" in {
      val f = Frame(Vec("a", "b", "c"), Vec("d", "e", "f"))
      f.T must_== Frame(Vec("a", "d"), Vec("b", "e"), Vec("c", "f"))
    }

  }

}
