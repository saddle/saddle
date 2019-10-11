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
    "frame equality - empty frame" in {
      (Frame.empty[Int, Int, Double] must_== Frame.empty[Int, Int, Double]) and
        (Frame.empty[Int, Int, Double] must_== List
          .empty[(Int, Series[Int, Double])]
          .toFrame) and
        (Frame.empty[Int, Int, Double].T must_== Frame
          .empty[Int, Int, Double]
          .toColSeq
          .toFrame
          .T)
    }

    "frame equality - NA" in {
      val f = Frame(0 -> Series[Int, Double](0 -> na.to[Double]))
      val f2 = Frame(0 -> Series[Int, Double](0 -> na.to[Double]))
      f must_== f2
    }

    "numCols and toColSeq" in {
      forAll { (f: Frame[Int, Int, Double]) =>
        f.toColSeq.size must_== f.numCols
      }
    }

    "numRows and toRowSet" in {
      forAll { (f: Frame[Int, Int, Double]) =>
        f.toRowSeq.size must_== f.numRows
      }
    }

    "isEmpty" in {
      forAll { (f: Frame[Int, Int, Double]) =>
        f.isEmpty == (f.numCols == 0 || f.numRows == 0)
      }
    }

    "transpose" in {
      forAll { (f: Frame[Int, Int, Double]) =>
        val tr = f.T
        tr.numCols == f.numRows &&
        tr.numRows == f.numCols &&
        tr.colIx == f.rowIx &&
        tr.rowIx == f.colIx &&
        tr.toMat == f.toMat.T
      }
    }

    "col" in {
      forAll { (f: Frame[Int, Int, Double], cx: Seq[Int]) =>
        f.col(cx: _*) must_== cx
          .flatMap(c => f.toColSeq.find(v => v._1 == c))
          .toFrame
      }
    }
    "row" in {
      forAll { (f: Frame[Int, Int, Double], rx: Seq[Int]) =>
        (f.row(rx: _*).rowIx must_== rx
          .flatMap(c => f.toRowSeq.find(v => v._1 == c))
          .toFrame
          .T
          .rowIx) and
          (f.row(rx: _*).colIx must_== f.colIx)

      }
    }
    "colAt" in {
      forAll { (f: Frame[Int, Int, Double], cx: Seq[Int]) =>
        f.colAt(cx.filter(c => c >= 0 && c < f.numCols): _*) must_== cx
          .flatMap(
            c =>
              if (c >= f.numCols || c < 0) Nil
              else List(f.toColSeq(c))
          )
          .toFrame
      }
    }

    "col" in {
      Frame(1 -> Series(1 -> 1, 2 -> 2), 2 -> Series(1 -> 3, 2 -> 4))
        .col(2, 2) must_==
        Frame(2 -> Series(1 -> 3, 2 -> 4), 2 -> Series(1 -> 3, 2 -> 4))
    }
    "colSliceBy" in {
      Frame(
        1 -> Series(1 -> 1, 2 -> 2),
        2 -> Series(1 -> 3, 2 -> 4),
        3 -> Series(1 -> 5, 2 -> 6)
      ).colSliceBy(1, 2) must_==
        Frame(1 -> Series(1 -> 1, 2 -> 2), 2 -> Series(1 -> 3, 2 -> 4))
    }
    "colAt slice" in {
      Frame(
        1 -> Series(1 -> 1, 2 -> 2),
        2 -> Series(1 -> 3, 2 -> 4),
        3 -> Series(1 -> 5, 2 -> 6)
      ).colAt(0 -> 1) must_==
        Frame(1 -> Series(1 -> 1, 2 -> 2), 2 -> Series(1 -> 3, 2 -> 4))
    }

    "squeeze" in {
      forAll { (f: Frame[Int, Int, Double]) =>
        f.squeeze must_== f.toColSeq
          .filterNot(_._2.toVec.toSeq.forall(_.toScalar.isNA))
          .toFrame
      }
    }

    "rsqueeze" in {
      forAll { (f: Frame[Int, Int, Double]) =>
        f.rsqueeze.toRowSeq must_== f.toRowSeq
          .filterNot(_._2.toVec.toSeq.forall(_.toScalar.isNA))
      }
    }

    "rsqueeze" in {
      val f = Frame(0 -> Series(0 -> Double.NaN))
      f.rsqueeze.toRowSeq must_== f.toRowSeq
        .filterNot(_._2.toVec.toSeq.forall(_.toScalar.isNA))

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

    "sum works" in {
      forAll { (f: Frame[Int, Int, Double]) =>
        f.sum must_== f.reduce(_.toVec.toSeq.filterNot(_.isNaN).sum)
      }
    }
    "prod works" in {
      forAll { (f: Frame[Int, Int, Double]) =>
        f.prod must_== f.reduce(
          _.toVec.toSeq.filterNot(_.isNaN).foldLeft(1d)(_ * _)
        )
      }
    }
    "count works" in {
      forAll { (f: Frame[Int, Int, Double]) =>
        f.count must_== f.reduce(
          _.toVec.count
        )
      }
    }
    "min works" in {
      forAll { (f: Frame[Int, Int, Double]) =>
        val expected = f.reduce(
          _.toVec.toSeq
            .filterNot(_.isNaN)
            .sorted
            .headOption
            .toScalar
            .unbox
        )
        f.min must_== expected
      }
    }
    "max works" in {
      forAll { (f: Frame[Int, Int, Double]) =>
        f.max must_== f.reduce(
          _.toVec.toSeq
            .filterNot(_.isNaN)
            .sorted
            .reverse
            .headOption
            .toScalar
            .unbox
        )
      }
    }

  }

}
