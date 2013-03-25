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

import mat.MatMath
import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import org.scalacheck.{Gen, Arbitrary}
import org.scalacheck.Prop._
import org.saddle._
import org.saddle.array._
import vec.BeCloseToVec

/**
  * Test Mat
  */
class MatCheck extends Specification with ScalaCheck {

   "Double Mat Tests" in {
     implicit val arbMat = Arbitrary(MatArbitraries.matDouble)

     "equality works" in {
       forAll { (m: Mat[Double]) =>
         (m must_== Mat(m.numRows, m.numCols, m.toArray)) and (m must_== m)
       }
     }

     "map works" in {
       forAll { (m: Mat[Double]) =>
         val res = m.map(_ + 1)
         val exp = m.contents.map(_ + 1)
         res.contents must_== exp
       }
     }

     "reshape works" in {
       forAll { (m: Mat[Double]) =>
         val res = m.reshape(m.numCols, m.numRows)
         res.contents must_== m.contents
         res.numCols must_== m.numRows
         res.numRows must_== m.numCols
       }
     }

     "isSquare works" in {
       forAll { (m: Mat[Double]) =>
         m.isSquare must_== (m.numRows == m.numCols)
       }
     }

     "map works" in {
       forAll { (m: Mat[Double]) =>
         val data = m.contents
         m.map(_ + 1.0) must_== Mat(m.numRows, m.numCols, data.map(_ + 1.0))
         m.map(d => 5.0) must_== Mat(m.numRows, m.numCols, (data.map(d => if (d.isNaN) na.to[Double] else 5.0)))
         m.map(d => 5) must_== Mat[Int](m.numRows, m.numCols, data.map(d => if (d.isNaN) na.to[Int] else 5))
       }
     }

     "foldLeft works" in {
       implicit val arbMat = Arbitrary(MatArbitraries.matDoubleWithNA)

       forAll { (m: Mat[Double]) =>
         val res = m.foldLeft(0)((c: Int, x: Double) => c + { if (x.isNaN) 0 else 1 } )
         val exp = Vec(m.contents).count
         res must_== exp
       }
     }

     "transpose works" in {
       implicit val arbMat = Arbitrary(MatArbitraries.matDoubleWithNA)

       forAll { (m: Mat[Double]) =>
         val res = m.T
         res.numCols must_== m.numRows
         res.numRows must_== m.numCols
         for(i <- Range(0, m.numRows); j <- Range(0, m.numCols)) m.at(i, j) must_== res.at(j, i)
         res.T must_== m
       }
     }


     "takeRows works" in {
       forAll { (m: Mat[Double]) =>
         val idx = Gen.listOfN(3, Gen.choose[Int](0, m.numRows - 1))
         forAll(idx) { i =>
           val loc = Set(i : _*)
           val res = m.takeRows(i.toArray)
           res.numRows must_== loc.size
           val exp = for (j <- 0 until m.numRows if loc.contains(j)) yield m.row(j)
           res must_== Mat(exp : _*).T
         }
       }
     }

     "takeCols works" in {
       forAll { (m: Mat[Double]) =>
         val idx = Gen.listOfN(3, Gen.choose[Int](0, m.numCols - 1))
         forAll(idx) { i =>
           val loc = Set(i : _*)
           val res = m.takeCols(i.toArray)
           res.numCols must_== loc.size
           val exp = for (j <- 0 until m.numCols if loc.contains(j)) yield m.col(j)
           res must_== Mat(exp : _*)
         }
       }
     }

     "withoutRows works" in {
       forAll { (m: Mat[Double]) =>
         val idx = Gen.listOfN(3, Gen.choose[Int](0, m.numRows - 1))
         forAll(idx) { i =>
           val loc = Set(i : _*)
           val res = m.withoutRows(i.toArray)
           res.numRows must_== (m.numRows - loc.size)
           val exp = for (j <- 0 until m.numRows if !loc.contains(j)) yield m.row(j)
           res must_== Mat(exp : _*).T
         }
       }
     }

     "withoutCols works" in {
       forAll { (m: Mat[Double]) =>
         val idx = Gen.listOfN(3, Gen.choose[Int](0, m.numCols - 1))
         forAll(idx) { i =>
           val loc = Set(i : _*)
           val res = m.withoutCols(i.toArray)
           res.numCols must_== (m.numCols - loc.size)
           val exp = for (j <- 0 until m.numCols if !loc.contains(j)) yield m.col(j)
           res must_== Mat(exp : _*)
         }
       }
     }

     "rowsWithNA works (no NA)" in {
       forAll { (m: Mat[Double]) =>
         m.rowsWithNA must_== Set.empty[Double]
       }
     }

     "rowsWithNA works (with NA)" in {
       implicit val arbMat = Arbitrary(MatArbitraries.matDoubleWithNA)
       forAll { (m: Mat[Double]) =>
         val exp = (m.rows() zip Range(0, m.numRows)).flatMap {
           case (a: Vec[_], b: Int) => if (a.hasNA) Some(b) else None
         }
         m.rowsWithNA must_== exp.toSet
       }
     }

     "dropRowsWithNA works" in {
       implicit val arbMat = Arbitrary(MatArbitraries.matDoubleWithNA)
       forAll { (m: Mat[Double]) =>
         m.dropRowsWithNA must_== m.rdropNA.toMat
       }
     }

     "dropColsWithNA works" in {
       implicit val arbMat = Arbitrary(MatArbitraries.matDoubleWithNA)
       forAll { (m: Mat[Double]) =>
         m.dropColsWithNA must_== m.dropNA.toMat
       }
     }

     "cols works" in {
       forAll { (m: Mat[Double]) =>
         val data = m.T.contents
         val exp = for (i <- IndexedSeq(Range(0, m.numCols) : _*))
                   yield Vec(data).slice(i * m.numRows, (i + 1) * m.numRows)
         m.cols() must_== exp
       }
     }

     "rows works" in {
       forAll { (m: Mat[Double]) =>
         val data = m.contents
         val exp = for (i <- IndexedSeq(Range(0, m.numRows) : _*))
         yield Vec(data).slice(i * m.numCols, (i + 1) * m.numCols)
         m.rows() must_== exp
       }
     }

     "col works" in {
       forAll { (m: Mat[Double]) =>
         val idx = Gen.choose(0, m.numCols - 1)
         val data = m.T.contents
         forAll(idx) { i =>
           m.col(i) must_== Vec(data).slice(i * m.numRows, (i + 1) * m.numRows)
         }
       }
     }

     "row works" in {
       forAll { (m: Mat[Double]) =>
         val idx = Gen.choose(0, m.numRows - 1)
         val data = m.contents
         forAll(idx) { i =>
           m.row(i) must_== Vec(data).slice(i * m.numCols, (i + 1) * m.numCols)
         }
       }
     }

     "mult works" in {
       import org.apache.commons.math.linear.Array2DRowRealMatrix

       forAll { (ma: Mat[Double], mb: Mat[Double]) =>
         if (ma.numCols != mb.numRows)
           ma.mult(mb) must throwAn[IllegalArgumentException]
         else {
           val res = ma.mult(mb)

           res.numRows must_== ma.numRows
           res.numCols must_== mb.numCols

           if (ma.numRows > 0 && mb.numRows > 0) {
             val matA = new Array2DRowRealMatrix(ma.rows().map(_.toArray).toArray)
             val matB = new Array2DRowRealMatrix(mb.rows().map(_.toArray).toArray)

             val matC = matA.multiply(matB)

             res.contents must_== flatten(matC.getData)
           }
           else {
             res.numRows must_== 0
           }
         }
       }
     }

     "roundTo works" in {
       forAll { (ma: Mat[Double]) =>
         ma.contents.map((v: Double) => math.round(v * 100) / 100d) must_== ma.roundTo(2).contents
       }
     }

     "cov works" in {
       forAll { (ma: Mat[Double]) =>
         import org.apache.commons.math.stat.correlation.Covariance

         if (ma.numRows < 2 || ma.numCols < 2) {
           MatMath.cov(ma) must throwAn[IllegalArgumentException]
         }
         else {
           val aCov = new Covariance(ma.rows().map(_.toArray).toArray)
           val exp  = aCov.getCovarianceMatrix
           val res  = MatMath.cov(ma).contents

           Vec(res) must BeCloseToVec(Vec(flatten(exp.getData)), 1e-9)
         }
       }
     }
   }
 }
