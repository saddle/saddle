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
import scalar.Scalar

class IndexCheck extends Specification with ScalaCheck {
  "Int Index Tests" in {
    implicit val arbIndex = Arbitrary(IndexArbitraries.indexIntWithDups)

    "access works" in {
      forAll { (ix: Index[Int]) =>
        (ix.length > 0) ==> {
          val idx = Gen.choose(0, ix.length - 1)
          forAll(idx) { i =>
            ix.at(i) must_== Scalar(ix.toVec.contents(i))
            ix.raw(i) must_== ix.toVec.contents(i)
          }
        }
      }
    }

    "without works" in {
      forAll { (ix: Index[Int]) =>
        (ix.length > 0) ==> {

          val idx = for {
            n <- Gen.choose(0, ix.length - 1)
            l <- Gen.listOfN(n, Gen.oneOf(ix.toSeq))
          } yield l

          forAll(idx) { idx =>
            val without = ix.without(idx.toArray)
            without must_== Index(
              ix.toSeq.zipWithIndex
                .filterNot(v => idx.contains(v._2))
                .map(_._1): _*
            )
          }
        }
      }
    }

    "reversed works" in {
      forAll { (ix: Index[Int]) =>
        val reversed = ix.reversed
        reversed must_== Index(ix.toSeq.reverse: _*)
      }
    }

    "lsearch and rsearch works" in {
      forAll { (ix1: Index[Int], elem: Int) =>
        val ix = ix1.sorted
        val cl = ix.lsearch(elem)
        val cr = ix.lsearch(elem)

        (cl <= cr) and
          (Index((ix.toSeq.take(cl) :+ elem) ++ ix.toSeq.drop(cl): _*).isMonotonic must_== true) and
          (Index((ix.toSeq.take(cr) :+ elem) ++ ix.toSeq.drop(cr): _*).isMonotonic must_== true)
      }
    }

    "IndexMaker tuple2 works" in {
      val gen = for {
        n <- Gen.choose(0, 100)
        v1 <- Gen.listOfN(n, Gen.choose(0, 1000))
        v2 <- Gen.listOfN(n, Gen.choose(0, 1000))
      } yield (v1, v2)
      forAll(gen) {
        case (v1, v2) =>
          Index.make((v1.toVector, v2.toVector)).toSeq must_== (v1 zip v2)
      }
    }

    "IndexMaker tuple2 works with Vecs" in {
      val gen = for {
        n <- Gen.choose(0, 100)
        v1 <- Gen.listOfN(n, Gen.choose(0, 1000))
        v2 <- Gen.listOfN(n, Gen.choose(0, 1000))
      } yield (v1, v2)
      forAll(gen) {
        case (v1, v2) =>
          Index.make((v1.toVec, v2.toVec)).toSeq must_== (v1 zip v2)
      }
    }

    "hashcode and equality works" in {
      val gen = for {
        n <- Gen.choose(1, 100)
        v1 <- Gen.listOfN(n, Arbitrary.arbitrary[Index[Int]])
      } yield v1.distinct
      forAll(gen) { indices =>
        val hashmap = indices.zipWithIndex.toMap
        indices.zipWithIndex.forall {
          case (ix, idx) =>
            hashmap(ix) == idx
        } must_== true
      }
    }

    "contains works" in {
      forAll { (ix: Index[Int], elem: Int) =>
        ix.contains(elem) must_== ix.toSeq.contains(elem)
      }
    }
    "exists works" in {
      forAll { (ix: Index[Int], elem: Int) =>
        ix.exists(_ == elem) must_== ix.toSeq.find(_ == elem).isDefined
      }
    }
    "last works" in {
      forAll { (ix: Index[Int]) =>
        (ix.last: Option[Int]) must_== (if (ix.toSeq.isEmpty) None
                                        else Some(ix.toSeq.last))
      }
    }
    "first works" in {
      forAll { (ix: Index[Int]) =>
        (ix.first: Option[Int]) must_== (if (ix.toSeq.isEmpty) None
                                         else Some(ix.toSeq.head))
      }
    }

    "split works" in {
      implicit val arb = Arbitrary(IndexArbitraries.intPair)
      forAll { (ix: Index[(Int, Int)]) =>
        ix.split must_== {
          val (v1, v2) = ix.toSeq.unzip
          (Index(v1: _*), Index(v2: _*))
        }
      }
    }
    "dropLevel works" in {
      implicit val arb = Arbitrary(IndexArbitraries.intPair)
      forAll { (ix: Index[(Int, Int)]) =>
        ix.dropLevel must_== ix.toSeq.map(_._1).toIndex
      }
    }

    "stack work" in {
      forAll { (ix: Index[Int], ix2: Index[Int]) =>
        ix.stack(ix2) must_== (
          for (i <- ix.toSeq; j <- ix2.toSeq) yield (i, j)
        ).toIndex
      }
    }

    "key lookup works" in {
      forAll { (ix: Index[Int]) =>
        (ix.length > 0) ==> {
          val idx = Gen.choose(0, ix.length - 1)
          forAll(idx) { i =>
            val v = ix.raw(i)
            ix.apply(v) must_== array.range(0, ix.length).filter(ix.raw(_) == v)
          }
        }
      }
    }

    "key counts work" in {
      forAll { (ix: Index[Int]) =>
        (ix.length > 0) ==> {
          val idx = Gen.choose(0, ix.length - 1)
          forAll(idx) { i =>
            val v = ix.raw(i)
            ix.count(v) must_== array
              .range(0, ix.length)
              .map(l => if (ix.raw(l) == v) 1 else 0)
              .sum
          }
        }
      }
    }

    "index joins work" in {
      forAll { (ix1: Index[Int], ix2: Index[Int]) =>
        val all = Seq(
          index.LeftJoin,
          index.RightJoin,
          index.OuterJoin,
          index.InnerJoin
        ) map { jointype =>
          val res = ix1.join(ix2, how = jointype)

          val exp = res.index.toVec
          val lix = ix1.toVec
          val rix = ix2.toVec
          val lft = res.lTake.map(x => lix.take(x)) getOrElse lix fillNA {
            exp.raw(_)
          }
          val rgt = res.rTake.map(x => rix.take(x)) getOrElse rix fillNA {
            exp.raw(_)
          }

          lft must_== exp
          rgt must_== exp
        }
        all.foldLeft(true)((acc, v) => acc && v.isSuccess)
      }
    }

    "index union works" in {
      implicit val arbIndex = Arbitrary(IndexArbitraries.indexIntNoDups)
      forAll { (ix1: Index[Int], ix2: Index[Int]) =>
        ix1
          .union(ix2)
          .index
          .toSeq
          .toSet must_== { ix1.toSeq ++ ix2.toSeq }.toSet
      }
    }

    "without dups, index union is outer join" in {
      implicit val arbIndex = Arbitrary(IndexArbitraries.indexIntNoDups)

      forAll { (ix1: Index[Int], ix2: Index[Int]) =>
        ix1.join(ix2, how = index.OuterJoin).index.toSeq.toSet must_== {
          ix1.toSeq ++ ix2.toSeq
        }.toSet
      }
    }

    "index intersect works" in {
      implicit val arbIndex = Arbitrary(IndexArbitraries.indexIntNoDups)

      forAll { (ix1: Index[Int], ix2: Index[Int]) =>
        ix1.intersect(ix2).index.toSeq.toSet must_== ix1.toSeq
          .toSet[Int]
          .intersect(ix2.toSeq.toSet[Int])
      }
    }

    "joins preserves index order with dups" in {
      implicit val arbIndex = Arbitrary(IndexArbitraries.indexIntWithDups)

      forAll { (ix1: Index[Int], ix2: Index[Int]) =>
        val ixs1 = ix1.sorted
        val ixs2 = ix2.sorted

        ixs1.join(ixs2, how = index.RightJoin).index.isMonotonic must beTrue
        ixs1.join(ixs2, how = index.LeftJoin).index.isMonotonic must beTrue
        ixs1.join(ixs2, how = index.InnerJoin).index.isMonotonic must beTrue
        ixs1.join(ixs2, how = index.OuterJoin).index.isMonotonic must beTrue
      }
    }

    "joins preserves index order no dups" in {
      implicit val arbIndex = Arbitrary(IndexArbitraries.indexIntNoDups)

      forAll { (ix1: Index[Int], ix2: Index[Int]) =>
        val ixs1 = ix1.sorted
        val ixs2 = ix2.sorted

        ixs1.join(ixs2, how = index.RightJoin).index.isMonotonic must beTrue
        ixs1.join(ixs2, how = index.LeftJoin).index.isMonotonic must beTrue
        ixs1.join(ixs2, how = index.InnerJoin).index.isMonotonic must beTrue
        ixs1.join(ixs2, how = index.OuterJoin).index.isMonotonic must beTrue
      }
    }

  }

}
