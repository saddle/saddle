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

import org.saddle.Serde._
import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import org.scalacheck.{Gen, Arbitrary}
import org.scalacheck.Prop._
import scalar.Scalar
import org.joda.time._

class IndexCheck extends Specification with ScalaCheck {
  "Int Index Tests" in {
    implicit val arbIndex = Arbitrary(IndexArbitraries.indexIntWithDups)

    "access works" in {
      forAll { (ix: Index[Int]) =>
        val idx = Gen.choose(0, ix.length - 1)
        forAll(idx) { i =>
          ix.at(i) must_== Scalar(ix.toVec.contents(i))
          ix.raw(i) must_== ix.toVec.contents(i)
        }
      }
    }

    "key lookup works" in {
      forAll { (ix: Index[Int]) =>
        val idx = Gen.choose(0, ix.length - 1)
        forAll(idx) { i =>
          val v = ix.raw(i)
          ix.apply(v) must_== array.range(0, ix.length).filter(ix.raw(_) == v)
        }
      }
    }

    "key counts work" in {
      forAll { (ix: Index[Int]) =>
        val idx = Gen.choose(0, ix.length - 1)
        forAll(idx) { i =>
          val v = ix.raw(i)
          ix.count(v) must_== array.range(0, ix.length).map(l => if(ix.raw(l) == v) 1 else 0).sum
        }
      }
    }

    "index joins work" in {
      forAll { (ix1: Index[Int], ix2: Index[Int]) =>
        val all = Seq(index.LeftJoin, index.RightJoin, index.OuterJoin, index.InnerJoin) map { jointype =>
          val res = ix1.join(ix2, how = jointype)

          val exp = res.index.toVec
          val lix = ix1.toVec
          val rix = ix2.toVec
          val lft = res.lTake.map(x => lix.take(x)) getOrElse lix fillNA { exp.raw(_) }
          val rgt = res.rTake.map(x => rix.take(x)) getOrElse rix fillNA { exp.raw(_) }

          lft must_== exp
          rgt must_== exp
        }
        all.foldLeft(true)((acc, v) => acc && v.isSuccess)
      }
    }

    "index union works" in {
      implicit val arbIndex = Arbitrary(IndexArbitraries.indexIntNoDups)
      forAll { (ix1: Index[Int], ix2: Index[Int]) =>
        ix1.union(ix2).index.toSeq.toSet must_== { ix1.toSeq ++ ix2.toSeq }.toSet
      }
    }

    "without dups, index union is outer join" in {
      implicit val arbIndex = Arbitrary(IndexArbitraries.indexIntNoDups)

      forAll { (ix1: Index[Int], ix2: Index[Int]) =>
        ix1.join(ix2, how=index.OuterJoin).index.toSeq.toSet must_== { ix1.toSeq ++ ix2.toSeq }.toSet
      }
    }

    "index intersect works" in {
      implicit val arbIndex = Arbitrary(IndexArbitraries.indexIntNoDups)

      forAll { (ix1: Index[Int], ix2: Index[Int]) =>
        ix1.intersect(ix2).index.toSeq.toSet must_== ix1.toSeq.toSet[Int].intersect(ix2.toSeq.toSet[Int])
      }
    }

    "joins preserves index order with dups" in {
      implicit val arbIndex = Arbitrary(IndexArbitraries.indexIntWithDups)

      forAll { (ix1: Index[Int], ix2: Index[Int]) =>
        val ixs1 = ix1.sorted
        val ixs2 = ix2.sorted

        ixs1.join(ixs2, how=index.RightJoin).index.isMonotonic must beTrue
        ixs1.join(ixs2, how=index.LeftJoin).index.isMonotonic must beTrue
        ixs1.join(ixs2, how=index.InnerJoin).index.isMonotonic must beTrue
        ixs1.join(ixs2, how=index.OuterJoin).index.isMonotonic must beTrue
      }
    }

    "joins preserves index order no dups" in {
      implicit val arbIndex = Arbitrary(IndexArbitraries.indexIntNoDups)

      forAll { (ix1: Index[Int], ix2: Index[Int]) =>
        val ixs1 = ix1.sorted
        val ixs2 = ix2.sorted

        ixs1.join(ixs2, how=index.RightJoin).index.isMonotonic must beTrue
        ixs1.join(ixs2, how=index.LeftJoin).index.isMonotonic must beTrue
        ixs1.join(ixs2, how=index.InnerJoin).index.isMonotonic must beTrue
        ixs1.join(ixs2, how=index.OuterJoin).index.isMonotonic must beTrue
      }
    }

    "serialization works" in  {
      implicit val arbIndex = Arbitrary(IndexArbitraries.indexIntNoDups)

      forAll { (ix1: Index[Int], ix2: Index[Int]) => {
        ix1 must_== serializedCopy(ix1)
        ix2 must_== serializedCopy(ix2)
      }
      }
    }


  }

  "Time Index Tests" in {
    "access works" in {
      implicit val arbIndex = Arbitrary(IndexArbitraries.indexTimeWithDups)
      forAll { (ix: Index[DateTime]) =>
        val idx = Gen.choose(0, ix.length - 1)
        forAll(idx) { i =>
          ix.at(i) must_== Scalar(ix.toVec.contents(i))
          ix.raw(i) must_== ix.toVec.contents(i)
        }
      }
    }

    "key lookup works" in {
      implicit val arbIndex = Arbitrary(IndexArbitraries.indexTimeWithDups)
      forAll { (ix: Index[DateTime]) =>
        val idx = Gen.choose(0, ix.length - 1)
        forAll(idx) { i =>
          val v = ix.raw(i)
          ix.apply(v) must_== array.range(0, ix.length).filter(ix.raw(_) == v)
        }
      }
    }

    "key counts work" in {
      implicit val arbIndex = Arbitrary(IndexArbitraries.indexTimeWithDups)
      forAll { (ix: Index[DateTime]) =>
        val idx = Gen.choose(0, ix.length - 1)
        forAll(idx) { i =>
          val v = ix.raw(i)
          ix.count(v) must_== array.range(0, ix.length).map(l => if(ix.raw(l) == v) 1 else 0).sum
        }
      }
    }

    "index joins work" in {
      implicit val arbIndex = Arbitrary(IndexArbitraries.indexTimeWithDups)

      forAll { (ix1: Index[DateTime], ix2: Index[DateTime]) =>
        val all = Seq(index.LeftJoin, index.RightJoin, index.OuterJoin, index.InnerJoin) map { jointype =>
          val res = ix1.join(ix2, how = jointype)

          val exp = res.index.toVec
          val lix = ix1.toVec
          val rix = ix2.toVec
          val lft = res.lTake.map(x => lix.take(x)) getOrElse lix fillNA { exp.raw(_) }
          val rgt = res.rTake.map(x => rix.take(x)) getOrElse rix fillNA { exp.raw(_) }

          lft must_== exp
          rgt must_== exp
        }
        all.foldLeft(true)((acc, v) => acc && v.isSuccess)
      }
    }

    "index union works" in {
      implicit val arbIndex = Arbitrary(IndexArbitraries.indexTimeNoDups)

      forAll { (ix1: Index[DateTime], ix2: Index[DateTime]) =>
        ix1.union(ix2).index.toSeq.toSet must_== { ix1.toSeq ++ ix2.toSeq }.toSet
      }
    }

    "without dups, index union is outer join" in {
      implicit val arbIndex = Arbitrary(IndexArbitraries.indexTimeNoDups)

      forAll { (ix1: Index[DateTime], ix2: Index[DateTime]) =>
        ix1.join(ix2, how=index.OuterJoin).index.toSeq.toSet must_== { ix1.toSeq ++ ix2.toSeq }.toSet
      }
    }

    "index intersect works" in {
      implicit val arbIndex = Arbitrary(IndexArbitraries.indexTimeNoDups)

      forAll { (ix1: Index[DateTime], ix2: Index[DateTime]) =>
        ix1.intersect(ix2).index.toSeq.toSet must_== ix1.toSeq.toSet[DateTime].intersect(ix2.toSeq.toSet[DateTime])
      }
    }

    "joins preserves index order with dups" in {
      implicit val arbIndex = Arbitrary(IndexArbitraries.indexTimeWithDups)

      forAll { (ix1: Index[DateTime], ix2: Index[DateTime]) =>
        val ixs1 = ix1.sorted
        val ixs2 = ix2.sorted

        ixs1.join(ixs2, how=index.RightJoin).index.isMonotonic must beTrue
        ixs1.join(ixs2, how=index.LeftJoin).index.isMonotonic must beTrue
        ixs1.join(ixs2, how=index.InnerJoin).index.isMonotonic must beTrue
        ixs1.join(ixs2, how=index.OuterJoin).index.isMonotonic must beTrue
      }
    }

    "joins preserves index order no dups" in {
      implicit val arbIndex = Arbitrary(IndexArbitraries.indexTimeNoDups)

      forAll { (ix1: Index[DateTime], ix2: Index[DateTime]) =>
        val ixs1 = ix1.sorted
        val ixs2 = ix2.sorted

        ixs1.join(ixs2, how=index.RightJoin).index.isMonotonic must beTrue
        ixs1.join(ixs2, how=index.LeftJoin).index.isMonotonic must beTrue
        ixs1.join(ixs2, how=index.InnerJoin).index.isMonotonic must beTrue
        ixs1.join(ixs2, how=index.OuterJoin).index.isMonotonic must beTrue
      }
    }

    "serialization works" in  {

      implicit val arbIndex = Arbitrary(IndexArbitraries.indexTimeWithDups)

      forAll { (ix1: Index[DateTime], ix2: Index[DateTime]) => {
        ix1 must_== serializedCopy(ix1)
        ix2 must_== serializedCopy(ix2)
      }
      }
    }

  }
}
