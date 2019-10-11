package org.saddle.util

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
import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import org.scalacheck.Prop._

/**
  * Tests for Concat
  */
class LongMapCheck extends Specification with ScalaCheck {
  "LongMap" in {
    forAll { (a1: Seq[(Long, Int)]) =>
      val lmap = new LongMap
      val hmap = scala.collection.mutable.HashMap[Long, Int]()
      a1.foreach { case (k, v)  => lmap.update(k, v) }
      a1.foreach { case (k, v)  => hmap.update(k, v) }
      a1.forall { case (k, _)   => lmap.get(k) == hmap.get(k) } and
        a1.forall { case (k, _) => lmap.get(k + 1) == hmap.get(k + 1) } and
        a1.forall { case (k, _) => lmap.get(k - 1) == hmap.get(k - 1) }

    }
  }
  "DoubleMap" in {
    forAll { (a1: Seq[(Double, Int)]) =>
      val lmap = new DoubleMap
      val hmap = scala.collection.mutable.HashMap[Double, Int]()
      a1.foreach { case (k, v)  => lmap.update(k, v) }
      a1.foreach { case (k, v)  => hmap.update(k, v) }
      a1.forall { case (k, _)   => lmap.get(k) == hmap.get(k) } and
        a1.forall { case (k, _) => lmap.get(k + 1) == hmap.get(k + 1) } and
        a1.forall { case (k, _) => lmap.get(k - 1) == hmap.get(k - 1) }
    }
  }
  "IntMap" in {
    forAll { (a1: Seq[(Int, Int)]) =>
      val lmap = new IntMap
      val hmap = scala.collection.mutable.HashMap[Int, Int]()
      a1.foreach { case (k, v)  => lmap.update(k, v) }
      a1.foreach { case (k, v)  => hmap.update(k, v) }
      a1.forall { case (k, _)   => lmap.get(k) == hmap.get(k) } and
        a1.forall { case (k, _) => lmap.get(k + 1) == hmap.get(k + 1) } and
        a1.forall { case (k, _) => lmap.get(k - 1) == hmap.get(k - 1) }
    }
  }
}
