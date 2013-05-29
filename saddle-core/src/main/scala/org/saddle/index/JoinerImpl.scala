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

package org.saddle.index

import scala.{ specialized => spec }
import org.saddle._
import locator.Locator

/**
 * Concrete implementation of Joiner instance which is specialized on basic
 * types.
 */
class JoinerImpl[@spec(Boolean, Int, Long, Double) T: ST: ORD] extends Joiner[T] {
  private implicit def wrapArray(arr: Array[Int]): Option[Array[Int]] = Some(arr)

  def join(left: Index[T], right: Index[T], how: JoinType): ReIndexer[T] = {
    if (left == right) {
      ReIndexer(None, None, right)
    }
    else if (left.isUnique && right.isUnique) {
      how match {
        case InnerJoin => intersect(left, right)
        case OuterJoin => union(left, right)
        case LeftJoin  => leftJoinUnique(left, right)
        case RightJoin => leftJoinUnique(right, left).swap
      }
    }
    else if (right.isUnique && how == LeftJoin) {
      leftJoinUnique(left, right)
    }
    else if (left.isUnique && how == RightJoin) {
      leftJoinUnique(right, left).swap
    }
    else if (left.isMonotonic && right.isMonotonic) {
      how match {
        case InnerJoin => innerJoinMonotonic(left, right)
        case OuterJoin => outerJoinMonotonic(left, right)
        case LeftJoin  => leftJoinMonotonic(left, right)
        case RightJoin => leftJoinMonotonic(right, left).swap
      }
    }
    else {
      how match {
        case RightJoin => factorizedJoin(right, left, LeftJoin).swap
        case _         => factorizedJoin(left, right, how)
      }
    }
  }

  // unions two indices with set semantics
  private def union(left: Index[T], right: Index[T]): ReIndexer[T] = {
    if (!left.isUnique || !right.isUnique)
      throw Index.IndexException("Cannot union non-unique indexes")

    if (left.isMonotonic && right.isMonotonic) {
      outerJoinMonotonicUnique(left, right)
    }
    else {
      outerJoinUnique(left, right)
    }
  }

  // Intersects two indices if both have set semantics
  private def intersect(left: Index[T], right: Index[T]): ReIndexer[T] = {
    if (!left.isUnique || !right.isUnique)
      throw Index.IndexException("Cannot intersect non-unique indexes")

    val ll = left.length
    val rl = right.length
    val min = ll.min(rl)
    val max = ll.max(rl)

    if (left.isMonotonic && right.isMonotonic && !(max > 5 * min)) {
      innerJoinMonotonicUnique(left, right)
    }
    else {
      innerJoinUnique(left, right)
    }
  }

  private def leftJoinUnique(left: Index[T], right: Index[T]): ReIndexer[T] = {
    val ll = left.length
    val rl = right.length

    if (left.isMonotonic && right.isMonotonic && !(ll > 5 * rl)) {
      leftJoinMonotonicUnique(left, right)
    }
    else {
      val indexer = array.empty[Int](ll)
      var i = 0
      while (i < ll) {
        val otherVal = left.raw(i)
        indexer(i) = right.getFirst(otherVal)
        i += 1
      }
      ReIndexer(None, Some(indexer), left)
    }
  }

  // driver function

  private def factorizedJoin(left: Index[T], right: Index[T], how: JoinType): ReIndexer[T] = {
    // factorize left and right inputs
    val rizer       = new Factorizer(left.length + right.length)
    val leftLabels  = rizer.factorize(left)
    val rightLabels = rizer.factorize(right)

    val max_groups = rizer.numUniq

    val JoinResult(lTake, rTake) = JoinHelper(leftLabels, rightLabels, max_groups, how)

    // construct new joint index
    val newIdx = array.empty[T](lTake.length)
    var i = 0
    while (i < newIdx.length) {
      val lpos = lTake(i)
      newIdx(i) = if (lpos != -1) left.raw(lpos) else right.raw(rTake(i))
      i += 1
    }

    ReIndexer(lTake, rTake, Index(newIdx))
  }

  // Private class to factorize indexes (ie, turn into enum representation)
  private class Factorizer(sz: Int) {
    val map     = Locator[T](sz)                // backing hashmap
    var uniques = Buffer[T](sz)                 // list of unique index keys seen
    var numUniq = 0                             // number of distinct factors

    // Yields factor labels based on all the indexes processed in a successive manner.
    // Updates factor counts as well
    def factorize(idx: Index[T]): Array[Int] = {
      val n = idx.length
      val labels = Array.ofDim[Int](n)

      var i = 0
      while (i < n) {
        val v = idx.raw(i)
        val loc = map.get(v)
        if (loc != -1) {
          labels(i) = loc
        }
        else {
          map.put(v, numUniq)
          uniques.add(v)
          labels(i) = numUniq
          numUniq += 1
        }
        i += 1
      }

      labels
    }
  }

  // ****** Fast algorithms for monotonic joins

  // helper trait to store three values into three arrays at location loc
  private trait TripleArrayStore {
    def apply(ar1: Array[Int], ar2: Array[Int], ar3: Array[T], v1: Int, v2: Int, v3: T, loc: Int)
  }

  private object TNoOp extends TripleArrayStore {
    def apply(ar1: Array[Int], ar2: Array[Int], ar3: Array[T], v1: Int, v2: Int, v3: T, loc: Int) {}
  }

  private object TStore extends TripleArrayStore {
    def apply(ar1: Array[Int], ar2: Array[Int], ar3: Array[T], v1: Int, v2: Int, v3: T, loc: Int) {
      ar1(loc) = v1
      ar2(loc) = v2
      ar3(loc) = v3
    }
  }

  def innerJoinMonotonicUnique(left: Index[T], right: Index[T]): ReIndexer[T] = {
    val scalar = left.scalarTag

    val ll = left.length
    val rl = right.length

    if (ll == 0 || rl == 0)
      ReIndexer(Some(Array.empty[Int]), Some(Array.empty[Int]), Index(Array.empty[T]))
    else {
      // first, count
      var i = 0
      var j = 0
      var c = 0

      var l: T = left.raw(i)
      var r: T = right.raw(j)
      while (i < ll && j < rl) {
        while (i < ll && { l = left.raw(i);  scalar.lt(l, r) } ) i += 1
        while (j < rl && { r = right.raw(j); scalar.lt(r, l) } ) j += 1
        if (l == r) {
          c += 1
          i += 1
          j += 1
        }
      }

      // now, fill up with values
      val res = Array.ofDim[T](c)
      val lft = Array.ofDim[Int](c)
      val rgt = Array.ofDim[Int](c)

      i = 0
      j = 0
      c = 0

      l = left.raw(i)
      r = right.raw(j)
      while (i < ll && j < rl) {
        if (scalar.lt(l, r)) {
          i += 1
          if (i < ll) l = left.raw(i)
        }
        else if (scalar.lt(r, l)) {
          j += 1
          if (j < rl) r = right.raw(j)
        }
        else {
          res(c) = l
          lft(c) = i
          rgt(c) = j
          i += 1
          j += 1
          if (i < left.length) l = left.raw(i)
          if (j < right.length) r = right.raw(j)
          c += 1
        }
      }

      // consider two special cases that speed things up down the line
      if (c == ll)
        ReIndexer(None, rgt, left)
      else if (c == rl)
        ReIndexer(lft, None, right)
      else
        ReIndexer(lft, rgt, Index(res))
    }
  }

  def innerJoinMonotonic(left: Index[T], right: Index[T]): ReIndexer[T] = {
    val scalar = left.scalarTag
    val nleft  = left.length
    val nright = right.length

    // first pass counts
    def passThru(callback: TripleArrayStore, l: Array[Int], r: Array[Int], res: Array[T]): Int = {
      var lc = 0
      var rc = 0
      var rgrp = 0
      var count = 0
      if (nleft > 0 && nright > 0) {
        while (lc < nleft && rc < nright) {
          val lval: T = left.raw(lc)
          val rval: T = right.raw(rc)
          if (lval == rval) {
            callback(l, r, res, lc, rc, lval, count)
            rc += 1
            if (rc < nright && right.raw(rc) == lval)
              rgrp += 1
            else {
              lc += 1
              rc -= rgrp + 1
              rgrp = 0
            }
            count += 1
          }
          else if (scalar.lt(lval, rval))
            lc += 1
          else
            rc += 1
        }
      }
      count
    }

    // first pass counts
    val nobs = passThru(TNoOp, null, null, null)

    val (lIdx, rIdx, result) = (array.empty[Int](nobs), array.empty[Int](nobs), array.empty[T](nobs))

    // second pass populates results
    passThru(TStore, lIdx, rIdx, result)

    ReIndexer(lIdx, rIdx, Index(result))
  }

  def innerJoinUnique(left: Index[T], right: Index[T]): ReIndexer[T] = {
    // want to scan over the smaller one; make lft the smaller one
    val szhint = if (left.length > right.length) right.length else left.length

    val res = Buffer[T](szhint)
    val lft = Buffer[Int](szhint)
    val rgt = Buffer[Int](szhint)

    val switchLR = left.length > right.length

    val (ltmp, rtmp) = if (switchLR) (right, left) else (left, right)

    var i = 0
    while (i < ltmp.length) {
      val k = ltmp.raw(i)
      val v = rtmp.getFirst(k)
      if (v != -1) {
        res.add(k)
        rgt.add(v)
        lft.add(i)
      }
      i += 1
    }

    val result: Array[T] = res
    val (lres, rres) = if (switchLR) (rgt, lft) else (lft, rgt)

    ReIndexer(Some(lres), Some(rres), Index(Vec(result)))
  }

  def outerJoinMonotonicUnique(left: Index[T], right: Index[T]): ReIndexer[T] = {
    val scalar = left.scalarTag

    val ll = left.length
    val rl = right.length

    if (ll == 0) {
      val lft = Array.ofDim[Int](rl)
      var i = 0
      while(i < rl) { lft(i) = -1; i += 1 }
      ReIndexer(lft, None, right)
    }
    else if (rl == 0) {
      val rgt = Array.ofDim[Int](ll)
      var i = 0
      while(i < ll) { rgt(i) = -1; i += 1 }
      ReIndexer(None, rgt, left)
    }
    else {
      // first count uniques
      var c = 0
      var i = 0
      var j = 0
      var l: T = left.raw(0)
      var r: T = right.raw(0)
      while (i < ll && j < rl) {
        l = left.raw(i)
        r = right.raw(j)
        if (l == r) {
          c += 1
          i += 1
          j += 1
        }
        else if (scalar.lt(l, r)) {
          c += 1
          i += 1
        }
        else {
          c += 1
          j += 1
        }
      }
      c += (ll - i)
      c += (rl - j)

      // then fill

      val res = Array.ofDim[T](c)
      val lft = Array.ofDim[Int](c)
      val rgt = Array.ofDim[Int](c)

      c = 0
      i = 0
      j = 0
      l = left.raw(0)
      r = right.raw(0)
      while (i < ll && j < rl) {
        while (i < ll && scalar.lt(l, r)) {
          res(c) = l
          lft(c) = i
          rgt(c) = -1
          i += 1
          c += 1
          if (i < ll) l = left.raw(i)
        }
        while (i < ll && j < rl && r == l) {
          res(c) = r
          rgt(c) = j
          lft(c) = i
          j += 1
          i += 1
          c += 1
          if (i < ll) l = left.raw(i)
          if (j < rl) r = right.raw(j)
        }
        while (j < rl && scalar.lt(r, l)) {
          res(c) = r
          lft(c) = -1
          rgt(c) = j
          j += 1
          c += 1
          if (j < rl) r = right.raw(j)
        }
      }
      while (i < ll) {
        res(c) = left.raw(i)
        lft(c) = i
        rgt(c) = -1
        c += 1
        i += 1
      }
      while (j < rl) {
        res(c) = right.raw(j)
        rgt(c) = j
        lft(c) = -1
        c += 1
        j += 1
      }

      ReIndexer(lft, rgt, Index(res))
    }
  }

  def outerJoinMonotonic(left: Index[T], right: Index[T]): ReIndexer[T] = {
    val scalar = left.scalarTag
    val nleft  = left.length
    val nright = right.length

    // first pass counts
    def passThru(callback: TripleArrayStore, l: Array[Int], r: Array[Int], res: Array[T]): Int = {
      var lc = 0
      var rc = 0
      var done = false
      var count = 0
      if (nleft == 0) {
        if (callback == TNoOp)
          count = nright
        else while (rc < nright) {
          val v: T = right.raw(rc)
          callback(l, r, res, -1, rc, v, rc)
          rc += 1
        }
      }
      else if (nright == 0) {
        if (callback == TNoOp)
          count = nleft
        else while (lc < nleft) {
          val v: T = left.raw(lc)
          callback(l, r, res, lc, -1, v, lc)
          lc += 1
        }
      }
      else {
        while (!done) {
          if (lc == nleft) {
            if (callback == TNoOp)
              count += nright - rc
            else while (rc < nright) {
              val v: T = right.raw(rc)
              callback(l, r, res, -1, rc, v, count)
              count += 1
              rc += 1
            }
            done = true
          }
          else if (rc == nright) {
            if (callback == TNoOp)
              count += nleft - lc
            else while (lc < nleft) {
              val v: T = left.raw(lc)
              callback(l, r, res, lc, -1, v, count)
              count += 1
              lc += 1
            }
            done = true
          }
          else {
            val lval: T = left.raw(lc)
            val rval: T = right.raw(rc)
            if (lval == rval) {
              var ldups = 0
              var rdups = 0
              while (lc + ldups < nleft  && lval == left.raw(lc + ldups))  ldups += 1
              while (rc + rdups < nright && rval == right.raw(rc + rdups)) rdups += 1
              var m = 0
              while (m < ldups) {
                var n = 0
                while (n < rdups) {
                  callback(l, r, res, lc + m, rc + n, lval, count)
                  count += 1
                  n += 1
                }
                m += 1
              }
              lc += ldups
              rc += rdups
            }
            else if (scalar.lt(lval, rval)) {
              callback(l, r, res, lc, -1, lval, count)
              count += 1
              lc += 1
            }
            else {
              callback(l, r, res, -1, rc, rval, count)
              count += 1
              rc += 1
            }
          }
        }
      }
      count
    }

    // first pass counts
    val nobs = passThru(TNoOp, null, null, null)

    val (lIdx, rIdx, result) = (array.empty[Int](nobs), array.empty[Int](nobs), array.empty[T](nobs))

    // second pass populates results
    passThru(TStore, lIdx, rIdx, result)

    ReIndexer(lIdx, rIdx, Index(result))
  }

  def outerJoinUnique(left: Index[T], right: Index[T]): ReIndexer[T] = {
    // hits hashmap
    val szhint = left.length + right.length

    val res = Buffer[T](szhint)
    val lft = Buffer[Int](szhint)
    val rgt = Buffer[Int](szhint)

    var i = 0
    while (i < left.length) {
      val v = left.raw(i)
      val r = right.getFirst(v)
      res.add(v)
      lft.add(i)
      rgt.add(r)
      i += 1
    }

    var j = 0
    while (j < right.length) {
      val v = right.raw(j)
      if (left.getFirst(v) == -1) {
        res.add(v)
        rgt.add(j)
        lft.add(-1)
      }
      j += 1
    }

    val result: Array[T] = res
    ReIndexer(Some(lft), Some(rgt), Index(result))
  }

  def leftJoinMonotonicUnique(left: Index[T], right: Index[T]): ReIndexer[T] = {
    val scalar = left.scalarTag
    val rgt = Array.ofDim[Int](left.length)

    var i = 0
    var j = 0
    val ll = left.length
    val rl = right.length

    while (i < ll && j < rl) {
      val l: T = left.raw(i)
      var r: T = l

      while (j < rl && scalar.lt( { r = right.raw(j); r }, l)) {
        j += 1
      }

      if (j < rl && l == r)
        rgt(i) = j
      else
        rgt(i) = -1

      i += 1
    }

    while(i < ll) {
      rgt(i) = -1
      i += 1
    }

    ReIndexer(None, rgt, left)
  }

  def leftJoinMonotonic(left: Index[T], right: Index[T]): ReIndexer[T] = {
    val scalar = left.scalarTag
    val nleft  = left.length
    val nright = right.length

    def passThru(callback: TripleArrayStore, l: Array[Int], r: Array[Int], res: Array[T]): Int = {
      var lc = 0
      var rc = 0
      var rgrp = 0
      var done = false
      var count = 0

      if (nleft > 0) {
        while (!done) {
          if (lc == nleft) {
            done = true
          }
          else if (rc == nright) {
            if (callback == TNoOp)
              count += nleft - lc
            else while (lc < nleft) {
              val v: T = left.raw(lc)
              callback(l, r, res, lc, -1, v, count)
              count += 1
              lc += 1
            }
            done = true
          }
          else {
            val lval: T = left.raw(lc)
            val rval: T = right.raw(rc)
            if (lval == rval) {
              callback(l, r, res, lc, rc, lval, count)
              rc += 1
              if (rc < nright && right.raw(rc) == lval)
                rgrp += 1
              else {
                lc += 1
                rc -= rgrp + 1
                rgrp = 0
              }
              count += 1
            }
            else if (scalar.lt(lval, rval)) {
              callback(l, r, res, lc, -1, lval, count)
              count += 1
              lc += 1
            }
            else {
              rc += 1
            }
          }
        }
      }
      count
    }

    // first pass counts
    val nobs = passThru(TNoOp, null, null, null)

    val (lIdx, rIdx, result) = (array.empty[Int](nobs), array.empty[Int](nobs), array.empty[T](nobs))

    // second pass populates results
    passThru(TStore, lIdx, rIdx, result)

    ReIndexer(lIdx, rIdx, Index(result))
  }
}

private[saddle] object JoinerImpl {
  def join[@spec(Boolean, Int, Long, Double) T: ST: ORD](
    left: Index[T], right: Index[T], how: JoinType) = (new JoinerImpl[T]).join(left, right, how)
}