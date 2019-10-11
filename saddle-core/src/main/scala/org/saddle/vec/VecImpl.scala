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
package org.saddle.vec

import scala.{specialized => spec}
import org.saddle._
import org.saddle.Buffer

// Specialized method implementations for code reuse in implementations of Vec; NA-safe
private[saddle] object VecImpl {
  def mask[@spec(Boolean, Int, Long, Double) A: ST](
      v1: Vec[A],
      v2: Vec[Boolean],
      value: A
  ): Vec[A] = {
    require(v1.length == v2.length, "Vectors must be the same length")
    val buf = Array.ofDim[A](v1.length)
    var i = 0
    while (i < v1.length) {
      val a = v1.raw(i)
      val b = v2.raw(i)
      buf(i) = if (!b) a else value
      i += 1
    }
    Vec(buf)
  }

  def mask[@spec(Boolean, Int, Long, Double) A: ST](
      v1: Vec[A],
      f: A => Boolean,
      value: A
  ): Vec[A] = {
    val sa = implicitly[ST[A]]
    val buf = Array.ofDim[A](v1.length)
    var i = 0
    while (i < v1.length) {
      val a = v1.raw(i)
      buf(i) = if (sa.isMissing(a) || !f(a)) a else value
      i += 1
    }
    Vec(buf)
  }

  def foldLeft[
      @spec(Boolean, Int, Long, Double) A: ST,
      @spec(Boolean, Int, Long, Double) B
  ](vec: Vec[A])(init: B)(f: (B, A) => B): B = {
    val sa = implicitly[ST[A]]
    var acc = init
    var i = 0
    while (i < vec.length) {
      val v = vec.raw(i)
      if (sa.notMissing(v)) acc = f(acc, v)
      i += 1
    }
    acc
  }

  /**
    * Same as foldLeft, but with a condition that operates on the accumulator and element
    * that if false, breaks out of the fold
    */
  def foldLeftWhile[
      @spec(Boolean, Int, Long, Double) A: ST,
      @spec(Boolean, Int, Long, Double) B
  ](vec: Vec[A])(init: B)(f: (B, A) => B)(cond: (B, A) => Boolean): B = {
    val sa = implicitly[ST[A]]
    var acc = init
    var i = 0
    while (i < vec.length) {
      val v = vec.raw(i)
      if (sa.notMissing(v)) {
        if (cond(acc, v))
          acc = f(acc, v)
        else
          i = vec.length
      }
      i += 1
    }
    acc
  }

  def map[
      @spec(Boolean, Int, Long, Double) A: ST,
      @spec(Boolean, Int, Long, Double) B: ST
  ](vec: Vec[A])(f: A => B): Vec[B] = {
    val sca = implicitly[ST[A]]
    val scb = implicitly[ST[B]]
    val buf = Array.ofDim[B](vec.length)
    var i = 0
    while (i < vec.length) {
      val v: A = vec.raw(i)
      if (sca.isMissing(v))
        buf(i) = scb.missing
      else
        buf(i) = f(v)
      i += 1
    }
    Vec(buf)
  }

  def flatMap[
      @spec(Boolean, Int, Long, Double) A: ST,
      @spec(Boolean, Int, Long, Double) B: ST
  ](vec: Vec[A])(f: A => Vec[B]): Vec[B] = {
    var i = 0
    val b = implicitly[ST[B]].makeBuf(vec.length)
    while (i < vec.length) {
      val v: A = vec.raw(i)
      for { u <- f(v) } b.+=(u)
      i += 1
    }
    Vec(b.toArray)
  }

  /**
    * Same as foldLeft, but store and return intermediate accumulated results. Note this differs
    * from the Scala collections library by not including the initial value at the head of the
    * scan.
    */
  def scanLeft[
      @spec(Boolean, Int, Long, Double) A: ST,
      @spec(Boolean, Int, Long, Double) B: ST
  ](vec: Vec[A])(init: B)(f: (B, A) => B): Vec[B] = {
    val sca = implicitly[ST[A]]
    val scb = implicitly[ST[B]]
    val buf = Array.ofDim[B](vec.length)
    var acc = init
    var i = 0
    while (i < vec.length) {
      val v = vec.raw(i)
      if (sca.notMissing(v)) {
        acc = f(acc, v)
        buf(i) = acc
      } else {
        buf(i) = scb.missing
      }
      i += 1
    }
    Vec(buf)
  }

  def zipMap[
      @spec(Int, Long, Double) A: ST,
      @spec(Int, Long, Double) B: ST,
      @spec(Boolean, Int, Long, Double) C: ST
  ](v1: Vec[A], v2: Vec[B])(f: (A, B) => C): Vec[C] = {
    require(v1.length == v2.length, "Vectors must be the same length")
    val sca = implicitly[ST[A]]
    val scb = implicitly[ST[B]]
    val scc = implicitly[ST[C]]
    val buf = Array.ofDim[C](v1.length)
    var i = 0
    while (i < v1.length) {
      val a = v1.raw(i)
      val b = v2.raw(i)
      if (sca.isMissing(a) || scb.isMissing(b)) {
        buf(i) = scc.missing
      } else {
        buf(i) = f(a, b)
      }
      i += 1
    }
    Vec(buf)
  }

  def filterFoldLeft[
      @spec(Boolean, Int, Long, Double) A: ST,
      @spec(Boolean, Int, Long, Double) B
  ](vec: Vec[A])(pred: (A) => Boolean)(init: B)(f: (B, A) => B): B = {
    val sa = implicitly[ST[A]]
    var acc = init
    var i = 0
    while (i < vec.length) {
      val vi = vec.raw(i)
      if (sa.notMissing(vi) && pred(vi)) {
        acc = f(acc, vi)
      }
      i += 1
    }
    acc
  }

  def filterScanLeft[
      @spec(Boolean, Int, Long, Double) A: ST,
      @spec(Boolean, Int, Long, Double) B: ST
  ](vec: Vec[A])(pred: (A) => Boolean)(init: B)(f: (B, A) => B): Vec[B] = {
    val sa = implicitly[ST[A]]
    val sb = implicitly[ST[B]]
    val buf = Array.ofDim[B](vec.length)
    var acc = init
    var i = 0
    while (i < vec.length) {
      val v = vec.raw(i)
      if (sa.notMissing(v) && pred(v)) {
        acc = f(acc, v)
        buf(i) = acc
      } else {
        buf(i) = sb.missing
      }
      i += 1
    }
    Vec(buf)
  }

  def rolling[
      @spec(Boolean, Int, Long, Double) A,
      @spec(Boolean, Int, Long, Double) B: ST
  ](vec: Vec[A])(winSz: Int, f: Vec[A] => B): Vec[B] = {
    if (winSz <= 0)
      Vec.empty[B]
    else {
      val len = vec.length
      val win = if (winSz > len) len else winSz
      if (len == 0)
        Vec.empty
      else {
        val buf = new Array[B](len - win + 1)
        var i = win
        while (i <= vec.length) {
          buf(i - win) = f(vec.slice(i - win, i))
          i += 1
        }
        Vec(buf)
      }
    }
  }

  def foreach[@spec(Boolean, Int, Long, Double) A: ST](
      vec: Vec[A]
  )(op: A => Unit) {
    val sa = implicitly[ST[A]]
    var i = 0
    while (i < vec.length) {
      val v: A = vec.raw(i)
      if (sa.notMissing(v)) op(v)
      i += 1
    }
  }

  def forall[@spec(Boolean, Int, Long, Double) A: ST](
      vec: Vec[A]
  )(pred: A => Boolean)(op: A => Unit) {
    val sa = implicitly[ST[A]]
    var i = 0
    while (i < vec.length) {
      val v: A = vec.raw(i)
      if (sa.notMissing(v) && pred(v)) op(v)
      i += 1
    }
  }

  def find[@spec(Boolean, Int, Long, Double) A: ST](
      vec: Vec[A]
  )(pred: A => Boolean): Vec[Int] = {
    val sa = implicitly[ST[A]]
    var i = 0
    val buf = Buffer.empty[Int]
    while (i < vec.length) {
      val v: A = vec.raw(i)
      if (sa.notMissing(v) && pred(v)) buf.+=(i)
      i += 1
    }
    Vec(buf.toArray)
  }

  def findOneNA[@spec(Boolean, Int, Long, Double) A: ST](
      vec: Vec[A]
  ): Boolean = {
    val sa = implicitly[ST[A]]
    var ex = false
    var i = 0
    while (!ex && i < vec.length) {
      val v: A = vec.raw(i)
      ex = sa.isMissing(v)
      i += 1
    }
    ex
  }

  def isAllNA[@spec(Boolean, Int, Long, Double) A: ST](vec: Vec[A]): Boolean = {
    val sa = implicitly[ST[A]]
    var ex = true
    var i = 0
    while (ex && i < vec.length) {
      val v: A = vec.raw(i)
      ex = ex && sa.isMissing(v)
      i += 1
    }
    ex
  }

  def findOne[@spec(Boolean, Int, Long, Double) A: ST](
      vec: Vec[A]
  )(pred: A => Boolean): Int = {
    val sa = implicitly[ST[A]]
    var ex = false
    var i = 0
    while (!ex && i < vec.length) {
      val v: A = vec.raw(i)
      ex = sa.notMissing(v) && pred(v)
      i += 1
    }
    if (ex) i - 1 else -1
  }

  def filter[@spec(Boolean, Int, Long, Double) A: ST](
      vec: Vec[A]
  )(pred: A => Boolean): Vec[A] = {
    val sa = implicitly[ST[A]]
    var i = 0
    val buf = Buffer.empty[A]
    while (i < vec.length) {
      val v: A = vec.raw(i)
      if (sa.notMissing(v) && pred(v)) buf.+=(v)
      i += 1
    }
    Vec(buf.toArray)
  }

  def filterAt[@spec(Boolean, Int, Long, Double) A: ST](
      vec: Vec[A]
  )(pred: Int => Boolean): Vec[A] = {
    var i = 0
    val buf = Buffer.empty[A]
    while (i < vec.length) {
      val v: A = vec.raw(i)
      if (pred(i)) buf.+=(v)
      i += 1
    }
    Vec(buf.toArray)
  }

  def where[@spec(Boolean, Int, Long, Double) A: ST](
      vec: Vec[A]
  )(pred: Array[Boolean]): Vec[A] = {
    var i = 0
    val buf = Buffer.empty[A]
    while (i < vec.length) {
      val v: A = vec.raw(i)
      if (pred(i)) buf.+=(v)
      i += 1
    }
    Vec(buf.toArray)
  }

  def vecfillNA[@spec(Boolean, Int, Long, Double) A: ST](
      vec: Vec[A]
  )(f: (Int) => A): Vec[A] = {
    val buf = vec.contents
    var i = 0
    val l = vec.length
    val s = implicitly[ST[A]]
    while (i < l) {
      if (s.isMissing(buf(i))) buf(i) = f(i)
      i += 1
    }
    Vec(buf)
  }

  def seriesfillNA[
      @spec(Int, Long, Double) X,
      @spec(Boolean, Int, Long, Double) A: ST
  ](idx: Vec[X], vec: Vec[A])(f: X => A): Vec[A] = {
    val buf = vec.contents
    var i = 0
    val l = vec.length
    val s = implicitly[ST[A]]
    while (i < l) {
      if (s.isMissing(buf(i))) buf(i) = f(idx.raw(i))
      i += 1
    }
    Vec(buf)
  }
}
