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

import scala.language.higherKinds

import org.saddle.{Index, ST, ORD, Vec}

/**
  * An IndexMaker takes some input of type I and returns an Index whose
  * elements are of type O.
  *
  * The basic use case is to take a Tuple,,N,, of Seq-like instances and
  * return an Index whose entries are instances of Tuple,,N,, corresponding
  * to the elements of the original Seqs.
  *
  * @tparam I Type of input with which to make index
  * @tparam O Type of contents of output index
  */
trait IndexMaker[I, O] {
  def apply(in: I): Index[O]
}

/**
  * Companion object which houses implicit instances of IndexMaker
  */
object IndexMaker extends IndexMakerLowPriority {

  // -------------------------
  // IndexMaker instances

  implicit def make2V[T[K] <: IndexedSeq[K], I1: ST: ORD, I2: ST: ORD] =
    new IndexMaker[(T[I1], T[I2]), (I1, I2)] {
      def apply(in: (T[I1], T[I2])) = zip2V(in._1, in._2)
    }

  implicit def make2Vec[I1: ST: ORD, I2: ST: ORD] =
    new IndexMaker[(Vec[I1], Vec[I2]), (I1, I2)] {
      def apply(in: (Vec[I1], Vec[I2])) = zip2Vec(in._1, in._2)
    }

  implicit def make3V[T[K] <: IndexedSeq[K], I1: ST: ORD, I2: ST: ORD, I3: ST: ORD] =
    new IndexMaker[(T[I1], T[I2], T[I3]), (I1, I2, I3)] {
      def apply(in: (T[I1], T[I2], T[I3])) = zip3V(in._1, in._2, in._3)
    }

  implicit def make4V[T[K] <: IndexedSeq[K], I1: ST: ORD, I2: ST: ORD, I3: ST: ORD, I4: ST: ORD] =
    new IndexMaker[(T[I1], T[I2], T[I3], T[I4]), (I1, I2, I3, I4)] {
      def apply(in: (T[I1], T[I2], T[I3], T[I4])) =
        zip4V(in._1, in._2, in._3, in._4)
    }

  implicit def make5V[T[K] <: IndexedSeq[K], I1: ST: ORD, I2: ST: ORD, I3: ST: ORD, I4: ST: ORD, I5: ST: ORD] =
    new IndexMaker[(T[I1], T[I2], T[I3], T[I4], T[I5]), (I1, I2, I3, I4, I5)] {
      def apply(in: (T[I1], T[I2], T[I3], T[I4], T[I5])) =
        zip5V(in._1, in._2, in._3, in._4, in._5)
    }

  // -------------------------
  // Zip helpers

  private def zip2V[T[K] <: IndexedSeq[K], A: ST: ORD, B: ST: ORD](
      a: T[A],
      b: T[B]
  ): Index[(A, B)] = {
    require(a.length == b.length, "Arguments must have same length")
    val sz = a.length
    val arr = Array.ofDim[(A, B)](sz)
    var i = 0
    while (i < sz) {
      arr(i) = (a(i), b(i))
      i += 1
    }
    Index(arr)
  }

  private def zip2Vec[A: ST: ORD, B: ST: ORD](
      a: Vec[A],
      b: Vec[B]
  ): Index[(A, B)] = {
    require(a.length == b.length, "Arguments must have same length")
    val sz = a.length
    val arr = Array.ofDim[(A, B)](sz)
    var i = 0
    while (i < sz) {
      arr(i) = (a.raw(i), b.raw(i))
      i += 1
    }
    Index(arr)
  }

  private def zip3V[T[K] <: IndexedSeq[K], A: ST: ORD, B: ST: ORD, C: ST: ORD](
      a: T[A],
      b: T[B],
      c: T[C]
  ): Index[(A, B, C)] = {
    require(
      a.length == b.length && b.length == c.length,
      "Arguments must have same length"
    )
    val sz = a.length
    val arr = Array.ofDim[(A, B, C)](sz)
    var i = 0
    while (i < sz) {
      arr(i) = (a(i), b(i), c(i))
      i += 1
    }
    Index(arr)
  }

  private def zip4V[T[K] <: IndexedSeq[K], A: ST: ORD, B: ST: ORD, C: ST: ORD, D: ST: ORD](
      a: T[A],
      b: T[B],
      c: T[C],
      d: T[D]
  ): Index[(A, B, C, D)] = {
    require(
      a.length == b.length && b.length == c.length && c.length == d.length,
      "Arguments must have same length"
    )
    val sz = a.length
    val arr = Array.ofDim[(A, B, C, D)](sz)
    var i = 0
    while (i < sz) {
      arr(i) = (a(i), b(i), c(i), d(i))
      i += 1
    }
    Index(arr)
  }

  private def zip5V[T[K] <: IndexedSeq[K], A: ST: ORD, B: ST: ORD, C: ST: ORD, D: ST: ORD, E: ST: ORD](
      a: T[A],
      b: T[B],
      c: T[C],
      d: T[D],
      e: T[E]
  ): Index[(A, B, C, D, E)] = {
    require(
      a.length == b.length && b.length == c.length && c.length == d.length && d.length == e.length,
      "Arguments must have same length"
    )
    val sz = a.length
    val arr = Array.ofDim[(A, B, C, D, E)](sz)
    var i = 0
    while (i < sz) {
      arr(i) = (a(i), b(i), c(i), d(i), e(i))
      i += 1
    }
    Index(arr)
  }
}

trait IndexMakerLowPriority {

  implicit def make1V[T[K] <: IndexedSeq[K], A](
      implicit st: ST[A],
      org: ORD[A]
  ) =
    new IndexMaker[T[A], A] {
      def apply(in: T[A]): Index[A] = {
        val sz = in.length
        val arr = Array.ofDim[A](sz)
        var i = 0
        while (i < sz) {
          arr(i) = in(i)
          i += 1
        }
        Index(arr)
      }
    }
}
