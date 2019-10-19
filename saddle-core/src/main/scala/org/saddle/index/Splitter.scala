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

import org.saddle.{Index, ST, ORD}

/**
  * A Splitter operates on an input index whose elements have arity N, and yields the following
  * pair of output indexes: the left has elements whose arity is N-1, where each element has the
  * first N-1 constituents of the original tuple; and the right is an index whose elements were
  * those in the Nth position of the original tuple.
  *
  * For example,
  *
  * {{{
  *   Index[(Char, Int)](('a', 1), ('a', 2), ('b', 1), ('b', 2)).split
  * }}}
  *
  * yields
  *
  * {{{
  *   (Index[Char]('a', 'a', 'b', 'b'), Index[Int](1, 2, 1, 2))
  * }}}
  *
  * @tparam I Input index whose elements have arity > 1
  * @tparam OL Left output index whose elements have arity >= 1
  * @tparam OR Right output index whose elements have arity 1
  */
trait Splitter[I, OL, OR] {
  def apply(i: Index[I]): (Index[OL], Index[OR])
}

/**
  * Companion object houses implicit instances of Splitter
  */
object Splitter {
  implicit def split2nd[T1: ST: ORD, T2: ST: ORD] =
    new Splitter[(T1, T2), T1, T2] {
      def apply(i: Index[(T1, T2)]) = (i.map(_._1), i.map(_._2))
    }

  implicit def split3rd[T1: ST: ORD, T2: ST: ORD, T3: ST: ORD] =
    new Splitter[(T1, T2, T3), (T1, T2), T3] {
      def apply(i: Index[(T1, T2, T3)]) =
        (i.map(t => (t._1, t._2)), i.map(_._3))
    }

  implicit def split4th[T1: ST: ORD, T2: ST: ORD, T3: ST: ORD, T4: ST: ORD] =
    new Splitter[(T1, T2, T3, T4), (T1, T2, T3), T4] {
      def apply(i: Index[(T1, T2, T3, T4)]) =
        (i.map(t => (t._1, t._2, t._3)), i.map(_._4))
    }

  implicit def split5th[
      T1: ST: ORD,
      T2: ST: ORD,
      T3: ST: ORD,
      T4: ST: ORD,
      T5: ST: ORD
  ] =
    new Splitter[(T1, T2, T3, T4, T5), (T1, T2, T3, T4), T5] {
      def apply(i: Index[(T1, T2, T3, T4, T5)]) =
        (i.map(t => (t._1, t._2, t._3, t._4)), i.map(_._5))
    }

  implicit def split6th[
      T1: ST: ORD,
      T2: ST: ORD,
      T3: ST: ORD,
      T4: ST: ORD,
      T5: ST: ORD,
      T6: ST: ORD
  ] =
    new Splitter[(T1, T2, T3, T4, T5, T6), (T1, T2, T3, T4, T5), T6] {
      def apply(i: Index[(T1, T2, T3, T4, T5, T6)]) =
        (i.map(t => (t._1, t._2, t._3, t._4, t._5)), i.map(_._6))
    }

  implicit def split7th[
      T1: ST: ORD,
      T2: ST: ORD,
      T3: ST: ORD,
      T4: ST: ORD,
      T5: ST: ORD,
      T6: ST: ORD,
      T7: ST: ORD
  ] =
    new Splitter[(T1, T2, T3, T4, T5, T6, T7), (T1, T2, T3, T4, T5, T6), T7] {
      def apply(i: Index[(T1, T2, T3, T4, T5, T6, T7)]) =
        (i.map(t => (t._1, t._2, t._3, t._4, t._5, t._6)), i.map(_._7))
    }

  implicit def split8th[
      T1: ST: ORD,
      T2: ST: ORD,
      T3: ST: ORD,
      T4: ST: ORD,
      T5: ST: ORD,
      T6: ST: ORD,
      T7: ST: ORD,
      T8: ST: ORD
  ] =
    new Splitter[
      (T1, T2, T3, T4, T5, T6, T7, T8),
      (T1, T2, T3, T4, T5, T6, T7),
      T8
    ] {
      def apply(i: Index[(T1, T2, T3, T4, T5, T6, T7, T8)]) =
        (i.map(t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7)), i.map(_._8))
    }

  implicit def split9th[
      T1: ST: ORD,
      T2: ST: ORD,
      T3: ST: ORD,
      T4: ST: ORD,
      T5: ST: ORD,
      T6: ST: ORD,
      T7: ST: ORD,
      T8: ST: ORD,
      T9: ST: ORD
  ] =
    new Splitter[
      (T1, T2, T3, T4, T5, T6, T7, T8, T9),
      (T1, T2, T3, T4, T5, T6, T7, T8),
      T9
    ] {
      def apply(i: Index[(T1, T2, T3, T4, T5, T6, T7, T8, T9)]) =
        (
          i.map(t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8)),
          i.map(_._9)
        )
    }
}
