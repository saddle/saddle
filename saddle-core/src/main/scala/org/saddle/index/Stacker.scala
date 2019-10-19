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

import org.saddle.{ST, ORD, Index}

/**
  * A Stacker operates on two input Index instances and produces a new output Index
  * whose entries are drawn from the Cartesian product of the elements of the original
  * indexes, and whose ordering is likewise specified by the original orderings. For
  * instance,
  *
  * {{{
  *   Index[Int](1,2) stack Index[Char]('a','b')
  * }}}
  *
  * results in
  *
  * {{{
  *   Index[(Int, Char)]((1,'a'), (1,'b'), (2,'a'), (2,'b'))
  * }}}
  *
  * whereas
  *
  * {{{
  *   Index[(Char, Int)](('x',1), ('y',2)) stack Index[Char]('a','b')
  * }}}
  *
  * results in
  *
  * {{{
  *   Index[(Char, Int, Char)](('x',1,'a'), ('x',1,'b'), ('y',2,'a'), ('y',2,'b'))
  * }}}
  *
  * @tparam I Type of the elements of the left index
  * @tparam J Type of the elements of the right index
  * @tparam O Type of the elements of the output index
  */
trait Stacker[I, J, O] {

  /**
    * Take two indexes and stack them, producing a third index
    * @param ix1 Left index
    * @param ix2 Right index
    */
  def apply(ix1: Index[I], ix2: Index[J]): Index[O]

  /**
    * Implementation of an Ordering for O
    */
  def ord: ORD[O]

  /**
    * Implementation of a ST for O
    */
  def tag: ST[O]
}

/**
  * Companion object which houses implicit Stacker instances.
  */
object Stacker extends StackerLowPriority {
  implicit def stack3rd[T1: ST: ORD, T2: ST: ORD, T3: ST: ORD] =
    new Stacker[(T1, T2), T3, (T1, T2, T3)] {
      def apply(ix1: Index[(T1, T2)], ix2: Index[T3]) = {
        val data =
          for (x <- ix1.toArray; y <- ix2.toArray) yield (x._1, x._2, y)
        Index(data)
      }

      def ord = implicitly[ORD[(T1, T2, T3)]]
      def tag = implicitly[ST[(T1, T2, T3)]]
    }

  implicit def stack4th[T1: ST: ORD, T2: ST: ORD, T3: ST: ORD, T4: ST: ORD] =
    new Stacker[(T1, T2, T3), T4, (T1, T2, T3, T4)] {
      def apply(ix1: Index[(T1, T2, T3)], ix2: Index[T4]) = {
        val data =
          for (x <- ix1.toArray; y <- ix2.toArray) yield (x._1, x._2, x._3, y)
        Index(data)
      }

      def ord = implicitly[ORD[(T1, T2, T3, T4)]]
      def tag = implicitly[ST[(T1, T2, T3, T4)]]
    }

  implicit def stack5th[
      T1: ST: ORD,
      T2: ST: ORD,
      T3: ST: ORD,
      T4: ST: ORD,
      T5: ST: ORD
  ] =
    new Stacker[(T1, T2, T3, T4), T5, (T1, T2, T3, T4, T5)] {
      def apply(ix1: Index[(T1, T2, T3, T4)], ix2: Index[T5]) = {
        val data =
          for (x <- ix1.toArray; y <- ix2.toArray)
            yield (x._1, x._2, x._3, x._4, y)
        Index(data)
      }

      def ord = implicitly[ORD[(T1, T2, T3, T4, T5)]]
      def tag = implicitly[ST[(T1, T2, T3, T4, T5)]]
    }

  implicit def stack6th[
      T1: ST: ORD,
      T2: ST: ORD,
      T3: ST: ORD,
      T4: ST: ORD,
      T5: ST: ORD,
      T6: ST: ORD
  ] =
    new Stacker[(T1, T2, T3, T4, T5), T6, (T1, T2, T3, T4, T5, T6)] {
      def apply(ix1: Index[(T1, T2, T3, T4, T5)], ix2: Index[T6]) = {
        val data =
          for (x <- ix1.toArray; y <- ix2.toArray)
            yield (x._1, x._2, x._3, x._4, x._5, y)
        Index(data)
      }

      def ord = implicitly[ORD[(T1, T2, T3, T4, T5, T6)]]
      def tag = implicitly[ST[(T1, T2, T3, T4, T5, T6)]]
    }

  implicit def stack7th[
      T1: ST: ORD,
      T2: ST: ORD,
      T3: ST: ORD,
      T4: ST: ORD,
      T5: ST: ORD,
      T6: ST: ORD,
      T7: ST: ORD
  ] =
    new Stacker[(T1, T2, T3, T4, T5, T6), T7, (T1, T2, T3, T4, T5, T6, T7)] {
      def apply(ix1: Index[(T1, T2, T3, T4, T5, T6)], ix2: Index[T7]) = {
        val data =
          for (x <- ix1.toArray; y <- ix2.toArray)
            yield (x._1, x._2, x._3, x._4, x._5, x._6, y)
        Index(data)
      }

      def ord = implicitly[ORD[(T1, T2, T3, T4, T5, T6, T7)]]
      def tag = implicitly[ST[(T1, T2, T3, T4, T5, T6, T7)]]
    }

  implicit def stack8th[
      T1: ST: ORD,
      T2: ST: ORD,
      T3: ST: ORD,
      T4: ST: ORD,
      T5: ST: ORD,
      T6: ST: ORD,
      T7: ST: ORD,
      T8: ST: ORD
  ] =
    new Stacker[
      (T1, T2, T3, T4, T5, T6, T7),
      T8,
      (T1, T2, T3, T4, T5, T6, T7, T8)
    ] {
      def apply(ix1: Index[(T1, T2, T3, T4, T5, T6, T7)], ix2: Index[T8]) = {
        val data =
          for (x <- ix1.toArray; y <- ix2.toArray)
            yield (x._1, x._2, x._3, x._4, x._5, x._6, x._7, y)
        Index(data)
      }

      def ord = implicitly[ORD[(T1, T2, T3, T4, T5, T6, T7, T8)]]
      def tag = implicitly[ST[(T1, T2, T3, T4, T5, T6, T7, T8)]]
    }

  implicit def stack9th[
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
    new Stacker[
      (T1, T2, T3, T4, T5, T6, T7, T8),
      T9,
      (T1, T2, T3, T4, T5, T6, T7, T8, T9)
    ] {
      def apply(
          ix1: Index[(T1, T2, T3, T4, T5, T6, T7, T8)],
          ix2: Index[T9]
      ) = {
        val data =
          for (x <- ix1.toArray; y <- ix2.toArray)
            yield (x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, y)
        Index(data)
      }

      def ord = implicitly[ORD[(T1, T2, T3, T4, T5, T6, T7, T8, T9)]]
      def tag = implicitly[ST[(T1, T2, T3, T4, T5, T6, T7, T8, T9)]]
    }
}

/**
  * Implicit instance of Stacker for two indexes of arbitrary type. The priority is
  * lower than the Stacker instances in the Stacker companion object because we want
  * to specialize the case when the left index is composed of Tuples.
  */
trait StackerLowPriority {
  implicit def stack2nd[T1: ST: ORD, T2: ST: ORD] =
    new Stacker[T1, T2, (T1, T2)] {
      def apply(ix1: Index[T1], ix2: Index[T2]) = {
        val data = for (x <- ix1.toArray; y <- ix2.toArray) yield (x, y)
        Index(data)
      }

      def ord = implicitly[ORD[(T1, T2)]]
      def tag = implicitly[ST[(T1, T2)]]
    }
}
