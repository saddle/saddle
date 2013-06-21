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

package org

import joda.time.DateTime
import org.saddle.index._

// some typeclass interfaces we'll alias
import org.saddle.scalar.ScalarTag

/**
 * == Saddle ==
 *
 * Saddle is a '''S'''cala '''D'''ata '''L'''ibrary.
 *
 * Saddle provides array-backed, indexed one- and two-dimensional data structures.
 *
 * These data structures are specialized on JVM primitives. With them one can often avoid
 * the overhead of boxing and unboxing.
 *
 * Basic operations also aim to be robust to missing values (NA's)
 *
 * The building blocks are intended to be easily composed.
 *
 * Additionally, there are some numerical utilities: eg, a fast suite of [[org.saddle.util.Random]]
 * number generators).
 *
 * The foundational building blocks are:
 *
 *  - [[org.saddle.Vec]]
 *  - [[org.saddle.Mat]]
 *  - [[org.saddle.Index]]
 *  - [[org.saddle.Series]]
 *  - [[org.saddle.Frame]]
 *
 * Inspiration for Saddle comes from many sources, including the R programming language, the
 * pandas data analysis library for Python, and the Scala collections library.
 */
package object saddle {
  // ********************** Some type aliases, save our fingers in typing

  /**
   * Shorthand for ordering typeclass
   */
  type ORD[C] = Ordering[C]

  /**
   * Shorthand for numeric typeclass
   */
  type NUM[C] = Numeric[C]

  /**
   * Shorthand for class manifest typeclass
   */
  type CLM[C] = ClassManifest[C]

  /**
   * Shorthand for scalar tag typeclass
   */
  type ST[C] = ScalarTag[C]

  // **********************

  /**
   * Allow timing of an operation
   *
   * {{{
   *   clock { bigMat.T dot bigMat }
   * }}}
   *
   */
  def clock[T](op: => T): (Double, T) = {
    val s = System.nanoTime
    val r = op
    val e = System.nanoTime
    ((e-s)/1e9, r)
  }

  /**
   * Syntactic sugar, allow '->' to generate an (inclusive) index slice
   *
   * {{{
   *   val v = Vec(1,2,3,4)
   *   val u = v(0 -> 2)
   * }}}
   */
  implicit def pair2Slice[T](p: (T, T)) = Slice(p._1, p._2)

  /**
   * Syntactic sugar, allow '* -> ' to generate an (inclusive) index slice, open on left
   *
   * {{{
   *   val v = Vec(1,2,3,4)
   *   val u = v(* -> 2)
   * }}}
   */
  implicit def pair2SliceTo[T](p: (SliceAll, T)) = SliceTo(p._2)

  /**
   * Syntactic sugar, allow ' -> *' to generate an (inclusive) index slice, open on right
   *
   * {{{
   *   val v = Vec(1,2,3,4)
   *   val u = v(1 -> *)
   * }}}
   */
  implicit def pair2SliceFrom[T](p: (T, SliceAll)) = SliceFrom(p._1)

  /**
   * Provides for one-element slicing, e.g.
   *
   * {{{
   *   val v = Vec(1,2,3, 4)
   *   val u = v(1)
   * }}}
   */
  implicit def pair2SliceSingle[T](k: T) = Slice(k, k)

  /**
   * Syntactic sugar, placeholder for 'slice-all'
   *
   * {{{
   *   val v = Vec(1,2,3, 4)
   *   val u = v(*)
   * }}}
   */
  def * = SliceAll()

  /**
   * `na` provides syntactic sugar for constructing primitives recognized as NA.
   * A use case is be:
   *
   * {{{
   *   Vec[Int](1,2,na,4)
   * }}}
   *
   * `na` will implicitly convert to a primitive having the designated missing
   * value bit pattern. That pattern is as follows:
   *
   *  1. byte    => Byte.MinValue
   *  1. char    => Char.MinValue
   *  1. short   => Short.Minvalue
   *  1. int     => Int.MinValue
   *  1. long    => Long.MinValue
   *  1. float   => Float.NaN
   *  1. double  => Double.NaN
   *
   *  The NA bit pattern for integral types is `MinValue` because it induces a
   *  symmetry on the remaining bound of values; e.g. the remaining `Byte` bound
   *  is (-127, +127).
   *
   *  Note since `Boolean`s can only take on two values, it has no `na` primitive
   *  bit pattern.
   */
  object na {
    /**
     * Generates a primitive missing value bit pattern.
     */
    def to[T](implicit fn: na.type => T): T = fn(this)

    implicit def naToByte(v: na.type): Byte = scalar.ScalarTagByte.missing
    implicit def naToChar(v: na.type): Char = scalar.ScalarTagChar.missing
    implicit def naToShort(v: na.type): Short = scalar.ScalarTagShort.missing

    implicit def naToInt(v: na.type): Int = scalar.ScalarTagInt.missing
    implicit def naToLong(v: na.type): Long = scalar.ScalarTagLong.missing
    implicit def naToFloat(v: na.type): Float = scalar.ScalarTagFloat.missing
    implicit def naToDouble(v: na.type): Double = scalar.ScalarTagDouble.missing

    override def toString = "na"
  }

  // Augment Seq with a few conversion methods
  //

  /**
   * Augments Seq with a toVec method that returns a new Vec instance.
   *
   * For example,
   *
   * {{{
   *   val s = IndexedSeq(1,2,3)
   *   val v = s.toVec
   * }}}
   *
   * @param s  A value of type Seq[T]
   * @tparam T Type of elements of Vec
   */
  implicit def seqToVec[T: ST](s: Seq[T]) = new {
    def toVec: Vec[T] = Vec(s : _*)
  }

  /**
   * Augments Seq with a toIndex method that returns a new Index instance.
   *
   * For example,
   *
   * {{{
   *   val i = IndexedSeq(1,2,3)
   *   val s = i.toIndex
   * }}}
   *
   * @param ix A value of type Seq[X]
   * @tparam X Type of index elements
   */
  implicit def seqToIndex[X: ST: ORD](ix: Seq[X]) = new {
    def toIndex: Index[X] = Index(ix : _*)
  }

  /**
   * Augments Seq with a toSeries method that returns a new Series instance.
   *
   * For example,
   *
   * {{{
   *   val p = IndexedSeq(1,2,3) zip IndexedSeq(4,5,6)
   *   val s = p.toSeries
   * }}}
   *
   * @param s  A value of type Seq[(X, T)]
   * @tparam T Type of data elements of Series
   * @tparam X Type of index elements of Series
   */
  implicit def seqToSeries[T: ST, X: ST: ORD](s: Seq[(X, T)]) = new {
    def toSeries: Series[X, T] = Series(s : _*)
  }

  /**
   * Augments Seq with a toFrame method that returns a new Frame instance.
   *
   * For example,
   *
   * {{{
   *   val t = IndexedSeq(("a", "x", 3), ("b", "y", 4))
   *   val f = t.toFrame
   *
   *   res0: org.saddle.Frame[java.lang.String,java.lang.String,Int] =
   *   [2 x 2]
   *         x  y
   *        -- --
   *   a ->  3 NA
   *   b -> NA  4
   * }}}
   *
   * @param s  A value of type Seq[(RX, CX, T)]
   * @tparam T Type of data elements of Frame
   * @tparam RX Type of row index elements of Frame
   * @tparam CX Type of col index elements of Frame
   */
  implicit def seqToFrame[RX: ST: ORD, CX: ST: ORD, T: ST](s: Seq[(RX, CX, T)]) = new {
    def toFrame: Frame[RX, CX, T] = {
      val grp = s.map { case (r, c, v) => ((r, c), v) }
      grp.toSeries.pivot
    }
  }

  /**
   * Constant used in string byte-level manipulation
   */
  val UTF8 = "UTF-8"
}