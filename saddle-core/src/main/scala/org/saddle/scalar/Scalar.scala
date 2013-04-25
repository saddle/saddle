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

package org.saddle.scalar

import org.saddle._
import java.util.NoSuchElementException

/**
 * Scalar wrapper for a single element of a vector-like container.
 *
 * @tparam T The type of element wrapped
 */
sealed abstract class Scalar[+T] {
  def isNA: Boolean
  def get: T

  @inline final def map[B: ST](f: T => B): Scalar[B] =
    if (isNA) NA else Value(f(get))

  @inline final def flatMap[B](f: T => Scalar[B]): Scalar[B] =
    if (isNA) NA else f(this.get)

  @inline final def foreach[U](f: T => U) {
    if (!isNA) f(this.get)
  }
}

object Scalar {
  /** An Scalar factory which creates Value(x) when the argument is neither null nor an NA primitive;
    * otherwise produces NA.
    *
    *  @param  x the value
    *  @return Value(value) if value not null or NA primitive; otherwise NA
    *  */
  def apply[T: ST](x: T): Scalar[T] =
    if (x == null || implicitly[ST[T]].isMissing(x)) NA else Value(x)

  /**
   * Provides comparisons of Scalars, where NA always evaluates as less than non-NA
   */
  implicit def ord[T : ORD] = new ORD[Scalar[T]] {
    def compare(x: Scalar[T], y: Scalar[T]): Int = (x, y) match {
      case (NA, NA) =>  0
      case (NA,  _) => -1
      case (_,  NA) =>  1
      case (_,   _) => implicitly[ORD[T]].compare(x.get, y.get)
    }
  }

  /**
   * Provides implicit boxing of primitive to scalar
   */
  implicit def scalarBox[T : ST](el: T): Scalar[T] = Scalar(el)

  /**
   * Provides implicit unboxing from double scalar to primitive
   */
  implicit def scalarUnboxD(ds: Scalar[Double]): Double = {
    if (ds.isNA) Double.NaN else ds.get
  }

  /**
   * Provides implicit unboxing from float scalar to primitive
   */
  implicit def scalarUnboxF(ds: Scalar[Float]): Float = {
    if (ds.isNA) Float.NaN else ds.get
  }

  /**
   * Scalar is isomorphic to Option
   */
  implicit def scalarToOption[T](sc: Scalar[T]): Option[T] = if (sc.isNA) None else Some(sc.get)
  implicit def optionToScalar[T: ST](op: Option[T]): Scalar[T] = op.map { Scalar(_) } getOrElse NA
}

case class Value[+T : ST](el: T) extends Scalar[T] {
  def isNA = implicitly[ST[T]].isMissing(el)
  def get = el

  override def toString = el.toString
}

case object NA extends Scalar[Nothing] {
  def isNA = true
  def get  = throw new NoSuchElementException("NA.get")

  override def toString = "NA"
}