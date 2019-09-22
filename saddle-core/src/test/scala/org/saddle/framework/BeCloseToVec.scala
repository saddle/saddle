/**
 * Copyright (c) 2013 Saddle Development Team
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 **/

package org.saddle.framework

import org.saddle._
import org.specs2.matcher._
import scala.reflect.ClassTag

/**
 * A matcher for two numeric Vecs that must be equal to within
 * a tolerance
 */
class BeCloseToVec[T: Numeric : ClassTag](v: Vec[T], delta: T) extends Matcher[Vec[T]] {
  def apply[S <: Vec[T]](x: Expectable[S]) = {
    val num = implicitly[Numeric[T]]

    result(v.length == 0 || {
      val res = v.toSeq.zipWithIndex map {
        case (n, i) =>
          num.lteq(num.minus(n, delta), x.value.raw(i)) &&
          num.lteq(x.value.raw(i), num.plus(n, delta))
      }
      Vec(res: _*).all
    },
      " are close +/- " + delta, " are close +/- " + delta, x)
  }
}

object BeCloseToVec {
  def apply[T: Numeric : ClassTag](v: Vec[T], delta: T) = new BeCloseToVec[T](v, delta)
}