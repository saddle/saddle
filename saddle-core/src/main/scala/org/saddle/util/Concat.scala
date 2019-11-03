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
package org.saddle.util

import scala.{specialized => spec}
import org.saddle.{ST, array}

/**
  * Provides a way to append two arrays of possibly different types together by
  * intelligently promoting primitive types where possible.
  *
  * Key method is Concat.append(array1, array2)
  */
object Concat {

  /**
    * Append two arrays of possibly different types together by intelligently
    * promoting primitive types where possible.
    *
    * @param a1 First array
    * @param a2 Second array
    * @param wd Evidence of instance of Promoter for involved types
    * @param mc Evidence of ST for result type
    * @tparam A First array type
    * @tparam B Second array type
    * @tparam C Result array type
    */
  def append[
      @spec(Boolean, Byte, Int, Long, Double, Float) A
  ](
      a1: Array[A],
      a2: Array[A]
  )(implicit st: ST[A]): Array[A] = {
    val result = array.empty[A](a1.length + a2.length)
    System.arraycopy(a1, 0, result, 0, a1.length)
    System.arraycopy(a2, 0, result, a1.length, a2.length)
    result
  }
}
