/**
  * This code is copied from https://github.com/denisrosset/meta
  * The MIT License (MIT)
  * =====================
  *
  * Copyright (c) 2015 Denis Rosset
  * Hash set and hash map implementations based on code (c) 2012-2014 Eirk Osheim
  *
  * Permission is hereby granted, free of charge, to any person obtaining a copy of
  * this software and associated documentation files (the "Software"), to deal in
  * the Software without restriction, including without limitation the rights to
  * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
  * of the Software, and to permit persons to whom the Software is furnished to do
  * so, subject to the following conditions:
  *
  * The above copyright notice and this permission notice shall be included in all
  * copies or substantial portions of the Software.
  *
  * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  * SOFTWARE.
  */
package org.saddle

import scala.reflect.ClassTag

final class Buffer[@specialized V](var array: Array[V], var length: Int)(
    implicit val ctV: ClassTag[V]
) {

  final def toArray: Array[V] = {
    val res = ctV.newArray(length.toInt)
    Array.copy(array, 0, res, 0, length)
    res
  }

  def +=(elem: V): this.type = {
    ensureLength(length + 1)
    array(length.toInt) = elem
    length += 1
    this
  }

  /** Grow if necessary the underlying array to accomodate at least n elements. */
  def ensureLength(n: Long): Buffer.Dummy[V] = {
    val arrayLength: Long = array.length
    if (n > arrayLength) {
      var newLength: Long = spire.math.max(arrayLength.toLong * 2, 1)
      while (n > newLength) newLength = newLength * 2
      if (newLength > Int.MaxValue) newLength = Int.MaxValue
      val newArray = ctV.newArray(newLength.toInt)
      Array.copy(array, 0, newArray, 0, length.toInt)
      array = newArray
    }
    null
  }

}

object Buffer {

  class Dummy[@specialized A]

  object Dummy {

    implicit def apply[@specialized A]: Dummy[A] = null
  }

  val startSize = 8

  val INIT_CAPACITY = startSize

  def empty[T: ClassTag] = new Buffer(new Array[T](startSize), 0)

}
