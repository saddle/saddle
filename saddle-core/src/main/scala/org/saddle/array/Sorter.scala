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
package org.saddle.array

import org.saddle.vec.VecBool
import org.saddle.ORD
import spire.std.byte._
import spire.std.int._
import spire.std.long._
import spire.std.double._
import spire.std.char._
import spire.std.short._
import spire.std.float._
import spire.math.MergeSort

/**
  * Typeclass interface for sorting implementations
  */
trait Sorter[T] {
  def argSorted(arr: Array[T]): Array[Int]
  def sorted(arr: Array[T]): Array[T]
}

object Sorter {
  object boolSorter extends Sorter[Boolean] {
    def argSorted(arr: Array[Boolean]) = VecBool.argSort(arr)
    def sorted(arr: Array[Boolean]) = VecBool.sort(arr)
  }

  object byteSorter extends Sorter[Byte] {
    def argSorted(arr: Array[Byte]) = {
      val res = range(0, arr.length)
      PermuteMergeSort.sort(arr, res)
      res
    }

    def sorted(arr: Array[Byte]) = {
      val res = arr.clone()
      MergeSort.sort(res)
      res
    }
  }

  object charSorter extends Sorter[Char] {
    def argSorted(arr: Array[Char]) = {
      val res = range(0, arr.length)
      PermuteMergeSort.sort(arr, res)
      res
    }

    def sorted(arr: Array[Char]) = {
      val res = arr.clone()
      MergeSort.sort(res)
      res
    }
  }

  object shortSorter extends Sorter[Short] {
    def argSorted(arr: Array[Short]) = {
      val res = range(0, arr.length)
      PermuteMergeSort.sort(arr, res)
      res
    }

    def sorted(arr: Array[Short]) = {
      val res = arr.clone()
      MergeSort.sort(res)
      res
    }
  }

  object intSorter extends Sorter[Int] {
    def argSorted(arr: Array[Int]) = {
      val res = range(0, arr.length)
      PermuteMergeSort.sort(arr, res)
      res
    }

    def sorted(arr: Array[Int]) = {
      val res = arr.clone()
      MergeSort.sort(res)
      res
    }
  }

  object floatSorter extends Sorter[Float] {
    def argSorted(arr: Array[Float]) = {
      val tmp = nanToNegInf(arr) // fastutil sorts NaN to PosInf
      val res = range(0, arr.length)
      PermuteMergeSort.sort(tmp, res)
      res
    }

    def sorted(arr: Array[Float]) = {
      val res = nanToNegInf(arr)
      MergeSort.sort(res)
      res
    }
  }

  object longSorter extends Sorter[Long] {
    def argSorted(arr: Array[Long]) = {
      val res = range(0, arr.length)
      PermuteMergeSort.sort(arr, res)
      res
    }

    def sorted(arr: Array[Long]) = {
      val res = arr.clone()
      MergeSort.sort(res)
      res
    }
  }

  object doubleSorter extends Sorter[Double] {
    def argSorted(arr: Array[Double]) = {
      val tmp = nanToNegInf(arr) // fastutil sorts NaN to PosInf
      val res = range(0, arr.length)
      PermuteMergeSort.sort(tmp, res)
      res
    }

    def sorted(arr: Array[Double]) = {
      val res = nanToNegInf(arr)
      MergeSort.sort(res)
      res
    }
  }

  private def nanToNegInf(arr: Array[Double]): Array[Double] = {
    val tmp = arr.clone()
    var i = 0
    while (i < tmp.length) {
      val ti = tmp(i)
      if (ti != ti) tmp(i) = Double.NegativeInfinity
      i += 1
    }
    tmp
  }

  private def nanToNegInf(arr: Array[Float]): Array[Float] = {
    val tmp = arr.clone()
    var i = 0
    while (i < tmp.length) {
      val ti = tmp(i)
      if (ti != ti) tmp(i) = Float.NegativeInfinity
      i += 1
    }
    tmp
  }

  def anySorter[T: ORD] = new Sorter[T] {
    def argSorted(arr: Array[T]) = {
      val res = range(0, arr.length)
      val cmp = implicitly[ORD[T]]
      res.sortWith((a, b) => cmp.compare(arr(a), arr(b)) < 0)
    }

    def sorted(arr: Array[T]) = {
      val res = arr.clone()
      res.sorted
    }
  }
}
