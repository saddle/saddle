/** The code in this file is adapted from spire (https://github.com/typelevel/spire)
  * Spire is Copyright (c) 2011-2012 Erik Osheim, Tom Switzer
  * and is released under MIT license.
  * 
  * Modifications:
  *  - add index sorters (arg sort, permutation index)
  * 
  */
package org.saddle.array 

import spire.algebra.Order
import scala.reflect.ClassTag
import scala.{specialized => sp}

/**
 * An implementation of insertion sort.
 *
 * Works well for small arrays but due to quadratic complexity is not generally optimal.
 */
object PermuteInsertionSort  {

  /**
    * Uses insertion sort on `data` to sort the entries from the index `start`
    * up to, but not including, the index `end`. 
    * Permutation indices are returned in `perm`: data[perm[k]] is the k-th smallest elem in data
    * `data` is not modified.
    * Does not allocate, except on stack.
    *
    * @param data the array to be sorted
    * @param start the index of the first element, inclusive, to be sorted
    * @param end the index of the last element, exclusive, to be sorted
    * @param perm array of permutation indices
    * @tparam A a member of the type class `Order`
    */
  final def sort[@sp A](data:Array[A], start:Int, end:Int, perm: Array[Int])(implicit o:Order[A]): Unit = {
    require(start <= end && start >= 0 && end <= data.length && end <= perm.length)
    if (perm.isEmpty) ()
    else {
      perm(start) = start
      var i = start + 1
      while (i < end) {
        val item = data(i)
        var hole = i 
        perm(hole) = hole
        while (hole > start && o.gt(data(perm(hole - 1)), item)) {
          val swap = perm(hole)
          perm(hole) = perm(hole - 1)
          perm(hole - 1 ) = swap
          hole -= 1
        }
        i += 1
      }
    }
  }
}

/**
 * In-place merge sort implementation. This sort is stable but does mutate
 * the given array. It is an in-place sort but it does allocate a temporary
 * array of the same size as the input. It uses InsertionSort for sorting very
 * small arrays.
 */
object PermuteMergeSort{
  @inline final def startWidth: Int = 8
  @inline final def startStep: Int = 16

  final def sort[@sp A:Order:ClassTag](data:Array[A], perm: Array[Int]): Unit = {
    val len = data.length

    if (len <= startStep) {
      PermuteInsertionSort.sort(data, 0, len, perm)
      return
    }

    var buf1:Array[Int] = perm
    var buf2:Array[Int] = new Array[Int](len)
    var tmp:Array[Int] = null

    var i = 0
    var limit = len - startWidth
    while (i < limit) { PermuteInsertionSort.sort(data, i, i + startWidth, perm); i += startWidth }
    if (i < len) PermuteInsertionSort.sort(data, i, len, perm)
    var width = startWidth
    var step = startStep
    while (width < len) {
      i = 0
      limit = len - step
      while (i < limit) {
        mergePerm(data,buf1, buf2, i, i + width, i + step); i += step
      }
      while (i < len) {
        mergePerm(data,buf1, buf2, i, math.min(i + width, len), len); i += step
      }
      tmp = buf2
      buf2 = buf1
      buf1 = tmp

      width *= 2
      step *= 2
    }

    if (!buf1.eq(perm)) System.arraycopy(buf1, 0, perm, 0, len)
  }

  @inline final def mergePerm[@sp A](data:Array[A], in:Array[Int], out:Array[Int], start:Int, mid:Int, end:Int)(implicit o:Order[A]): Unit = {
    require(start >= 0 && start <= mid && mid <= end && end <= in.length && end <= out.length)
    var ii = start
    var jj = mid
    var kk = start
    while (kk < end) {
      if (ii < mid && (jj >= end || o.lteqv(data(in(ii)), data(in(jj))))) {
        out(kk) = in(ii); ii += 1
      } else {
        out(kk) = in(jj); jj += 1
      }
      kk += 1
    }
  }
}