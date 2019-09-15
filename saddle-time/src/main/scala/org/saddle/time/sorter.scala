package org.saddle.time

import org.joda.time._

import org.saddle.array.Sorter
import org.saddle.array
import org.saddle.scalar.ScalarTagTime
import it.unimi.dsi.fastutil.longs.LongArrays

object timeSorter extends Sorter[DateTime] {
  def argSorted(arr: Array[DateTime]) = {
    val res = array.range(0, arr.length)
    LongArrays.radixSortIndirect(res, ScalarTagTime.time2LongArray(arr), true)
    res
  }

  def sorted(arr: Array[DateTime]) = {
    array.take(arr, argSorted(arr), ScalarTagTime.missing)
  }
}