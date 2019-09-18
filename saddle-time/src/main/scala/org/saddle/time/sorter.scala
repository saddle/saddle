package org.saddle.time

import org.joda.time._

import org.saddle.array.Sorter
import org.saddle.array
import org.saddle.scalar.ScalarTagTime
import org.saddle.array.PermuteMergeSort
import spire.std.long._

object timeSorter extends Sorter[DateTime] {
  def argSorted(arr: Array[DateTime]) = {
    val res = array.range(0, arr.length)
    PermuteMergeSort.sort(ScalarTagTime.time2LongArray(arr),res)
    res
  }

  def sorted(arr: Array[DateTime]) = {
    array.take(arr, argSorted(arr), ScalarTagTime.missing)
  }
}