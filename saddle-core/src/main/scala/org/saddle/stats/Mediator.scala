package org.saddle.stats

/**
 * Mediator is an auxiliary class for O(N log k) rolling median. It is inspired by
 * AShelly's C99 implementation, which is (c) 2011 ashelly.myopenid.com and licensed
 * under the MIT license: http://www.opensource.org/licenses/mit-license
 *
 * Reference:
 *   http://stackoverflow.com/questions/5527437/rolling-median-in-c-turlach-implementation
 */
class Mediator(winSz: Int) {
  require(winSz > 0, "Window length must be > 0!")

  // auxiliary data
  private val data = Array.ofDim[Double](winSz)   // circular buffer of values
  private val loc = Array.ofDim[Int](winSz)       // ith value's location within heap array
  private val heap = Array.ofDim[Int](winSz)      // orders data array into [max heap] :: median :: [min heap]
  private val sawNa = Array.ofDim[Boolean](winSz) // circular buffer of na markers
  private var idx = 0                             // position in circular buffer
  private var naIdx = 0                           // position in circular buffer
  private var minCt = 0                           // # items in minheap
  private var maxCt = 0                           // # items in maxheap
  private var totCt = 0                           // # items in data
  private var nanCt = 0                           // count of NA's in current window

  // heap array contains indexes of data array giving a max-mid-min heap structure centered at hMid:
  //   index: [0           ...            hMid           ...      winSz-1]
  //   value: [... | child2 child1 | max] mid  [min | child1 child2 | ...]
  //
  // such that data(heap(max)) <= data(heap(hMid)) <= data(heap(min))
  //
  // also, we maintain invariants:
  //   (a) size(minheap) <= size(maxheap)
  //   (b) size(minheap) >= size(maxheap) - 1

  private val hMid = winSz / 2   // heap(hMid) = x s.t. data(x) holds mid (between max/min heaps)

  // loc array is a reverse lookup for data into the heap, eg:
  //   loc(n) = -2  ==>  data(n) is maxheap child1
  //   loc(n) = +1  ==>  data(n) is minheap min

  // initialize auxiliary data
  private var i = 0
  while (i < winSz) {
    data(i) = Double.NaN
    i += 1
  }

  def median: Double = {
    val v = data(heap(hMid))
    if ((totCt & 1) == 0)
      (v + data(heap(hMid-1))) / 2.0
    else
      v
  }

  def push(v: Double) {
    val oldNa = sawNa(naIdx)

    if (v != v) {
      // observing na
      sawNa(naIdx) = true
      if (!oldNa) nanCt += 1
    }
    else {
      // observing real value
      sawNa(naIdx) = false
      if (oldNa) nanCt -= 1

      insert(v)

      if (totCt < winSz) totCt += 1
    }

    if (totCt + nanCt > winSz) {
      pop()
    }

    naIdx = (naIdx + 1) % winSz
  }

  def pop() {
    // get location of least recently inserted value
    val l = (idx - totCt + winSz) % winSz
    val p = loc(l)

    if (totCt > 0) {
      if (totCt == 1) {
        // don't need to do anything
      }
      else if (p > 0) {
        // item is in minheap
        swap(p, minCt)
        minCt -= 1
        minSortDown(p)
        if (minCt < maxCt - 1) maxToMin()
      }
      else if (p < 0) {
        // item is in maxheap
        swap(-maxCt, p)
        maxCt -= 1
        maxSortDown(p)
        if (maxCt < minCt) minToMax()
      }
      else {
        // item is mid
        if (maxCt > minCt) {
          // swap head of maxheap with mid
          swap(-1, 0)
          // drop head of maxheap
          swap(-maxCt, -1)
          maxCt -= 1
          maxSortDown(-1)
        }
        else {
          // swap head of minheap with mid
          swap(0, 1)
          // drop head of minheap
          swap(1, minCt)
          minCt -= 1
          minSortDown(1)
        }
      }

      totCt -= 1

      // must null out this value
      data(l) = Double.NaN
    }
  }

  // returns true if heap[i] < heap[j]
  private def isless(i: Int, j: Int): Boolean =
    data(heap(i + hMid)) < data(heap(j + hMid))

  // swaps items i & j in heap, maintains indexes
  private def swap(i: Int, j: Int) {
    val iOff = i + hMid
    val jOff = j + hMid
    val t = heap(iOff)
    heap(iOff) = heap(jOff)
    heap(jOff) = t
    loc(heap(iOff)) = i
    loc(heap(jOff)) = j
  }

  // swaps items i & j if i < j; returns true if swapped
  private def cas(i: Int, j: Int): Boolean = {
    if (isless(i, j)) {
      swap(i, j)
      true
    } else false
  }

  // maintains minheap property for all items below i in heap
  private def minSortDown(iIn: Int) {
    var i = iIn * 2
    while (i <= minCt) {
      if (i < minCt && isless(i+1, i)) { i += 1 }
      if (!cas(i, i / 2))
        i = minCt + 1 // break
      else
        i *= 2
    }
  }

  // maintains maxheap property for all items below i in heap
  private def maxSortDown(iIn: Int) {
    var i = iIn * 2
    while (i >= -maxCt) {
      if (i > -maxCt && isless(i, i-1)) { i -= 1 }
      if (!cas(i/2, i))
        i = -(maxCt+1) // break
      else
        i *= 2
    }
  }

  // maintains minheap property for all items above i in heap, including median
  // returns true if median changed
  private def minSortUp(iIn: Int): Boolean = {
    var i = iIn
    while (i > 0 && cas(i, i / 2)) i /= 2
    i == 0
  }

  // maintains maxheap property for all items above i in heap, including median
  // returns true if median changed
  private def maxSortUp(iIn: Int): Boolean = {
    var i = iIn
    while (i < 0 && cas(i / 2, i)) i /= 2
    i == 0
  }

  // rebalance toward maxheap
  private def minToMax() {
    maxCt += 1                      // make room on maxheap
    swap(minCt, -maxCt)             // swap element from minheap
    minCt -= 1
    if (maxSortUp(-maxCt) && (minCt != 0) && cas(1, 0)) { minSortDown(1) }
  }

  // rebalance toward minheap
  private def maxToMin() {
    minCt += 1                      // make room on minheap
    swap(-maxCt, minCt)             // swap element from maxheap
    maxCt -=1
    if (minSortUp(minCt) && cas(0, -1)) { maxSortDown(-1) }
  }

  private def insert(v: Double) {
    // save old value
    val old = data(idx)

    // store new value
    data(idx) = v

    // first element?
    if (totCt == 0) {
      loc(idx) = 0
      heap(hMid) = idx
    }
    else {
      if (totCt < winSz) {
        // room in buffer
        if (maxCt > minCt) {
          // add to minheap
          minCt += 1
          loc(idx) = minCt
          heap(hMid + minCt) = idx
          if (minSortUp(minCt) && cas(0, -1)) { maxSortDown(-1) }
        }
        else {
          // add to maxheap
          maxCt += 1
          loc(idx) = -maxCt
          heap(hMid - maxCt) = idx
          if (maxSortUp(-maxCt) && (minCt != 0) && cas(1, 0)) { minSortDown(1) }
        }
      }
      else {
        // overwriting old value
        var reSort = true
        val p = loc(idx)

        if (p > 0) {
          // new item was inserted in minheap
          if (minCt < (winSz - 1)/2) { minCt += 1 }
          else if (v > old) { minSortDown(p); reSort = false }
          if (reSort && minSortUp(p) && cas(0, -1)) { maxSortDown(-1) }
        }
        else if (p < 0) {
          // new item was inserted in maxheap
          if (maxCt < winSz / 2) { maxCt += 1 }
          else if (v < old) { maxSortDown(p); reSort = false }
          if (reSort && maxSortUp(p) && (minCt != 0) && cas(1, 0)) { minSortDown(1) }
        }
        else {
          // new item was inserted at median
          if ( (maxCt != 0) && maxSortUp(-1)) { maxSortDown(-1) }
          if ( (minCt != 0) && minSortUp(1) ) { minSortDown(1)  }
        }
      }
    }

    idx = (idx + 1) % winSz
  }

  private def printMaxHeap() {
    if (maxCt > 0)
      print("%6.2f" format data(heap(-1 + hMid)))
    var i = 2
    while (i <= maxCt) {
      print(" |%6.2f " format data(heap(-i + hMid)))
      i += 1
      if (i <= maxCt)
        print("%6.2f" format data(heap(-i + hMid)))
      i += 1
    }
    println("")
  }

  private def printMinHeap() {
    if (minCt > 0)
      print("%6.2f" format data(heap(1 + hMid)))
    var i = 2
    while (i <= minCt) {
      print(" |%6.2f " format data(heap(i + hMid)))
      i += 1
      if (i <= minCt)
        print("%6.2f" format data(heap(i + hMid)))
      i += 1
    }
    println("")
  }

  def debug() {
    println("Med: %6.2f" format median)
    println("Obs: %6d" format totCt)
    println("NAs: %6d" format nanCt)
    println("+H:  %6d" format maxCt)
    println("-H:  %6d" format minCt)
    println("-------------------- DATA LAYOUT --------------------")
    print("Max: "); printMaxHeap()
    println("Mid: %6.2f" format data(heap(hMid)))
    print("Min: "); printMinHeap()
    println("---------------------- ARRAYS -----------------------")
    println("  i |       DATA |       HEAP |         LOC | sawNA |")
    println("-----------------------------------------------------")
    for (i <- 0 until winSz) {
      val star1 = if (i == idx) "*" else " "
      val star2 = if (i == naIdx) " *" else "  "
      println("%s%3d|%12.6f|%12d|%12d |%5s%s|" format(star1, i, data(i), heap(i), loc(i), sawNa(i), star2))
    }
    println("-----------------------------------------------------")
  }
}