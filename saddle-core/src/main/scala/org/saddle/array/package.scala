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
package org.saddle

import scala.{specialized => spec}
import spire.random.rng.Well19937c

/**
  * This package contains utilities for working with arrays that
  * are specialized for numeric types.
  */
package object array {
  import spire.random.Generator

  /**
    * Create a new array consisting of a range of numbers from a lower bound up to, but
    * not including, an upper bound, at a particular increment (default 1)
    */
  def range(from: Int, until: Int, step: Int = 1): Array[Int] = {
    if (from >= until) Array.ofDim[Int](0)
    else {
      val sz = org.saddle.util.dividePositiveRoundUp(until - from, step)
      var i = from
      var k = 0
      val arr = Array.ofDim[Int](sz)
      while (k < sz) {
        arr(k) = i
        k += 1
        i += step
      }
      arr
    }
  }

  /**
    * Create a new initialized empty array
    */
  def empty[@spec(Boolean, Int, Long, Double) T: ST](len: Int): Array[T] =
    Array.ofDim[T](len)

  /**
    * Return a uniform random permutation of the array
    */
  def shuffle[@spec(Boolean, Int, Long, Double) T: ST](
      arr: Array[T],
      rng: Generator
  ): Array[T] = {
    var i = 0
    val sz = arr.length
    val result = arr.clone()
    while (i < sz) {
      // maintains the invariant that at position i in result, all items to the left of i
      // have been randomly selected from the remaining sz - i locations
      val loc = i + math.floor((sz - i) * rng.nextDouble).toInt
      val tmp = result(i)
      result(i) = result(loc)
      result(loc) = tmp
      i += 1
    }
    result
  }

  /**
    * Return a uniform random permutation of the array
    */
  def shuffle[@spec(Boolean, Int, Long, Double) T: ST](
      arr: Array[T]
  ): Array[T] = shuffle(arr, defaultRandom)

  /**
    * Repeat elements of the array some number of times
    */
  def tile[@spec(Boolean, Int, Long, Double) T: ST](
      arr: Array[T],
      n: Int
  ): Array[T] = {
    require(n >= 0, "n must not be negative")
    val sz = arr.length * n
    val res = empty[T](sz)
    var i = 0
    while (i < n) {
      System.arraycopy(arr, 0, res, i * arr.length, arr.length)
      i += 1
    }
    res
  }

  // *** random number generators
  private val defaultRandom = Well19937c.fromTime()

  /**
    * Generate an array of random integers
    */
  def randInt(sz: Int, rng: Generator): Array[Int] = {
    val arr = Array.ofDim[Int](sz)
    rng.fillInts(arr)
    arr
  }

  /**
    * Generate an array of random integers
    */
  def randInt(sz: Int): Array[Int] = randInt(sz, defaultRandom)

  /**
    * Generate an array of random integers in [from,to]
    */
  def randInt(sz: Int, from: Int, to: Int, rng: Generator): Array[Int] = {
    val arr = Array.ofDim[Int](sz)
    var i = 0
    while (i < sz) {
      arr(i) = rng.nextInt(from, to)
      i += 1
    }
    arr
  }

  /**
    * Generate an array of random integers in [from,to]
    */
  def randInt(sz: Int, from: Int, to: Int): Array[Int] =
    randInt(sz, from, to, defaultRandom)

  /**
    * Generate an array of a random long integers
    */
  def randLong(sz: Int, rng: Generator): Array[Long] = {
    val arr = Array.ofDim[Long](sz)
    rng.fillLongs(arr)
    arr
  }

  /**
    * Generate an array of a random long integers
    */
  def randLong(sz: Int): Array[Long] =
    randLong(sz, defaultRandom)

  /**
    * Generate an array of a random long integers in [from,to]
    */
  def randLong(sz: Int, from: Long, to: Long, rng: Generator): Array[Long] = {
    val arr = Array.ofDim[Long](sz)
    var i = 0
    while (i < sz) {
      arr(i) = rng.nextLong(from, to)
      i += 1
    }
    arr
  }

  /**
    * Generate an array of a random long integers in [from,to]
    */
  def randLong(sz: Int, from: Long, to: Long): Array[Long] =
    randLong(sz, from, to, defaultRandom)

  /**
    * Generate an array of random doubles on [0,1)
    */
  def randDouble(sz: Int, rng: Generator): Array[Double] = {
    val arr = Array.ofDim[Double](sz)
    var i = 0
    while (i < sz) {
      arr(i) = rng.nextDouble
      i += 1
    }
    arr
  }

  /**
    * Generate an array of random doubles on [0,1)
    */
  def randDouble(sz: Int): Array[Double] =
    randDouble(sz, defaultRandom)

  /**
    * Generate an array of random doubles on [0,n)
    */
  def randDouble(sz: Int, n: Double, rng: Generator): Array[Double] = {
    val arr = Array.ofDim[Double](sz)
    var i = 0
    while (i < sz) {
      arr(i) = rng.nextDouble(n)
      i += 1
    }
    arr
  }

  /**
    * Generate an array of random doubles on [0,n)
    */
  def randDouble(sz: Int, n: Double): Array[Double] =
    randDouble(sz, n, defaultRandom)

  /**
    * Generate an array of random doubles on [min,until)
    */
  def randDouble(
      sz: Int,
      min: Double,
      until: Double,
      rng: Generator
  ): Array[Double] = {
    val arr = Array.ofDim[Double](sz)
    var i = 0
    while (i < sz) {
      arr(i) = rng.nextDouble(min, until)
      i += 1
    }
    arr
  }

  /**
    * Generate an array of random doubles on [min,until)
    */
  def randDouble(sz: Int, min: Double, until: Double): Array[Double] =
    randDouble(sz, min, until, defaultRandom)

  /**
    * Generate an array of random positive integers
    */
  def randIntPos(sz: Int, rng: Generator): Array[Int] = {
    val arr = Array.ofDim[Int](sz)
    var i = 0
    while (i < sz) {
      arr(i) = rng.nextInt(1, Int.MaxValue)
      i += 1
    }
    arr
  }

  /**
    * Generate an array of random positive integers
    */
  def randIntPos(sz: Int): Array[Int] =
    randIntPos(sz, defaultRandom)

  /**
    * Generate an array of random long positive integers
    */
  def randLongPos(sz: Int, rng: Generator): Array[Long] = {
    val arr = Array.ofDim[Long](sz)
    var i = 0
    while (i < sz) {
      arr(i) = rng.nextLong(1L, Long.MaxValue)
      i += 1
    }
    arr
  }

  /**
    * Generate an array of random long positive integers
    */
  def randLongPos(sz: Int): Array[Long] =
    randLongPos(sz, defaultRandom)

  /**
    * Generate an array of random positive doubles on (0, 1]
    */
  def randDoublePos(sz: Int, rng: Generator): Array[Double] = {
    val arr = Array.ofDim[Double](sz)
    var i = 0
    while (i < sz) {
      arr(i) = 1d - rng.nextDouble
      i += 1
    }
    arr
  }

  /**
    * Generate an array of random positive doubles on (0, 1]
    */
  def randDoublePos(sz: Int): Array[Double] =
    randDoublePos(sz, defaultRandom)

  /**
    * Generate an array of random doubles which is normally distributed
    * with a mean of zero and stdev of one.
    */
  def randNormal(sz: Int, rng: Generator): Array[Double] = {
    val arr = Array.ofDim[Double](sz)
    rng.fillGaussians(arr)
    arr
  }

  /**
    * Generate an array of random doubles which is normally distributed
    * with a mean of zero and stdev of one.
    */
  def randNormal(sz: Int): Array[Double] =
    randNormal(sz, defaultRandom)

  /**
    * Generate an array of random doubles which is normally distributed
    * with a mean of mu and stdev of sigma.
    */
  def randNormal2(
      sz: Int,
      mu: Double,
      sigma: Double,
      rng: Generator
  ): Array[Double] = {
    val arr = Array.ofDim[Double](sz)
    var i = 0
    while (i < sz) {
      arr(i) = rng.nextGaussian(mu, sigma)
      i += 1
    }
    arr
  }

  /**
    * Generate an array of random doubles which is normally distributed
    * with a mean of mu and stdev of sigma.
    */
  def randNormal2(sz: Int, mu: Double, sigma: Double): Array[Double] =
    randNormal2(sz, mu, sigma, defaultRandom)

  /**
    * Takes values from array arr at particular offsets so as to produce a new array.
    * Offset -1 is mapped to by-name parameter `missing`.
    *
    * Note that each integer I at offset O in `offsets` works to "take" input[I] to
    * output[O]. Eg, Array(2,0,1) permutes locations as follows:
    *
    *  - 2 to 0
    *  - 0 to 1
    *  - 1 to 2
    *
    * For example,
    *
    * {{{
    *   take(Array(5,6,7), Array(2,0,1), -1) == Array(7,5,6)
    * }}}
    */
  def take[@spec(Boolean, Int, Long, Double) T: ST](
      arr: Array[T],
      offsets: Array[Int],
      missing: => T
  ): Array[T] = {
    val res = empty[T](offsets.length)
    var i = 0
    while (i < offsets.length) {
      val idx = offsets(i)
      if (idx == -1)
        res(i) = missing
      else
        res(i) = arr(idx)
      i += 1
    }
    res
  }

  /**
    * Takes values from array arr at particular offsets so as to produce a new array.
    * Offset -1 is mapped to by-name parameter `missing`.
    *
    * Note that each integer I at offset O in `offsets` works to "take" input[I] to
    * output[O]. Eg, Array(2,0,1) permutes locations as follows:
    *
    *  - 2 to 0
    *  - 0 to 1
    *  - 1 to 2
    *
    * For example,
    *
    * {{{
    *   take(Array(5,6,7), Array(2,0,1), -1) == Array(7,5,6)
    * }}}
    */
  def take[@spec(Boolean, Int, Long, Double) T: ST](
      arr: Array[T],
      offsets: Vec[Int],
      missing: => T
  ): Array[T] = take(arr, offsets.toArray, missing)

  /**
    * Compute the sum of the array at particular offets. If any of the offets is -1,
    * the pass-by-name value 'missing' is used instead.
    *
    * For example,
    *
    * {{{
    *   sum(Array(1,2,3,4), Array(0,2,), 0)
    * }}}
    */
  def sum[@spec(Boolean, Int, Long, Double) T: ST: NUM](
      arr: Array[T],
      offsets: Array[Int],
      missing: => T
  ): T = {
    val st = implicitly[ST[T]]
    val nm = implicitly[NUM[T]]
    var res = st.zero(nm)
    var i = 0
    while (i < offsets.length) {
      val idx = offsets(i)
      res = if (idx == -1) nm.plus(res, missing) else nm.plus(res, arr(idx))
      i += 1
    }
    res
  }

  /**
    * Sends values from an array to particular offsets so as to produce a new array.
    * This does the inverse of 'take'; ie, each integer I at offset O in `offsets`
    * works to "send" input[O] to output[I]. Eg, Array(2,0,1) permutes locations as
    * follows:
    *
    *   - 0 to 2
    *   - 1 to 0
    *   - 2 to 1
    *
    * For example,
    *
    * {{{
    *   send(Array(5,6,7), Array(2,0,1)) == Array(6,7,5)
    * }}}
    */
  def send[@spec(Boolean, Int, Long, Double) T: ST](
      arr: Array[T],
      offsets: Array[Int]
  ): Array[T] = {
    val res = empty[T](offsets.length)
    var i = 0
    while (i < offsets.length) {
      res(offsets(i)) = arr(i)
      i += 1
    }
    res
  }

  /**
    * Remove values from array arr at particular offsets so as to
    * produce a new array.
    */
  def remove[@spec(Boolean, Int, Long, Double) T: ST](
      arr: Array[T],
      locs: Array[Int]
  ): Array[T] = {
    val set = new util.IntMap
    var i = 0
    while (i < locs.length) {
      val loc = locs(i)
      if (loc >= 0 && loc < arr.length) set.update(loc, 0)
      i += 1
    }

    val len = arr.length - set.size
    val res = empty[T](len)

    i = 0
    var k = 0
    while (i < arr.length) {
      if (!set.contains(i)) {
        res(k) = arr(i)
        k += 1
      }
      i += 1
    }

    res
  }

  /**
    * Put a single value into array arr at particular offsets, so as to produce a new array.
    */
  def put[@spec(Boolean, Int, Long, Double) T](
      arr: Array[T],
      offsets: Array[Int],
      value: T
  ): Array[T] = {
    val res = arr.clone()
    var i = 0
    while (i < offsets.length) {
      val idx = offsets(i)
      res(idx) = value
      i += 1
    }
    res
  }

  /**
    * Put a value into array arr at particular offsets provided by a boolean array where its locations
    * are true, so as to produce a new array.
    */
  def put[@spec(Boolean, Int, Long, Double) T](
      arr: Array[T],
      offsets: Array[Boolean],
      value: T
  ): Array[T] = {
    val res = arr.clone()
    var i = 0
    while (i < offsets.length) {
      if (offsets(i)) res(i) = value
      i += 1
    }
    res
  }

  /**
    * Put n values into array arr at particular offsets, where the values come from another array,
    * so as to produce a new array.
    */
  def putn[@spec(Boolean, Int, Long, Double) T](
      arr: Array[T],
      offsets: Array[Int],
      values: Array[T]
  ): Array[T] = {
    val res = arr.clone()
    var i = 0
    while (i < offsets.length) {
      val idx = offsets(i)
      res(idx) = values(i)
      i += 1
    }
    res
  }

  /**
    * Fill array with value
    */
  def fill[@spec(Boolean, Int, Long, Double) T: ST](arr: Array[T], v: T) = {
    var i = 0
    while (i < arr.length) {
      arr(i) = v
      i += 1
    }
  }

  /**
    * Derived from numpy 1.7
    *
    * Return evenly spaced numbers over a specified interval.
    *
    * Returns num evenly spaced samples, calculated over the
    * interval [start, stop].
    *
    * If start < stop, then the order of elements are decreasing.
    *
    * The endpoint of the interval can optionally be excluded.
    */
  def linspace(
      start: Double,
      stop: Double,
      num: Int = 50,
      endpoint: Boolean = true
  ): Array[Double] = {
    if (num <= 0)
      Array.empty[Double]
    else if (num == 1)
      Array(start)
    else {
      val result = Array.ofDim[Double](num)
      val step = (stop - start) / (num - (if (endpoint) 1 else 0))

      val n = num
      var i = 0
      while (i < n) {
        result(i) = start + i * step
        i += 1
      }
      result
    }
  }

  /**
    * Stable indirect sort resulting in permutation of numbers [0, n), whose application
    * on an array results in a sorted array.
    *
    * @param arr Array to sort
    */
  def argsort[@spec(Double, Long, Int, Float) T: ORD](
      arr: Array[T]
  )(implicit st: ST[T]): Array[Int] =
    st.makeSorter.argSorted(arr)

  def argsort[@spec(Double, Long, Int, Float) T: ST: ORD](
      vec: Vec[T]
  ): Array[Int] = argsort(vec.toArray)

  /**
    * Stable sort of array argument (not destructive), using radix sort
    * implementation wherever possible.
    *
    * @param arr Array to sort
    */
  def sort[T: ST: ORD](arr: Array[T]): Array[T] =
    implicitly[ST[T]].makeSorter.sorted(arr)

  /**
    * Reverse an array
    */
  def reverse[@spec(Boolean, Int, Long, Double) T: ST](
      arr: Array[T]
  ): Array[T] = {
    val end = arr.length - 1
    val newArr = new Array[T](end + 1)

    var i = 0
    while (i <= end) {
      newArr(i) = arr(end - i)
      i += 1
    }
    newArr
  }

  /**
    * Filter an array based on a predicate function, wherever that predicate is true
    */
  def filter[@spec(Boolean, Int, Long, Double) T: ST](
      f: T => Boolean
  )(arr: Array[T]): Array[T] = {
    var i = 0
    var count = 0
    while (i < arr.length) {
      val v = arr(i)
      if (f(v)) count += 1
      i += 1
    }
    if (count == arr.length) arr
    else {
      val res = empty[T](count)
      i = 0
      count = 0
      while (i < arr.length) {
        val v = arr(i)
        if (f(v)) {
          res(count) = v
          count += 1
        }
        i += 1
      }
      res
    }
  }

  /**
    * Flatten a sequence of arrays into a single array
    */
  def flatten[@spec(Boolean, Int, Long, Double) T: ST](
      arrs: Seq[Array[T]]
  ): Array[T] = {
    val size = arrs.map(_.length).sum
    val newArr = new Array[T](size)
    var i = 0

    arrs.foreach { a =>
      val l = a.length
      System.arraycopy(a, 0, newArr, i, l)
      i += l
    }

    newArr
  }

  /**
    * Return the integer offset of the minimum element, or -1 for an empty array
    */
  def argmin[@spec(Int, Long, Double) T: ST: ORD: NUM](arr: Array[T]): Int = {
    val sca = implicitly[ST[T]]
    val sz = arr.length
    if (sz == 0) -1
    else {
      var (min, arg) = if (sca.isMissing(arr(0))) (sca.inf, -1) else (arr(0), 0)
      var i = 1
      while (i < sz) {
        val v = arr(i)
        if (sca.notMissing(v) && sca.compare(min, v) == 1) {
          min = arr(i)
          arg = i
        }
        i += 1
      }
      arg
    }
  }

  /**
    * Return the integer offset of the maximum element, or -1 for an empty array
    */
  def argmax[@spec(Int, Long, Double) T: ST: ORD: NUM](arr: Array[T]): Int = {
    val sca = implicitly[ST[T]]
    val sz = arr.length
    if (sz == 0) -1
    else {
      var (max, arg) =
        if (sca.isMissing(arr(0))) (sca.negInf, -1) else (arr(0), 0)
      var i = 1
      while (i < sz) {
        val v = arr(i)
        if (sca.notMissing(v) && sca.compare(v, max) == 1) {
          max = arr(i)
          arg = i
        }
        i += 1
      }
      arg
    }
  }
}
