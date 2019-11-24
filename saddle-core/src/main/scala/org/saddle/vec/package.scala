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

/**
  * Factory methods to generate Vec instances
  */
package object vec {

  /**
    * Generate a uniform random Vec[Long] of a certain number of elements
    * @param sz Number of elements of random vector
    */
  def randl(sz: Int): Vec[Long] = Vec(array.randLong(sz))

  /**
    * Generate a uniform random positive Vec[Long] of a certain number of
    * elements
    * @param sz Number of elements of random vector
    */
  def randpl(sz: Int): Vec[Long] = Vec(array.randLongPos(sz))

  /**
    * Generate a uniform random Vec[Int] of a certain number of elements
    * @param sz Number of elements of random vector
    */
  def randi(sz: Int): Vec[Int] = Vec(array.randInt(sz))

  /**
    * Generate a uniform random positive Vec[Int] of a certain number of
    * elements
    * @param sz Number of elements of random vector
    */
  def randpi(sz: Int): Vec[Int] = Vec(array.randIntPos(sz))

  /**
    * Generate a uniform [0,1) random Vec[Double] of a certain number of elements
    * @param sz Number of elements of random vector
    */
  def rand(sz: Int): Vec[Double] = Vec(array.randDouble(sz))

  /**
    * Generate a uniform (0,1] random  Vec[Double] of a certain number of
    * elements
    * @param sz Number of elements of random vector
    */
  def randp(sz: Int): Vec[Double] = Vec(array.randDoublePos(sz))

  /**
    * Generate a standard normal random Vec[Double] of a certain number of
    * elements
    * @param sz Number of elements of random vector
    */
  def randn(sz: Int): Vec[Double] = Vec(array.randNormal(sz))

  /**
    * Generate a gaussian(mu, sigma) random Vec[Double] of a certain number of
    * elements
    * @param sz Number of elements of random vector
    * @param mu Mean of distribution
    * @param sigma Stdev of distribution
    */
  def randn2(sz: Int, mu: Double, sigma: Double): Vec[Double] =
    Vec(array.randNormal2(sz, mu, sigma))

  /**
    * Generate a Vec[Double] containing a certain number of ones
    * @param sz Number of elements of ones vec
    */
  def ones(sz: Int): Vec[Double] = {
    val tmp = Array.ofDim[Double](sz)
    array.fill(tmp, 1.0)
    Vec(tmp)
  }

  /**
    * Generate a Vec[Double] containing a certain number of zeros
    * @param sz Number of elements of zero vec
    */
  def zeros(sz: Int): Vec[Double] = Vec(array.empty[Double](sz))

  /**
    * Generate a Vec[Int] containing the range of integers specified
    * @param from Start of range
    * @param until End of range, excluded from result
    * @param step Stride of range
    */
  def range(from: Int, until: Int, step: Int = 1): Vec[Int] =
    Vec(array.range(from, until, step))

  /**
    * Repeats a particular array some number of times
    *
    * @param v array of values to repeat
    * @param n number of repetitions
    * @tparam T type of elements in array
    */
  def repeat[@spec(Boolean, Int, Long, Double) T: ST](
      v: Array[T],
      n: Int
  ): Array[T] = {
    array.flatten(for (_ <- 1 to n) yield v)
  }
}
