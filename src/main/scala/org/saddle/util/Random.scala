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

import annotation.tailrec

/**
 * The Random class provides methods to generate pseudorandom numbers via Marsaglia Xorshift
 * PRNGs, which use simple exclusive-or and bit-shift operations to generate these numbers
 * extremely quickly over long cycles.
 *
 * For instance, nextInt and nextFloat have cycles of length 2^32^ - 1, while nextLong and
 * nextDouble have cycles of length 2^64^ - 1.
 *
 * There are 648 possible Marsiglia PRNG choices for 32 bits, and 2200 choices for 64 bits,
 * corresponding to which triples are utilized in the calculations. One triple is chosen at
 * random via a call to java.util.Random for cycling through the 32-bit state, and similarly
 * one for the 64-bit state.
 *
 * By default, the 64-bit seed is chosen via System.nanoTime, and the 32-bit seed by taking
 * the 32 lower-order bits of the same seed.
 *
 * To create repeatable behavior, seed0 and seed1 may be specified.
 *
 * See [[http://www.jstatsoft.org/v08/i14/ Marsiglia]] for details.
 *
 * @param seed0 Seed for java.util.Random triple selector
 * @param seed1 Seed for initial state of Marsiglia PRNG
 */
class Random(seed0: Option[Int], seed1: Option[Long]) {
  private var seedL: Long = seed1.getOrElse(System.nanoTime())
  private var seedI: Int = (((seedL >> 32) << 32) - seedL).asInstanceOf[Int]

  private val jrand = seed0.map(s => new java.util.Random(s)) getOrElse new java.util.Random()

  private def makeXorShift32( tup: (Int, Int, Int) ): () => Int = {
    val (a, b, c) = tup
    jrand.nextInt(8) match {
      case 0 => { () => seedI ^= seedI << a; seedI ^= seedI >>> b; seedI ^= seedI << c; seedI }
      case 1 => { () => seedI ^= seedI << c; seedI ^= seedI >>> b; seedI ^= seedI << a; seedI }
      case 2 => { () => seedI ^= seedI << a; seedI ^= seedI >>> b; seedI ^= seedI << c; seedI }
      case 3 => { () => seedI ^= seedI << c; seedI ^= seedI >>> b; seedI ^= seedI << a; seedI }
      case 4 => { () => seedI ^= seedI << a; seedI ^= seedI >>> c; seedI ^= seedI << b; seedI }
      case 5 => { () => seedI ^= seedI << c; seedI ^= seedI >>> a; seedI ^= seedI << b; seedI }
      case 6 => { () => seedI ^= seedI << a; seedI ^= seedI >>> c; seedI ^= seedI << b; seedI }
      case _ => { () => seedI ^= seedI << c; seedI ^= seedI >>> a; seedI ^= seedI << b; seedI }
    }
  }

  private def makeXorShift64( tup: (Int, Int, Int) ): () => Long = {
    val (a, b, c) = tup
    jrand.nextInt(8) match {
      case 0 => { () => seedL ^= seedL << a; seedL ^= seedL >>> b; seedL ^= seedL << c; seedL }
      case 1 => { () => seedL ^= seedL << c; seedL ^= seedL >>> b; seedL ^= seedL << a; seedL }
      case 2 => { () => seedL ^= seedL << a; seedL ^= seedL >>> b; seedL ^= seedL << c; seedL }
      case 3 => { () => seedL ^= seedL << c; seedL ^= seedL >>> b; seedL ^= seedL << a; seedL }
      case 4 => { () => seedL ^= seedL << a; seedL ^= seedL >>> c; seedL ^= seedL << b; seedL }
      case 5 => { () => seedL ^= seedL << c; seedL ^= seedL >>> a; seedL ^= seedL << b; seedL }
      case 6 => { () => seedL ^= seedL << a; seedL ^= seedL >>> c; seedL ^= seedL << b; seedL }
      case _ => { () => seedL ^= seedL << c; seedL ^= seedL >>> a; seedL ^= seedL << b; seedL }
    }
  }

  private lazy val xrng32 = makeXorShift32(triples32(jrand.nextInt(81)))  // one of 81 x 8 = 648 xorshift functions
  private lazy val xrng64 = makeXorShift64(triples64(jrand.nextInt(275))) // one of 275 x 8 = 2200 xorshift functions

  /**
   * Generate a new integer
   */
  def nextInt: Int = xrng32()

  /**
   * Generate a new long
   */
  def nextLong: Long = xrng64()

  /**
   * Generate a new float
   */
  def nextFloat: Float = nextInt.asInstanceOf[Float] / Int.MaxValue

  /**
   * Generate a new double
   */
  def nextDouble: Double = nextLong.asInstanceOf[Double] / Long.MaxValue

  /**
   * Generate a new non-negative integer
   */
  @tailrec final def nextNonNegInt: Int = {
    val tmp = nextInt
    if (tmp >= 0) tmp else nextNonNegInt
  }

  /**
   * Generate a new non-negative long
   */
  @tailrec final def nextNonNegLong: Long = {
    val tmp = nextLong
    if (tmp >= 0) tmp else nextNonNegLong
  }

  /**
   * Generate a new non-negative float
   */
  @tailrec final def nextNonNegFloat: Float = {
    val tmp = nextFloat
    if (tmp >= 0) tmp else nextNonNegFloat
  }


  /**
   * Generate a new non-negative double
   */
  @tailrec final def nextNonNegDouble: Double = {
    val tmp = nextDouble
    if (tmp >= 0) tmp else nextNonNegDouble
  }

  private var next = Double.NaN

  /**
   * Generate a new Gaussian (normally distributed) number
   *
   * This is based on Apache Commons Math's nextGaussian, which in turn is based
   * on the Polar Method of Box, Muller, & Marsiglia as described in Knuth 3.4.1C
   */
  @tailrec final def nextGaussian: Double = {
    if (next == next) {
      val tmp = next
      next = Double.NaN
      tmp
    }
    else {
      val u1 = 2.0 * nextDouble - 1.0
      val u2 = 2.0 * nextDouble - 1.0
      val s  = u1 * u1 + u2 * u2

      if (s >= 1) nextGaussian
      else {
        val bm = if (s != 0) { math.sqrt(-2.0 * math.log(s) / s) } else s
        next = u1 * bm
        u2 * bm
      }
    }
  }

  private val triples32 = Array(
    ( 1, 3,10), ( 1, 5,16), ( 1, 5,19), ( 1, 9,29), ( 1,11, 6), ( 1,11,16), ( 1,19, 3), ( 1,21,20), ( 1,27,27),
    ( 2, 5,15), ( 2, 5,21), ( 2, 7, 7), ( 2, 7, 9), ( 2, 7,25), ( 2, 9,15), ( 2,15,17), ( 2,15,25), ( 2,21, 9),
    ( 3, 1,14), ( 3, 3,26), ( 3, 3,28), ( 3, 3,29), ( 3, 5,20), ( 3, 5,22), ( 3, 5,25), ( 3, 7,29), ( 3,13, 7),
    ( 3,23,25), ( 3,25,24), ( 3,27,11), ( 4, 3,17), ( 4, 3,27), ( 4, 5,15), ( 5, 3,21), ( 5, 7,22), ( 5, 9, 7),
    ( 5, 9,28), ( 5, 9,31), ( 5,13, 6), ( 5,15,17), ( 5,17,13), ( 5,21,12), ( 5,27, 8), ( 5,27,21), ( 5,27,25),
    ( 5,27,28), ( 6, 1,11), ( 6, 3,17), ( 6,17, 9), ( 6,21, 7), ( 6,21,13), ( 7, 1, 9), ( 7, 1,18), ( 7, 1,25),
    ( 7,13,25), ( 7,17,21), ( 7,25,12), ( 7,25,20), ( 8, 7,23), ( 8,9, 23), ( 9, 5,1 ), ( 9, 5,25), ( 9,11,19),
    ( 9,21,16), (10, 9,21), (10, 9,25), (11, 7,12), (11, 7,16), (11,17,13), (11,21,13), (12, 9,23), (13, 3,17),
    (13, 3,27), (13, 5,19), (13,17,15), (14, 1,15), (14,13,15), (15, 1,29), (17,15,20), (17,15,23), (17,15,26))


  private val triples64 = Array(
    ( 1, 1,54), ( 1, 1,55), ( 1, 3,45), ( 1, 7, 9), ( 1, 7,44), ( 1, 7,46), ( 1, 9,50), ( 1,11,35), ( 1,11,50),
    ( 1,13,45), ( 1,15, 4), ( 1,15,63), ( 1,19, 6), ( 1,19,16), ( 1,23,14), ( 1,23,29), ( 1,29,34), ( 1,35, 5),
    ( 1,35,11), ( 1,35,34), ( 1,45,37), ( 1,51,13), ( 1,53, 3), ( 1,59,14), ( 2,13,23), ( 2,31,51), ( 2,31,53),
    ( 2,43,27), ( 2,47,49), ( 3, 1,11), ( 3, 5,21), ( 3,13,59), ( 3,21,31), ( 3,25,20), ( 3,25,31), ( 3,25,56),
    ( 3,29,40), ( 3,29,47), ( 3,29,49), ( 3,35,14), ( 3,37,17), ( 3,43, 4), ( 3,43, 6), ( 3,43,11), ( 3,51,16),
    ( 3,53, 7), ( 3,61,17), ( 3,61,26), ( 4, 7,19), ( 4, 9,13), ( 4,15,51), ( 4,15,53), ( 4,29,45), ( 4,29,49),
    ( 4,31,33), ( 4,35,15), ( 4,35,21), ( 4,37,11), ( 4,37,21), ( 4,41,19), ( 4,41,45), ( 4,43,21), ( 4,43,31),
    ( 4,53, 7), ( 5, 9,23), ( 5,11,54), ( 5,15,27), ( 5,17,11), ( 5,23,36), ( 5,33,29), ( 5,41,20), ( 5,45,16),
    ( 5,47,23), ( 5,53,20), ( 5,59,33), ( 5,59,35), ( 5,59,63), ( 6, 1,17), ( 6, 3,49), ( 6,17,47), ( 6,23,27),
    ( 6,27, 7), ( 6,43,21), ( 6,49,29), ( 6,55,17), ( 7, 5,41), ( 7, 5,47), ( 7, 5,55), ( 7, 7,20), ( 7, 9,38),
    ( 7,11,10), ( 7,11,35), ( 7,13,58), ( 7,19,17), ( 7,19,54), ( 7,23, 8), ( 7,25,58), ( 7,27,59), ( 7,33, 8),
    ( 7,41,40), ( 7,43,28), ( 7,51,24), ( 7,57,12), ( 8, 5,59), ( 8, 9,25), ( 8,13,25), ( 8,13,61), ( 8,15,21),
    ( 8,25,59), ( 8,29,19), ( 8,31,17), ( 8,37,21), ( 8,51,21), ( 9, 1,27), ( 9, 5,36), ( 9, 5,43), ( 9, 7,18),
    ( 9,19,18), ( 9,21,11), ( 9,21,20), ( 9,21,40), ( 9,23,57), ( 9,27,10), ( 9,29,12), ( 9,29,37), ( 9,37,31),
    ( 9,41,45), (10, 7,33), (10,27,59), (10,53,13), (11, 5,32), (11, 5,34), (11, 5,43), (11, 5,45), (11, 9,14),
    (11, 9,34), (11,13,40), (11,15,37), (11,23,42), (11,23,56), (11,25,48), (11,27,26), (11,29,14), (11,31,18),
    (11,53,23), (12, 1,31), (12, 3,13), (12, 3,49), (12, 7,13), (12,11,47), (12,25,27), (12,39,49), (12,43,19),
    (13, 3,40), (13, 3,53), (13, 7,17), (13, 9,15), (13, 9,50), (13,13,19), (13,17,43), (13,19,28), (13,19,47),
    (13,21,18), (13,21,49), (13,29,35), (13,35,30), (13,35,38), (13,47,23), (13,51,21), (14,13,17), (14,15,19),
    (14,23,33), (14,31,45), (14,47,15), (15, 1,19), (15, 5,37), (15,13,28), (15,13,52), (15,17,27), (15,19,63),
    (15,21,46), (15,23,23), (15,45,17), (15,47,16), (15,49,26), (16, 5,17), (16, 7,39), (16,11,19), (16,11,27),
    (16,13,55), (16,21,35), (16,25,43), (16,27,53), (16,47,17), (17,15,58), (17,23,29), (17,23,51), (17,23,52),
    (17,27,22), (17,45,22), (17,47,28), (17,47,29), (17,47,54), (18, 1,25), (18, 3,43), (18,19,19), (18,25,21),
    (18,41,23), (19, 7,36), (19, 7,55), (19,13,37), (19,15,46), (19,21,52), (19,25,20), (19,41,21), (19,43,27),
    (20, 1,31), (20, 5,29), (21, 1,27), (21, 9,29), (21,13,52), (21,15,28), (21,15,29), (21,17,24), (21,17,30),
    (21,17,48), (21,21,32), (21,21,34), (21,21,37), (21,21,38), (21,21,40), (21,21,41), (21,21,43), (21,41,23),
    (22, 3,39), (23, 9,38), (23, 9,48), (23, 9,57), (23,13,38), (23,13,58), (23,13,61), (23,17,25), (23,17,54),
    (23,17,56), (23,17,62), (23,41,34), (23,41,51), (24, 9,35), (24,11,29), (24,25,25), (24,31,35), (25, 7,46),
    (25, 7,49), (25, 9,39), (25,11,57), (25,13,29), (25,13,39), (25,13,62), (25,15,47), (25,21,44), (25,27,27),
    (25,27,53), (25,33,36), (25,39,54), (28, 9,55), (28,11,53), (29,27,37), (31, 1,51), (31,25,37), (31,27,35),
    (33,31,43), (33,31,55), (43,21,46), (49,15,61), (55, 9,56) )
}

object Random {
  def apply() = new Random(None, None)
  def apply(seed0: Int, seed1: Long) = new Random(Some(seed0), Some(seed1))
}