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
import java.io.InputStream
import org.saddle.vec

/**
 * The Random class provides methods to generate pseudo-random numbers via a plug-in
 * PRNG, which is simply any function that generates a Long primitive.
 */
class Random private (rng64: () => Long) {
  /**
   * Generate a new integer (taking the 32 low order bits of the
   * 64 bit state)
   */
  def nextInt: Int = rng64().asInstanceOf[Int]

  /**
   * Generate a new long
   */
  def nextLong: Long = rng64()

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
}

object Random {
  /**
   * Create Random instance
   */
  def apply() = new Random(XorShift(new java.util.Random().nextLong))

  /**
   * Create Random instance from provided seed
   */
  def apply(seed: Long) = new Random(XorShift(new java.util.Random(seed).nextLong))

  /**
   * Create Random instance from custom RNG function
   */
  def apply(rng: () => Long) = new Random(rng)
}

/**
 * Marsaglia XorShift PRNG
 *
 * See [[http://www.jstatsoft.org/v08/i14/ Marsaglia]]
 */
object XorShift {
  def apply(): () => Long = apply(new java.util.Random().nextLong)

  def apply(seed: Long): () => Long = makeRNG((13,7,17), seed)

  def makeRNG(tup: (Int, Int, Int), seed: Long): () => Long = {
    var seedL = seed
    val (a, b, c) = tup
    () => seedL ^= (seedL << a); seedL ^= (seedL >> b); seedL ^= (seedL << c); seedL
  }
}

/**
 * Marsaglia Lagged Fibonacci PRNG
 *
 * See [[https://groups.google.com/forum/?fromgroups=#!msg/sci.crypt/yoaCpGWKEk0/UXCxgufdTesJ]]
 */
object LFib4 {
  def apply(): () => Long = apply(new java.util.Random().nextLong)

  def apply(seed: Long): () => Long = makeRNG(seed)

  def makeRNG(seed: Long): () => Long = {
    val jrand = new java.util.Random(seed)
    val state = Array.ofDim[Long](256)      // 2K of memory
    for (i <- 0 until 256) state(i) = jrand.nextLong
    var c = 0

    () => {
      c += 1
      c &= 0xFF
      state(c) = state(c) + state((c+58) & 0xFF) + state((c+119) & 0xFF) + state((c+178) & 0xFF)
      state(c)
    }
  }
}

/**
 * Ziff 4-tap shift-register-sequence
 *
 * http://arxiv.org/pdf/cond-mat/9710104v1.pdf
 * http://www.aip.org/cip/pdf/vol_12/iss_4/385_1.pdf
 */
object Ziff98 {
  def apply(): () => Long = apply(new java.util.Random().nextLong)

  def apply(seed: Long): () => Long = makeRNG(seed)

  def makeRNG(seed: Long): () => Long = {
    val (a, b, c, d, m) = (471, 1586, 6988, 9689, 16383)

    val jrand = new java.util.Random(seed)
    val state = Array.ofDim[Long](m + 1)   // 128K of memory

    var nd = 0
    for (i <- 0 until m) state(i) = jrand.nextLong

    () => {
      nd += 1
      val (a1, b1, c1, d1, e1) = (nd & m, (nd - a) & m, (nd - b) & m, (nd - c) & m, (nd - d) & m)
      state(a1) = state(b1) ^ state(c1) ^ state(d1) ^ state(e1)
      state(a1)
    }
  }
}

/**
 * Create a random InputStream of bytes from a PRNG. Useful for testing, e.g.,
 * for feeding into dieharder battery of tests via stdin.
 */
case class RandomStream(rng: () => Long) extends InputStream {
  var c = 0
  var r = rng()

  def read(): Int = {
    c += 1
    val byte = c match {
      case 1 => r
      case 2 => r >>> 8
      case 3 => r >>> 16
      case 4 => r >>> 24
      case 5 => r >>> 32
      case 6 => r >>> 40
      case 7 => r >>> 48
      case 8 => c = 0; val tmp = (r >>> 56); r = rng(); tmp
    }
    (byte & 0xFF).asInstanceOf[Int]
  }
}

