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
package org.saddle.index

import org.saddle.{ST, ORD}

/**
  * A Melter operates on a Tuple,,N,, and a Tuple,,M,, and produces a Tuple,,N+M,, which is
  * composed of the corresponding tuple elements.
  */
trait Melter[A, B, C] {
  def apply(a: A, b: B): C

  def tag: ST[C]
  def ord: ORD[C]
}

/**
  * Lowest priority melter implicit instance takes two arbitrary types and produces a Tuple2
  */
trait MelterLowerPriority {

  /**
    * Creates a new Melter instance from a function that implements the internal mechanics
    * @param fn A function from (A, B) => C
    * @tparam A Type of left operand
    * @tparam B Type of right operand
    * @tparam C Type of result
    */
  protected def makeMelter[A: ST, B: ST, C: ST: ORD](fn: (A, B) => C) =
    new Melter[A, B, C] {
      def apply(a: A, b: B) = fn(a, b)
      def tag = implicitly[ST[C]]
      def ord = implicitly[ORD[C]]
    }

  implicit def melt[A: ST: ORD, B: ST: ORD] = makeMelter { (a: A, b: B) =>
    (a, b)
  }
}

/**
  * Next lowest priority melter implicit instances; takes one arbitrary types and a Tuple,,N,, and produces a Tuple,,N+1,,
  */
trait MelterLowPriority extends MelterLowerPriority {
  implicit def melt1_2[A: ST: ORD, B: ST: ORD, C: ST: ORD] = makeMelter {
    (a: A, b: (B, C)) =>
      (a, b._1, b._2)
  }
  implicit def melt1_3[A: ST: ORD, B: ST: ORD, C: ST: ORD, D: ST: ORD] =
    makeMelter { (a: A, b: (B, C, D)) =>
      (a, b._1, b._2, b._3)
    }
  implicit def melt1_4[
      A: ST: ORD,
      B: ST: ORD,
      C: ST: ORD,
      D: ST: ORD,
      E: ST: ORD
  ] = makeMelter { (a: A, b: (B, C, D, E)) =>
    (a, b._1, b._2, b._3, b._4)
  }
  implicit def melt1_5[
      A: ST: ORD,
      B: ST: ORD,
      C: ST: ORD,
      D: ST: ORD,
      E: ST: ORD,
      F: ST: ORD
  ] = makeMelter { (a: A, b: (B, C, D, E, F)) =>
    (a, b._1, b._2, b._3, b._4, b._5)
  }
  implicit def melt1_6[
      A: ST: ORD,
      B: ST: ORD,
      C: ST: ORD,
      D: ST: ORD,
      E: ST: ORD,
      F: ST: ORD,
      G: ST: ORD
  ] = makeMelter { (a: A, b: (B, C, D, E, F, G)) =>
    (a, b._1, b._2, b._3, b._4, b._5, b._6)
  }
  implicit def melt1_7[
      A: ST: ORD,
      B: ST: ORD,
      C: ST: ORD,
      D: ST: ORD,
      E: ST: ORD,
      F: ST: ORD,
      G: ST: ORD,
      H: ST: ORD
  ] = makeMelter { (a: A, b: (B, C, D, E, F, G, H)) =>
    (a, b._1, b._2, b._3, b._4, b._5, b._6, b._7)
  }
  implicit def melt1_8[
      A: ST: ORD,
      B: ST: ORD,
      C: ST: ORD,
      D: ST: ORD,
      E: ST: ORD,
      F: ST: ORD,
      G: ST: ORD,
      H: ST: ORD,
      I: ST: ORD
  ] = makeMelter { (a: A, b: (B, C, D, E, F, G, H, I)) =>
    (a, b._1, b._2, b._3, b._4, b._5, b._6, b._7, b._8)
  }

  implicit def melt2_1[A: ST: ORD, B: ST: ORD, C: ST: ORD] = makeMelter {
    (a: (A, B), b: C) =>
      (a._1, a._2, b)
  }
  implicit def melt3_1[A: ST: ORD, B: ST: ORD, C: ST: ORD, D: ST: ORD] =
    makeMelter { (a: (A, B, C), b: D) =>
      (a._1, a._2, a._3, b)
    }
  implicit def melt4_1[
      A: ST: ORD,
      B: ST: ORD,
      C: ST: ORD,
      D: ST: ORD,
      E: ST: ORD
  ] = makeMelter { (a: (A, B, C, D), b: E) =>
    (a._1, a._2, a._3, a._4, b)
  }
  implicit def melt5_1[
      A: ST: ORD,
      B: ST: ORD,
      C: ST: ORD,
      D: ST: ORD,
      E: ST: ORD,
      F: ST: ORD
  ] = makeMelter { (a: (A, B, C, D, E), b: F) =>
    (a._1, a._2, a._3, a._4, a._5, b)
  }
  implicit def melt6_1[
      A: ST: ORD,
      B: ST: ORD,
      C: ST: ORD,
      D: ST: ORD,
      E: ST: ORD,
      F: ST: ORD,
      G: ST: ORD
  ] = makeMelter { (a: (A, B, C, D, E, F), b: G) =>
    (a._1, a._2, a._3, a._4, a._5, a._6, b)
  }
  implicit def melt7_1[
      A: ST: ORD,
      B: ST: ORD,
      C: ST: ORD,
      D: ST: ORD,
      E: ST: ORD,
      F: ST: ORD,
      G: ST: ORD,
      H: ST: ORD
  ] = makeMelter { (a: (A, B, C, D, E, F, G), b: H) =>
    (a._1, a._2, a._3, a._4, a._5, a._6, a._7, b)
  }
  implicit def melt8_1[
      A: ST: ORD,
      B: ST: ORD,
      C: ST: ORD,
      D: ST: ORD,
      E: ST: ORD,
      F: ST: ORD,
      G: ST: ORD,
      H: ST: ORD,
      I: ST: ORD
  ] = makeMelter { (a: (A, B, C, D, E, F, G, H), b: I) =>
    (a._1, a._2, a._3, a._4, a._5, a._6, a._7, a._8, b)
  }
}

/**
  * Normal priority melter implicit instances takes one a Tuple,,N,, and a Tuple,,M,, and produce a Tuple,,N+M,,
  */
object Melter extends MelterLowPriority {
  implicit def melt2_2[A: ST: ORD, B: ST: ORD, C: ST: ORD, D: ST: ORD] =
    makeMelter { (a: (A, B), b: (C, D)) =>
      (a._1, a._2, b._1, b._2)
    }

  implicit def melt2_3[
      A: ST: ORD,
      B: ST: ORD,
      C: ST: ORD,
      D: ST: ORD,
      E: ST: ORD
  ] = makeMelter { (a: (A, B), b: (C, D, E)) =>
    (a._1, a._2, b._1, b._2, b._3)
  }
  implicit def melt3_2[
      A: ST: ORD,
      B: ST: ORD,
      C: ST: ORD,
      D: ST: ORD,
      E: ST: ORD
  ] = makeMelter { (a: (A, B, C), b: (C, D)) =>
    (a._1, a._2, a._3, b._1, b._2)
  }

  implicit def melt2_4[
      A: ST: ORD,
      B: ST: ORD,
      C: ST: ORD,
      D: ST: ORD,
      E: ST: ORD,
      F: ST: ORD
  ] = makeMelter { (a: (A, B), b: (C, D, E, F)) =>
    (a._1, a._2, b._1, b._2, b._3, b._4)
  }
  implicit def melt3_3[
      A: ST: ORD,
      B: ST: ORD,
      C: ST: ORD,
      D: ST: ORD,
      E: ST: ORD,
      F: ST: ORD
  ] = makeMelter { (a: (A, B, C), b: (D, E, F)) =>
    (a._1, a._2, a._3, b._1, b._2, b._3)
  }
  implicit def melt4_2[
      A: ST: ORD,
      B: ST: ORD,
      C: ST: ORD,
      D: ST: ORD,
      E: ST: ORD,
      F: ST: ORD
  ] = makeMelter { (a: (A, B, C, D), b: (E, F)) =>
    (a._1, a._2, a._3, a._4, b._1, b._2)
  }

  implicit def melt2_5[
      A: ST: ORD,
      B: ST: ORD,
      C: ST: ORD,
      D: ST: ORD,
      E: ST: ORD,
      F: ST: ORD,
      G: ST: ORD
  ] = makeMelter { (a: (A, B), b: (C, D, E, F, G)) =>
    (a._1, a._2, b._1, b._2, b._3, b._4, b._5)
  }
  implicit def melt3_4[
      A: ST: ORD,
      B: ST: ORD,
      C: ST: ORD,
      D: ST: ORD,
      E: ST: ORD,
      F: ST: ORD,
      G: ST: ORD
  ] = makeMelter { (a: (A, B, C), b: (D, E, F, G)) =>
    (a._1, a._2, a._3, b._1, b._2, b._3, b._4)
  }
  implicit def melt4_3[
      A: ST: ORD,
      B: ST: ORD,
      C: ST: ORD,
      D: ST: ORD,
      E: ST: ORD,
      F: ST: ORD,
      G: ST: ORD
  ] = makeMelter { (a: (A, B, C, D), b: (E, F, G)) =>
    (a._1, a._2, a._3, a._4, b._1, b._2, b._3)
  }
  implicit def melt5_2[
      A: ST: ORD,
      B: ST: ORD,
      C: ST: ORD,
      D: ST: ORD,
      E: ST: ORD,
      F: ST: ORD,
      G: ST: ORD
  ] = makeMelter { (a: (A, B, C, D, E), b: (F, G)) =>
    (a._1, a._2, a._3, a._4, a._5, b._1, b._2)
  }

  implicit def melt2_6[
      A: ST: ORD,
      B: ST: ORD,
      C: ST: ORD,
      D: ST: ORD,
      E: ST: ORD,
      F: ST: ORD,
      G: ST: ORD,
      H: ST: ORD
  ] = makeMelter { (a: (A, B), b: (C, D, E, F, G, H)) =>
    (a._1, a._2, b._1, b._2, b._3, b._4, b._5, b._6)
  }
  implicit def melt3_5[
      A: ST: ORD,
      B: ST: ORD,
      C: ST: ORD,
      D: ST: ORD,
      E: ST: ORD,
      F: ST: ORD,
      G: ST: ORD,
      H: ST: ORD
  ] = makeMelter { (a: (A, B, C), b: (D, E, F, G, H)) =>
    (a._1, a._2, a._3, b._1, b._2, b._3, b._4, b._5)
  }
  implicit def melt4_4[
      A: ST: ORD,
      B: ST: ORD,
      C: ST: ORD,
      D: ST: ORD,
      E: ST: ORD,
      F: ST: ORD,
      G: ST: ORD,
      H: ST: ORD
  ] = makeMelter { (a: (A, B, C, D), b: (E, F, G, H)) =>
    (a._1, a._2, a._3, a._4, b._1, b._2, b._3, b._4)
  }
  implicit def melt5_3[
      A: ST: ORD,
      B: ST: ORD,
      C: ST: ORD,
      D: ST: ORD,
      E: ST: ORD,
      F: ST: ORD,
      G: ST: ORD,
      H: ST: ORD
  ] = makeMelter { (a: (A, B, C, D, E), b: (F, G, H)) =>
    (a._1, a._2, a._3, a._4, a._5, b._1, b._2, b._3)
  }
  implicit def melt6_2[
      A: ST: ORD,
      B: ST: ORD,
      C: ST: ORD,
      D: ST: ORD,
      E: ST: ORD,
      F: ST: ORD,
      G: ST: ORD,
      H: ST: ORD
  ] = makeMelter { (a: (A, B, C, D, E, F), b: (G, H)) =>
    (a._1, a._2, a._3, a._4, a._5, a._6, b._1, b._2)
  }

  implicit def melt2_7[
      A: ST: ORD,
      B: ST: ORD,
      C: ST: ORD,
      D: ST: ORD,
      E: ST: ORD,
      F: ST: ORD,
      G: ST: ORD,
      H: ST: ORD,
      I: ST: ORD
  ] = makeMelter { (a: (A, B), b: (C, D, E, F, G, H, I)) =>
    (a._1, a._2, b._1, b._2, b._3, b._4, b._5, b._6, b._7)
  }
  implicit def melt3_6[
      A: ST: ORD,
      B: ST: ORD,
      C: ST: ORD,
      D: ST: ORD,
      E: ST: ORD,
      F: ST: ORD,
      G: ST: ORD,
      H: ST: ORD,
      I: ST: ORD
  ] = makeMelter { (a: (A, B, C), b: (D, E, F, G, H, I)) =>
    (a._1, a._2, a._3, b._1, b._2, b._3, b._4, b._5, b._6)
  }
  implicit def melt4_5[
      A: ST: ORD,
      B: ST: ORD,
      C: ST: ORD,
      D: ST: ORD,
      E: ST: ORD,
      F: ST: ORD,
      G: ST: ORD,
      H: ST: ORD,
      I: ST: ORD
  ] = makeMelter { (a: (A, B, C, D), b: (E, F, G, H, I)) =>
    (a._1, a._2, a._3, a._4, b._1, b._2, b._3, b._4, b._5)
  }
  implicit def melt5_4[
      A: ST: ORD,
      B: ST: ORD,
      C: ST: ORD,
      D: ST: ORD,
      E: ST: ORD,
      F: ST: ORD,
      G: ST: ORD,
      H: ST: ORD,
      I: ST: ORD
  ] = makeMelter { (a: (A, B, C, D, E), b: (F, G, H, I)) =>
    (a._1, a._2, a._3, a._4, a._5, b._1, b._2, b._3, b._4)
  }
  implicit def melt6_3[
      A: ST: ORD,
      B: ST: ORD,
      C: ST: ORD,
      D: ST: ORD,
      E: ST: ORD,
      F: ST: ORD,
      G: ST: ORD,
      H: ST: ORD,
      I: ST: ORD
  ] = makeMelter { (a: (A, B, C, D, E, F), b: (G, H, I)) =>
    (a._1, a._2, a._3, a._4, a._5, a._6, b._1, b._2, b._3)
  }
  implicit def melt7_2[
      A: ST: ORD,
      B: ST: ORD,
      C: ST: ORD,
      D: ST: ORD,
      E: ST: ORD,
      F: ST: ORD,
      G: ST: ORD,
      H: ST: ORD,
      I: ST: ORD
  ] = makeMelter { (a: (A, B, C, D, E, F, G), b: (H, I)) =>
    (a._1, a._2, a._3, a._4, a._5, a._6, a._7, b._1, b._2)
  }
}
