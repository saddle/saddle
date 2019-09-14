package org.saddle.framework

import org.saddle._
import org.specs2.matcher._

/**
 * A matcher for two numeric Mats that must be equal to within
 * a tolerance
 */
class BeCloseToMat[T: Numeric : ClassManifest](m: Mat[T], delta: T) extends Matcher[Mat[T]] {
  def apply[S <: Mat[T]](x: Expectable[S]) = {
    val num = implicitly[Numeric[T]]

    result(m.length == 0 || {
      val res = m.contents.zipWithIndex map {
        case (n, i) =>
          num.lteq(num.minus(n, delta), x.value.contents(i)) &&
          num.lteq(x.value.contents(i), num.plus(n, delta))
      }
      Vec(res: _*).all
    },
      " are close +/- " + delta, " are close +/- " + delta, x)
  }
}

object BeCloseToMat {
  def apply[T: Numeric : ClassManifest](v: Mat[T], delta: T) = new BeCloseToMat[T](v, delta)
}