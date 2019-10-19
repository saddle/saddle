package org.saddle.linalg

import org.saddle.Vec
import annotation.implicitNotFound

class VecPimp(val self: Vec[Double]) extends VecLinalgOps

@implicitNotFound(msg = "${O} not found")
trait VecBinOp[O, Res] {
  def apply(a: Vec[Double], b: O): Res
}

trait VecLinalgOps {
  val self: Vec[Double]
  type B = Vec[Double]

  def linalg = this

  def vv(other: Vec[Double])(
      implicit op: VecBinOp[Vec[Double], Double]
  ): Double = op(self, other)

  /* Sum. Does not filter out NAs*/
  def sum2 = {
    var s = 0d
    var i = 0
    val ar = self.toArray
    val n = ar.length
    while (i < n) {
      s += ar(i)
      i += 1
    }
    s
  }

  /* One pass mean using Welford's algorithm. Does not filter out NAs */
  def mean2 = {
    val n = self.length
    val ar = self.toArray
    var xm = 0d
    var i = 0
    while (i < n) {
      val x = ar(i)
      xm += (x - xm) / (i + 1)
      i += 1
    }
    xm
  }

  /* One pass sample variance using Welford's algorithm. Does not filter out NAs */
  def sampleVariance = {
    val n = self.length
    val ar = self.toArray
    var m = 0d
    var xm = 0d
    var i = 0
    while (i < n) {
      val x = ar(i)
      val tmp = xm
      xm += (x - xm) / (i + 1)
      m += (x - tmp) * (x - xm)
      i += 1
    }
    m / (n - 1)
  }

  def sampleStandardDeviation = math.sqrt(sampleVariance)

  /* Subtracts the mean from each element. Does not filter out NAs */
  def demeaned = {
    val mean = this.mean2
    val ar1 = self.toArray
    val n = ar1.length
    val ar2 = Array.ofDim[Double](n)
    var i = 0
    while (i < n) {
      ar2(i) = ar1(i) - mean
      i += 1
    }
    Vec(ar2)
  }

  /** One pass Pearson correlation coefficient. Does not filter out NAs.
    * https://prod-ng.sandia.gov/techlib-noauth/access-control.cgi/2008/086212.pdf
    */
  def pearson(other: Vec[Double]) = {
    val n = self.length
    assert(n == other.length)
    val ar1 = self.toArray
    val ar2 = other.toArray
    var covS = 0d
    var varS1 = 0d
    var varS2 = 0d
    var xm1 = 0d
    var xm2 = 0d
    var i = 0
    while (i < n) {
      val x1 = ar1(i)
      val x2 = ar2(i)
      val tmp1 = xm1
      val tmp2 = xm2
      xm1 += (x1 - xm1) / (i + 1)
      xm2 += (x2 - xm2) / (i + 1)
      covS += (x1 - tmp1) * (x2 - tmp2) * i / (i + 1)
      varS1 += (x1 - tmp1) * (x1 - xm1)
      varS2 += (x2 - tmp2) * (x2 - xm2)
      i += 1
    }
    covS / math.sqrt(varS1 * varS2)
  }

  /** One pass covariance of two vectors
    *
    * https://prod-ng.sandia.gov/techlib-noauth/access-control.cgi/2008/086212.pdf
    */
  def sampleCovariance(other: Vec[Double]) = {
    val n = self.length
    assert(n == other.length)
    val ar1 = self.toArray
    val ar2 = other.toArray
    var covS = 0d
    var xm1 = 0d
    var xm2 = 0d
    var i = 0
    while (i < n) {
      val x1 = ar1(i)
      val x2 = ar2(i)
      val tmp1 = xm1
      val tmp2 = xm2
      xm1 += (x1 - xm1) / (i + 1)
      xm2 += (x2 - xm2) / (i + 1)
      covS += (x1 - tmp1) * (x2 - tmp2) * i / (i + 1)
      i += 1
    }
    covS / (n - 1)
  }

}
