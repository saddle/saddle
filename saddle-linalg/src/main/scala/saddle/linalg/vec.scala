package org.saddle.linalg

import org.saddle._
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

}
