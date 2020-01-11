package org.saddle

import org.openjdk.jmh.annotations._

class TestImpl(self: Mat[Double]) {
  import org.saddle.linalg.NetLib._
  /* diag( A x t(A) ) */
  def diagOuterM_blas: Vec[Double] = {
    val x = self
    assert(x.numCols > 0)
    assert(x.numRows > 0)

    /* diag(AA') = rowSums(A * A elementwise) */
    val output = Array.ofDim[Double](x.numRows)
    var i = 0
    val M = x.numCols
    val N = x.numRows
    val xa = x.toArray
    while (i < N) {
      output(i) = BLAS.ddot(M, xa, i * M, 1, xa, i * M, 1)
      i += 1
    }

    Vec(output)

  }
  /* diag( A x t(A) ) */
  def diagOuterM_java: Vec[Double] = {
    val x = self
    assert(x.numCols > 0)
    assert(x.numRows > 0)

    /* diag(AA') = rowSums(A * A elementwise) */
    val output = Array.ofDim[Double](x.numRows)
    var i = 0
    var j = 0
    val M = x.numCols
    val N = x.numRows
    var s = 0d
    val xa = x.toArray
    while (i < N) {
      val iM = i * M
      while (j < M) {
        val v = xa(iM + j)
        s += v * v
        j += 1
      }
      output(i) = s
      s = 0d
      j = 0
      i += 1
    }

    Vec(output)

  }

  def diagOuterM_java_unroll: Vec[Double] = {
    val x = self
    assert(x.numCols > 0)
    assert(x.numRows > 0)

    /* diag(AA') = rowSums(A * A elementwise) */
    val output = Array.ofDim[Double](x.numRows)
    var i = 0
    var j = 0
    val M = x.numCols
    val N = x.numRows
    var s = 0d
    val xa = x.toArray
    while (i < N) {
      val iM = i * M
      val m = M % 5
      while (j < m) {
        val v = xa(iM + j)
        s += v * v
        j += 1
      }

      while (j < M) {
        val v1 = xa(iM + j)
        val v2 = xa(iM + j + 1)
        val v3 = xa(iM + j + 2)
        val v4 = xa(iM + j + 3)
        val v5 = xa(iM + j + 4)
        s += v1 * v1 + v2 * v2 + v3 * v3 + v4 * v4 + v5 * v5
        j += 5
      }
      output(i) = s
      s = 0d
      j = 0
      i += 1
    }

    Vec(output)

  }
}

@State(Scope.Benchmark)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@Fork(1)
@Threads(1)
class DiagOuterMBench {
  @Param(Array("10000"))
  var size: Int = _

  var m: Mat[Double] = _

  @Setup(Level.Iteration)
  def setup() = {
    m = mat.randn(500, size)
  }
  @Benchmark
  def blas(): Vec[Double] = {
    new TestImpl(m).diagOuterM_blas
  }
  @Benchmark
  def java_unroll(): Vec[Double] = {
    new TestImpl(m).diagOuterM_java_unroll
  }
  @Benchmark
  def java(): Vec[Double] = {
    new TestImpl(m).diagOuterM_java
  }

}
