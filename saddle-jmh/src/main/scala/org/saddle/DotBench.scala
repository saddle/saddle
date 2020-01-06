package org.saddle

import org.openjdk.jmh.annotations._

@State(Scope.Benchmark)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@Fork(1)
@Threads(1)
class DotBench {
  @Param(Array("100", "500", "1000"))
  var size: Int = _

  var v1: Vec[Double] = _
  var v2: Vec[Double] = _

  @Setup(Level.Iteration)
  def setup() = {
    v1 = vec.randn(size)
    v2 = vec.randn(size)
  }
  @Benchmark
  def blas(): Double = {
    import org.saddle.linalg._
    v1 vv v2
  }
  @Benchmark
  def toArray_inline(): Double = {
    var i = 0
    val a1 = v1.toArray
    val a2 = v2.toArray
    var s = 0d
    val N = a1.length
    while (i < N) {
      s += a1(i) * a2(i)
      i += 1
    }
    s
  }
  @Benchmark
  def raw(): Double = {
    import org.saddle.linalg._
    v1 vv2 v2
  }

}
