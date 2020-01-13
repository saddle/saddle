package org.saddle

import org.openjdk.jmh.annotations._
@State(Scope.Benchmark)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@Fork(1)
@Threads(1)
class DotBench {
  @Param(Array("500", "1000", "5000"))
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
    v1 vv_blas v2
  }
  @Benchmark
  def raw_unrolled(): Double = {
    import org.saddle.linalg._
    v1 vv_java v2
  }

}
