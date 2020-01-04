package org.saddle

import org.openjdk.jmh.annotations._

@State(Scope.Benchmark)
class OpBench {
  @Param(Array("100"))
  var size: Int = _

  var m1: Mat[Double] = _
  var m2: Mat[Double] = _

  @Setup(Level.Invocation)
  def setup() = {
    m1 = mat.randn(size, size)
    m2 = mat.randn(size, size)
  }
  @Benchmark
  def saddlePlusEquals(): Mat[Double] = {
    m1 += m2
    m1
  }

  @Benchmark
  def arrayPlusEquals(): Mat[Double] = {
    val m1a = m1.toArray
    val m2a = m2.toArray
    var i = 0
    val N = m1a.length
    while (i < N) {
      m1a(i) += m2a(i)
      i += 1
    }
    m1
  }

}
