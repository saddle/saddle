package org.saddle

import org.openjdk.jmh.annotations._

@State(Scope.Benchmark)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@Fork(1)
@Threads(1)
class FourInPlaceScalarLongOpBench {
  @Param(Array("10", "100"))
  var size: Int = _

  var m1: Mat[Long] = _
  var b: Long = _

  @Setup(Level.Iteration)
  def setup() = {
    m1 = mat.randn(size, size).map(x => (x * 1000).toLong)
    b = scala.util.Random.nextInt.toLong
  }
  @Benchmark
  def saddleVirtual(): Mat[Long] = {
    import org.saddle.ops.BinOps._
    m1 += b
    m1 *= b
    m1 -= b
    m1 /= b
    m1
  }
  @Benchmark
  def saddleInlined(): Mat[Long] = {
    org.saddle.macros.BinOps.matSclr_LL_Add(m1, b)
    org.saddle.macros.BinOps.matSclr_LL_Mult(m1, b)
    org.saddle.macros.BinOps.matSclr_LL_Sub(m1, b)
    org.saddle.macros.BinOps.matSclr_LL_Div(m1, b)
    m1
  }

  @Benchmark
  def array(): Mat[Long] = {
    val m1a = m1.toArray
    var i = 0
    i = 0
    val N = m1a.length
    while (i < N) {
      if (m1a(i) != Long.MinValue && b != Long.MinValue) {
        m1a(i) += b
      }
      i += 1
    }
    i = 0
    while (i < N) {
      if (m1a(i) != Long.MinValue && b != Long.MinValue) {
        m1a(i) *= b
      }
      i += 1
    }
    i = 0
    while (i < N) {
      if (m1a(i) != Long.MinValue && b != Long.MinValue) {
        m1a(i) -= b
      }
      i += 1
    }
    i = 0
    while (i < N) {
      m1a(i) /= b
      i += 1
    }
    m1
  }

}
