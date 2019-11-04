package org.saddle.binary
import org.saddle._
import org.scalatest.FunSuite

class BinarySuite extends FunSuite {
  test("2x3") {
    val frame = Frame(
      Mat(Vec(1d, 2d), Vec(3d, 4d), Vec(5d, 6d)),
      Index("r1", "r2"),
      Index("c1", "c2", "c3")
    )
    val binary = serialize(frame)
    val deser = deserialize(binary).right.get
    assert(deser == frame)
  }
  test("1x3") {
    val frame = Frame(
      Mat(Vec(1d, 2d), Vec(3d, 4d), Vec(5d, 6d)),
      Index("r1", "r2"),
      Index("c1", "c2", "c3")
    ).rowAt(Array(0))
    val binary = serialize(frame)
    val deser = deserialize(binary).right.get
    assert(deser == frame)
  }
  test("3x1") {
    val frame = Frame(
      Mat(Vec(1d, 2d), Vec(3d, 4d), Vec(5d, 6d)),
      Index("r1", "r2"),
      Index("c1", "c2", "c3")
    ).colAt(Array(0))
    val binary = serialize(frame)
    val deser = deserialize(binary).right.get
    assert(deser == frame)
  }
  test("empty") {
    val frame = Frame.empty[String, String, Double]
    val binary = serialize(frame)
    val deser = deserialize(binary).right.get
    assert(binary.size == 78)
    assert(deser == frame)
  }

}
