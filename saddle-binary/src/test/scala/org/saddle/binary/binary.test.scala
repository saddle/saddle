/**
  * Copyright (c) 2019 Saddle Development Team
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
  */
package org.saddle.binary
import org.saddle._
import org.scalatest.funsuite.AnyFunSuite

class BinarySuite extends AnyFunSuite {
  test("2x3") {
    val frame = Frame(
      Mat(Vec(1d, 2d), Vec(3d, 4d), Vec(5d, 6d)),
      Index("r1", "r2"),
      Index("c1", "c2", "c3")
    )
    val binaryFrame = Writer.writeFrameIntoArray(frame).right.get
    val binaryMat = Writer.writeMatIntoArray(frame.toMat).right.get
    val deserFrame = Reader.readFrameFromArray[Double](binaryFrame)
    val deserMat = Reader.readMatFromArray[Double](binaryFrame)
    val deserMat2 = Reader.readMatFromArray[Double](binaryMat)
    assert(deserFrame.right.get == frame)
    assert(deserMat.right.get == frame.toMat)
    assert(deserMat2.right.get == frame.toMat)
  }
  test("double") {
    val frame = Frame(
      Mat(Vec(1d, 2d), Vec(3d, 4d), Vec(5d, 6d)),
      Index("r1", "r2"),
      Index("c1", "c2", "c3")
    )
    val binaryFrame = Writer.writeFrameIntoArray(frame).right.get
    val deserFrame = Reader.readFrameFromArray[Double](binaryFrame)
    assert(deserFrame.right.get == frame)
  }
  test("int") {
    val frame = Frame(
      Mat(Vec(1d, 2d), Vec(3d, 4d), Vec(5d, 6d)).map(_.toInt),
      Index("r1", "r2"),
      Index("c1", "c2", "c3")
    )
    val binaryFrame = Writer.writeFrameIntoArray(frame).right.get
    val deserFrame = Reader.readFrameFromArray[Int](binaryFrame)
    assert(deserFrame.right.get == frame)
  }
  test("float") {
    val frame = Frame(
      Mat(Vec(1d, 2d), Vec(3d, 4d), Vec(5d, 6d)).map(_.toFloat),
      Index("r1", "r2"),
      Index("c1", "c2", "c3")
    )
    val binaryFrame = Writer.writeFrameIntoArray(frame).right.get
    val deserFrame = Reader.readFrameFromArray[Float](binaryFrame)
    assert(deserFrame.right.get == frame)
  }
  test("long") {
    val frame = Frame(
      Mat(Vec(1d, 2d), Vec(3d, 4d), Vec(5d, 6d)).map(_.toLong),
      Index("r1", "r2"),
      Index("c1", "c2", "c3")
    )
    val binaryFrame = Writer.writeFrameIntoArray(frame).right.get
    val deserFrame = Reader.readFrameFromArray[Long](binaryFrame)
    assert(deserFrame.right.get == frame)
  }
  test("1x3") {
    val frame = Frame(
      Mat(Vec(1d, 2d), Vec(3d, 4d), Vec(5d, 6d)),
      Index("r1", "r2"),
      Index("c1", "c2", "c3")
    ).rowAt(Array(0))
    val binary = Writer.writeFrameIntoArray(frame).right.get
    val deser = Reader.readFrameFromArray[Double](binary)
    assert(deser.right.get == frame)
  }
  test("3x1") {
    val frame = Frame(
      Mat(Vec(1d, 2d), Vec(3d, 4d), Vec(5d, 6d)),
      Index("r1", "r2"),
      Index("c1", "c2", "c3")
    ).colAt(Array(0))
    val binary = Writer.writeFrameIntoArray(frame).right.get
    val deser = Reader.readFrameFromArray[Double](binary)
    assert(deser.right.get == frame)
  }
  test("empty") {
    val frame = Frame.empty[String, String, Double]
    val binary = Writer.writeFrameIntoArray(frame).right.get
    val deser = Reader.readFrameFromArray[Double](binary)
    assert(deser.right.get == frame)
  }
  test("frame into file") {
    val frame = Frame(
      Mat(Vec(1d, 2d), Vec(3d, 4d), Vec(5d, 6d)).map(_.toLong),
      Index("r1", "r2"),
      Index("c1", "c2", "c3")
    )
    val file = java.io.File.createTempFile("saddletest", "saddle")
    val os = new java.io.FileOutputStream(file)
    val writableChannel = os.getChannel
    Writer.writeFrameIntoChannel(frame, writableChannel).right.get
    writableChannel.close
    val is = new java.io.FileInputStream(file)
    val readableChannel = is.getChannel
    val deserFrame = Reader.readFrameFromChannel[Long](readableChannel)
    readableChannel.close
    assert(deserFrame.right.get == frame)
  }
  test("mat into file") {
    val frame = Frame(
      Mat(Vec(1d, 2d), Vec(3d, 4d), Vec(5d, 6d)).map(_.toLong),
      Index("r1", "r2"),
      Index("c1", "c2", "c3")
    )
    val file = java.io.File.createTempFile("saddletest", "saddle")
    val os = new java.io.FileOutputStream(file)
    val writableChannel = os.getChannel
    Writer.writeMatIntoChannel(frame.toMat, writableChannel).right.get
    writableChannel.close
    val is = new java.io.FileInputStream(file)
    val readableChannel = is.getChannel
    val deser = Reader.readMatFromChannel[Long](readableChannel)
    readableChannel.close
    assert(deser.right.get == frame.toMat)
  }

}
