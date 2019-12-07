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
import org.saddle.{Buffer => _, _}
import org.saddle.scalar._
import java.nio._
import scala.util.{Left, Right, Either}
import java.nio.channels.ReadableByteChannel
import Writer._
import org.saddle.index.IndexIntRange

object Reader {

  def parse[T: ST](size: Int, from: ByteBuffer): Either[String, Array[T]] =
    implicitly[ST[T]] match {
      case ScalarTagDouble =>
        Right {
          val to = DoubleBuffer.allocate(size)
          while (to.hasRemaining() && from.hasRemaining()) {
            to.put(from.getDouble)
          }
          to.array
        }
      case ScalarTagInt =>
        Right {
          val to = IntBuffer.allocate(size)
          while (to.hasRemaining() && from.hasRemaining()) {
            to.put(from.getInt)
          }
          to.array
        }
      case ScalarTagFloat =>
        Right {
          val to = FloatBuffer.allocate(size)
          while (to.hasRemaining() && from.hasRemaining()) {
            to.put(from.getFloat())
          }
          to.array
        }
      case ScalarTagLong =>
        Right {
          val to = LongBuffer.allocate(size)
          while (to.hasRemaining() && from.hasRemaining()) {
            to.put(from.getLong())
          }
          to.array
        }
      case ScalarTagByte =>
        Right {
          val to = ByteBuffer.allocate(size)
          while (to.hasRemaining() && from.hasRemaining()) {
            to.asInstanceOf[ByteBuffer].put(from.get)
          }
          to.array
        }
      case other => Left(s"Type $other not supported.")
    }

  def sequence[A, B](s: Seq[Either[A, B]]): Either[A, Seq[B]] =
    if (s.forall(_.isRight))
      Right(s.map(_.right.get))
    else s.find(_.isLeft).get.asInstanceOf[Left[A, Seq[B]]]

  def readFully(bb: ByteBuffer, channel: ReadableByteChannel) = {
    bb.clear
    var i = 0
    while (bb.hasRemaining && i >= 0) {
      i = channel.read(bb)
    }
    bb.flip
  }

  def readHeaderFromChannel[T: ST](channel: ReadableByteChannel) =
    dtype[T].right.flatMap { expectedDataType =>
      val magicAndVersion = Array.ofDim[Byte](8)
      readFully(ByteBuffer.wrap(magicAndVersion), channel)
      val magic = String.valueOf(magicAndVersion.map(_.toChar), 0, 6)
      val major = magicAndVersion(6)
      val minor = magicAndVersion(7)
      if (major != 1 || minor != 0 || magic != "SADDLE")
        Left(
          s"Magic string is incorrect or version of file format not supported. Found in file: $magic / $major / $minor"
        )
      else {
        val headerLengthBB =
          ByteBuffer.allocate(4).order(ByteOrder.LITTLE_ENDIAN)
        readFully(headerLengthBB, channel)
        val headerLength = headerLengthBB.getInt
        val headerArray = Array.ofDim[Byte](headerLength)
        readFully(ByteBuffer.wrap(headerArray), channel)
        val header = new String(headerArray, "UTF-8")
        val descriptor = ujson.read(header).obj
        val datatype = descriptor(KEY_datatype).str
        val version = descriptor("v").num.toInt
        if (version != 1 || datatype != expectedDataType)
          Left("Data layout not supported")
        else Right(descriptor)
      }
    }

  def readMatDataFromChannel[T: ST](
      channel: ReadableByteChannel,
      numRows: Int,
      numCols: Int,
      width: Int
  ): Either[String, Mat[T]] = {
    val bb = ByteBuffer
      .allocate(width * numRows * numCols)
      .order(ByteOrder.LITTLE_ENDIAN)
    readFully(bb, channel)
    parse[T](numRows * numCols, bb).right.flatMap { data =>
      if (data.size != numRows * numCols) {
        Left("Premature end of input")
      } else {
        Right(Mat(numRows, numCols, data.asInstanceOf[Array[T]]))
      }
    }
  }

  def readMatFromChannel[T: ST](
      channel: ReadableByteChannel
  ): Either[String, Mat[T]] = {
    readHeaderFromChannel[T](channel).right.flatMap { descriptor =>
      width[T].right.flatMap { width =>
        val numRows =
          descriptor
            .get(KEY_numrows)
            .map(_.num.toInt)
            .getOrElse(descriptor(KEY_rowix).arr.length)
        val numCols =
          descriptor
            .get(KEY_numcols)
            .map(_.num.toInt)
            .getOrElse(descriptor(KEY_colix).arr.length)
        val rowmajor = descriptor(KEY_rowmajor).bool
        if (!rowmajor)
          readFrameDataFromChannel(
            channel,
            IndexIntRange(numRows).map(_.toString),
            IndexIntRange(numCols).map(_.toString),
            width
          ).right.map(_.toMat)
        else readMatDataFromChannel(channel, numRows, numCols, width)
      }
    }
  }

  def readFrameDataFromChannel[T: ST](
      channel: ReadableByteChannel,
      rowIx: Index[String],
      colIx: Index[String],
      width: Int
  ) = {
    val cols = colIx.toSeq.map { cIx =>
      val bb = ByteBuffer
        .allocate(width * rowIx.length)
        .order(ByteOrder.LITTLE_ENDIAN)
      readFully(bb, channel)
      parse[T](rowIx.length, bb).flatMap { col =>
        if (col.size != rowIx.length) {
          Left("Premature end of input")
        } else {
          Right((cIx, Series(col.toVec, rowIx)))
        }
      }
    }
    val colsSequenced = sequence(cols)
    colsSequenced.map(cols => Frame(cols: _*))
  }

  def readFrameFromChannel[T: ST](
      channel: ReadableByteChannel
  ): Either[String, Frame[String, String, T]] = {
    readHeaderFromChannel[T](channel).right.flatMap { descriptor =>
      width[T].right.flatMap { width =>
        val colIx = Index(descriptor(KEY_colix).arr.map(_.str): _*)
        val rowIx = Index(descriptor(KEY_rowix).arr.map(_.str): _*)
        val rowmajor = descriptor(KEY_rowmajor).bool
        if (rowmajor)
          Left("Row-major data should be read into a Mat")
        else readFrameDataFromChannel(channel, rowIx, colIx, width)
      }
    }
  }

  class ByteChannel(src: ByteBuffer) extends ReadableByteChannel {
    def read(dst: ByteBuffer) = {
      var i = 0
      while (dst.hasRemaining() && src.hasRemaining()) {
        dst.put(src.get)
        i += 1
      }
      i
    }
    def isOpen(): Boolean = true
    def close(): Unit = ()
  }

  def readFrameFromArray[T: ST](
      array: Array[Byte]
  ): Either[String, Frame[String, String, T]] =
    readFrameFromChannel(new ByteChannel(ByteBuffer.wrap(array)))

  def readMatFromArray[T: ST](
      array: Array[Byte]
  ): Either[String, Mat[T]] =
    readMatFromChannel(new ByteChannel(ByteBuffer.wrap(array)))
}
