package org.saddle.binary
import org.saddle.scalar._
import org.saddle._
import java.nio.ByteOrder
import java.nio.ByteBuffer
import java.nio.channels.WritableByteChannel

object Writer {
  private def int(i: Int) = {
    ByteBuffer
      .allocate(4)
      .order(ByteOrder.LITTLE_ENDIAN)
      .putInt(i)
      .array
  }

  val KEY_datatype = "datatype"
  val KEY_numcols = "numcols"
  val KEY_numrows = "numrows"
  val KEY_rowmajor = "rowmajor"
  val KEY_v = "v"
  val KEY_colix = "colix"
  val KEY_rowix = "rowix"

  def createMatDescriptor[T: ST](
      mat: Mat[T]
  ) = dtype[T].right.map { dtype =>
    ujson
      .write(
        ujson.Obj(
          KEY_datatype -> dtype,
          KEY_numcols -> mat.numCols,
          KEY_numrows -> mat.numRows,
          KEY_rowmajor -> true,
          KEY_v -> 1
        )
      )
  }
  def createFrameDescriptor[RX, CX, T: ST](
      frame: Frame[RX, CX, T]
  ) = dtype[T].right.map { dtype =>
    ujson
      .write(
        ujson.Obj(
          KEY_datatype -> dtype,
          KEY_colix -> frame.colIx.toSeq.map(_.toString),
          KEY_rowix -> frame.rowIx.toSeq.map(_.toString),
          KEY_rowmajor -> false,
          KEY_v -> 1
        )
      )
  }

  def createHeader(
      descriptor: Either[String, String]
  ): Either[String, Array[Byte]] = {
    descriptor.right.map { descriptorJson =>
      val descriptor = {
        val json = descriptorJson
          .getBytes("UTF-8")
        val jsonLength = json.length
        val usefulLength = jsonLength + 12
        val padding = usefulLength / 16 + 16 - usefulLength
        json ++ Array.fill(padding)(' '.toByte)
      }
      val magic = "SADDLE".getBytes("US-ASCII")
      val version = Array(1.toByte, 0.toByte)
      val headerLength = int(descriptor.length)
      val header = magic ++ version ++ headerLength ++ descriptor
      header
    }
  }

  private[binary] def dtype[T: ST] = implicitly[ST[T]] match {
    case ScalarTagDouble => Right("double")
    case ScalarTagInt    => Right("int")
    case ScalarTagFloat  => Right("float")
    case ScalarTagLong   => Right("long")
    case ScalarTagByte   => Right("byte")
    case other           => Left(s"Type $other not supported.")
  }

  private[binary] def width[T: ST] = implicitly[ST[T]] match {
    case ScalarTagDouble => Right(8)
    case ScalarTagInt    => Right(4)
    case ScalarTagFloat  => Right(4)
    case ScalarTagLong   => Right(8)
    case ScalarTagByte   => Right(1)
    case other           => Left(s"Type $other not supported.")
  }

  def put[@specialized(Double, Long, Int, Float, Byte) T: ST](
      t: Array[T],
      bb: ByteBuffer
  ) = implicitly[ST[T]] match {
    case ScalarTagDouble =>
      Right {
        var i = 0
        val n = t.length
        while (i < n) {
          bb.putDouble(t(i))
          i += 1
        }
      }
    case ScalarTagInt =>
      Right {
        var i = 0
        val n = t.length
        while (i < n) {
          bb.putInt(t(i).asInstanceOf[Int])
          i += 1
        }
      }
    case ScalarTagFloat =>
      Right {
        var i = 0
        val n = t.length
        while (i < n) {
          bb.putFloat(t(i).asInstanceOf[Float])
          i += 1
        }
      }
    case ScalarTagLong =>
      Right {
        var i = 0
        val n = t.length
        while (i < n) {
          bb.putLong(t(i).asInstanceOf[Long])
          i += 1
        }
      }
    case ScalarTagByte =>
      Right {
        var i = 0
        val n = t.length
        while (i < n) {
          bb.put(t(i).asInstanceOf[Byte])
          i += 1
        }
      }
    case other => Left(s"Type $other not supported.")
  }

  def writeMatIntoChannel[T: ST](
      mat: Mat[T],
      channel: WritableByteChannel
  ): Either[String, Unit] = {
    val header = createHeader(createMatDescriptor(mat))
    header.right.flatMap { header =>
      width[T].right.map { width =>
        writeFully(ByteBuffer.wrap(header), channel)

        mat.rows().foreach { row =>
          val bb = ByteBuffer
            .allocate(row.length * width)
            .order(ByteOrder.LITTLE_ENDIAN)
          put(row.toArray, bb)
          writeFully(bb, channel)
        }
      }
    }
  }

  def writeFully(bb: ByteBuffer, channel: WritableByteChannel) = {
    bb.rewind
    while (bb.hasRemaining) {
      channel.write(bb)
    }
  }

  def writeFrameIntoChannel[RX, CX, T: ST](
      frame: Frame[RX, CX, T],
      channel: WritableByteChannel
  ): Either[String, Unit] = {

    val header = createHeader(createFrameDescriptor(frame))
    header.right.flatMap { header =>
      width[T].right.map { width =>
        writeFully(ByteBuffer.wrap(header), channel)

        frame.values.foreach { col =>
          val bb = ByteBuffer
            .allocate(col.length * width)
            .order(ByteOrder.LITTLE_ENDIAN)
          put(col.toArray, bb)
          writeFully(bb, channel)
        }
      }
    }
  }

  def writeMatIntoArray[T: ST](
      mat: Mat[T]
  ): Either[String, Array[Byte]] = {
    val header = createHeader(createMatDescriptor(mat))
    header.right.flatMap { header =>
      width[T].right.map { width =>
        val result =
          Array.ofDim[Byte](header.length + width * mat.numRows * mat.numCols)
        System.arraycopy(header, 0, result, 0, header.length)
        val bb = ByteBuffer.wrap(result).order(ByteOrder.LITTLE_ENDIAN)
        bb.position(header.length)

        val ar = mat.toArray
        put(ar, bb)
        result
      }
    }
  }
  def writeFrameIntoArray[RX, CX, T: ST](
      frame: Frame[RX, CX, T]
  ): Either[String, Array[Byte]] = {
    val header = createHeader(createFrameDescriptor(frame))
    header.right.flatMap { header =>
      width[T].right.map { width =>
        val result =
          Array.ofDim[Byte](
            header.length + width * frame.numRows * frame.numCols
          )
        System.arraycopy(header, 0, result, 0, header.length)
        val bb = ByteBuffer.wrap(result).order(ByteOrder.LITTLE_ENDIAN)
        bb.position(header.length)

        frame.values.foreach { col =>
          put(col.toArray, bb)
        }
        result
      }
    }
  }

}
