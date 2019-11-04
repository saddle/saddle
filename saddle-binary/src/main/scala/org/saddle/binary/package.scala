package org.saddle
import java.nio.{ByteBuffer, ByteOrder}

/** Binary serialization for Frame[String,String,T] or Mat[T] with primitive T
  *
  * The layout of binary format is as follows:
  * - The first 6 bytes are "SADDLE"
  * - The next unsigned byte is the major version
  * - The next unsigned byte is the minor version
  * - The next 4 bytes form a little endian integer as HEADER_LENGTH
  * - The next HEADER_LENGTH bytes form an UTF-8 string as the header.
  * - The header is a valid JSON object with the following fields:
  *   - v: numeric positive integer is the version of the header structure
  *   - colix : a JSON array of strings, it is the column index of the frame
  *   - rowix : a JSON array of strings, it is the row index of the frame
  *   - numrows: numeric positive integer, number of rows
  *   - numcols: numeric positive integer, number of cols
  *   - Either one of rowix or numrows may be missing
  *   - Either one of colix or numcols may be missing
  *   - rowmajor : a boolean, indicating whether the data is stored in row-major or col-major order
  *   - datatype : string, either "double", "long", "int", "float", "byte"
  * - The header is padded with spaces (0x20) such that HEADER_LENGTH+12 is divisible by 16.
  *   The count of spaces are included in HEADER_LENGTH.
  * - The next width * numRows * numCols bytes form a little endian primitive array in row-major
  *   or col-major order. numRows and numCols are determined from the rowix/numrows and colix/numcols
  *   header fields. width is determined from the datatype field (8 for double and long, 4 for
  *   int and float, 1 for byte)
  */
package object binary {

  private def int(i: Int) = {
    java.nio.ByteBuffer
      .allocate(4)
      .order(ByteOrder.LITTLE_ENDIAN)
      .putInt(i)
      .array
  }

  def deserialize(
      array: Array[Byte]
  ): Either[String, Frame[String, String, Double]] = {
    val buffer = ByteBuffer.wrap(array).order(ByteOrder.LITTLE_ENDIAN)
    val magic = Array.ofDim[Byte](6)
    buffer.get(magic)
    val major = buffer.get
    val minor = buffer.get
    if (major != 1 || minor != 0) Left("Version of file format not supported")
    else {
      val headerLength = buffer.getInt
      val headerArray = Array.ofDim[Byte](headerLength)
      buffer.get(headerArray)
      val header = new String(headerArray, "UTF-8")
      val descriptor = ujson.read(header).obj
      val colIx = Index(descriptor("colix").arr.map(_.str): _*)
      val rowIx = Index(descriptor("rowix").arr.map(_.str): _*)
      val numRows = rowIx.length
      val rowmajor = descriptor("rowmajor").bool
      val datatype = descriptor("datatype").str
      val version = descriptor("v").num.toInt
      if (version != 1 || datatype != "double" || rowmajor)
        Left("Data layout not supported")
      else {
        if (buffer.remaining < numRows * colIx.length * 8)
          Left("Premature end of data")
        else {
          val cols = colIx.toSeq.map { cIx =>
            val col = Array.ofDim[Double](numRows)
            var i = 0
            while (i < numRows) {
              col(i) = buffer.getDouble
              i += 1
            }
            (cIx, Series(col.toVec, rowIx))
          }
          Right(Frame(cols: _*))
        }
      }
    }

  }

  def serialize(frame: Frame[String, String, Double]): Array[Byte] = {
    val descriptor = {
      val json = ujson
        .write(
          ujson.Obj(
            "datatype" -> "double",
            "v" -> 1,
            "colix" -> frame.colIx.toSeq,
            "rowix" -> frame.rowIx.toSeq,
            "rowmajor" -> false
          )
        )
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
    val result =
      Array.ofDim[Byte](header.length + 8 * frame.numRows * frame.numCols)
    System.arraycopy(header, 0, result, 0, header.length)
    val bb = ByteBuffer.wrap(result).order(ByteOrder.LITTLE_ENDIAN)
    bb.position(header.length)

    frame.values.foreach { col =>
      var i = 0
      val n = col.length
      while (i < n) {
        bb.putDouble(col.raw(i))
        i += 1
      }
    }
    result

  }

}
