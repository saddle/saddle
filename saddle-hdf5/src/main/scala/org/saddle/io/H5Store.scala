/**
 * Copyright (c) 2013 Saddle Development Team
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
 **/

package org.saddle.io

import java.nio.file.{ Files, Paths }
import ncsa.hdf.hdf5lib.{ H5, HDF5Constants }
import ncsa.hdf.hdf5lib.exceptions.HDF5LibraryException

import org.saddle._
import org.saddle.index.{IndexTime, IndexLong}
import org.joda.time.DateTime

import java.util.concurrent.locks.ReentrantLock

import scala.util.control.Exception._

/**
 * Implements (thread-safe) HDF5 I/O functionality for Series and Frames.
 *
 * We serialize all reads and writes to the underlying library as recommended here:
 *
 * http://www.hdfgroup.org/hdf-java-html/JNI/
 *   -- "Following the JNI specification, the recommended procedure is to use Java
 *       synchronization to create a monitor to serialize access to the library."
 */
object H5Store {
  private val monitor = new ReentrantLock()
  private def withMonitor[A](block: ⇒ A): A = {
    monitor.lock()
    try { block } catch {
      case e: HDF5LibraryException ⇒ throw wrapHdf5Exception(e)
    } finally { monitor.unlock() }
  }

  // *** Public API

  // ** Reading

  /**
   * Read a Series from an HDF5 file.
   * @param path Path to file to read
   * @param name Name of hdf5 group holding series data
   * @tparam X Series index type
   * @tparam T Series values type
   */
  def readSeries[X: ST: ORD, T: ST](path: String, name: String): Series[X, T] = withMonitor {
    readPandasSeries[X, T](path, name)
  }

  /**
   * Read a Series slice from an HDF5 file. Note that this still loads the entire series into
   * memory, so this is NOT meant to work with very large data chunks.
   *
   * @param path Path to file to read
   * @param name Name of hdf5 group holding series data
   * @param from Starting index of slice
   * @param to Ending index of slice
   * @param inclusive Whether to include or exclude ending index label (default true)
   * @tparam X Series index type
   * @tparam T Series values type
   */
  def readSeriesSlice[X: ST: ORD, T: ST](path: String, name: String,
                                         from: X, to: X, inclusive: Boolean): Series[X, T] = withMonitor {
    val series = readPandasSeries[X, T](path, name)
    series.sliceBy(from, to, inclusive)
  }

  /**
   * Read a Frame from an HDF5 file.
   * @param path Path to file to read
   * @param name Name of hdf5 group holding frame data
   * @tparam RX Frame row index type
   * @tparam CX Frame col index type
   * @tparam T Frame values type
   */
  def readFrame[RX: ST: ORD, CX: ST: ORD, T: ST](path: String, name: String): Frame[RX, CX, T] = withMonitor {
    readPandasFrame[RX, CX, T](path, name)
  }

  /**
   * Read a Frame slice from an HDF5 file. Note that the whole frame is still read into memory,
   * so this isn't meant for working with huge data.
   *
   * @param path Path to file to read
   * @param name Name of hdf5 group holding frame data
   * @param rowFrom row to start slice
   * @param rowTo row to end slice
   * @param colFrom col to start slice
   * @param colTo col to end slice
   * @param rowInclusive whether to include or exclude rowTo label in slice
   * @param colInclusive whether to include or exclude colTo label in slice
   * @tparam RX Frame row index type
   * @tparam CX Frame col index type
   * @tparam T Frame values type
   */
  def readFrameSlice[RX: ST: ORD, CX: ST: ORD, T: ST](path: String, name: String,
                                                      rowFrom: RX, rowTo: RX, colFrom: CX, colTo: CX,
                                                      rowInclusive: Boolean, colInclusive: Boolean): Frame[RX, CX, T] = withMonitor {
    val fr = readPandasFrame[RX, CX, T](path, name)
    fr.colSliceBy(colFrom, colTo, colInclusive).rowSliceBy(rowFrom, rowTo, rowInclusive)
  }

  /**
   * Read a Series from an already-open HDF5 file.
   * @param fileid HDF5 file handle returned from `openFile`
   * @param name Name of hdf5 group holding series data
   * @tparam X Series index type
   * @tparam T Series values type
   */
  def readSeries[X: ST: ORD, T: ST](fileid: Int, name: String): Series[X, T] = withMonitor {
    readPandasSeries[X, T](fileid, name)
  }

  /**
   * Read a Series slice from an HDF5 file. Note that this still loads the entire series into
   * memory, so this is NOT meant to work with very large data chunks.
   *
   * @param fileid HDF5 file handle returned from `openFile`
   * @param name Name of hdf5 group holding series data
   * @param from Starting index of slice
   * @param to Ending index of slice
   * @param inclusive Whether to include or exclude ending index label (default true)
   * @tparam X Series index type
   * @tparam T Series values type
   */
  def readSeriesSlice[X: ST: ORD, T: ST](fileid: Int, name: String,
                                         from: X, to: X, inclusive: Boolean): Series[X, T] = withMonitor {
    val series = readPandasSeries[X, T](fileid, name)
    series.sliceBy(from, to, inclusive)
  }

  /**
   * Read a Frame from an HDF5 file.
   * @param fileid HDF5 file handle returned from `openFile`
   * @param name Name of hdf5 group holding frame data
   * @tparam RX Frame row index type
   * @tparam CX Frame col index type
   * @tparam T Frame values type
   */
  def readFrame[RX: ST: ORD, CX: ST: ORD, T: ST](fileid: Int, name: String): Frame[RX, CX, T] = withMonitor {
    readPandasFrame[RX, CX, T](fileid, name)
  }

  /**
   * Read a Frame slice from an HDF5 file. Note that the whole frame is still read into memory,
   * so this isn't meant for working with huge data.
   *
   * @param fileid HDF5 file handle returned from `openFile`
   * @param name Name of hdf5 group holding frame data
   * @param rowFrom row to start slice
   * @param rowTo row to end slice
   * @param colFrom col to start slice
   * @param colTo col to end slice
   * @param rowInclusive whether to include or exclude rowTo label in slice
   * @param colInclusive whether to include or exclude colTo label in slice
   * @tparam RX Frame row index type
   * @tparam CX Frame col index type
   * @tparam T Frame values type
   */
  def readFrameSlice[RX: ST: ORD, CX: ST: ORD, T: ST](fileid: Int, name: String,
                                                      rowFrom: RX, rowTo: RX, colFrom: CX, colTo: CX,
                                                      rowInclusive: Boolean, colInclusive: Boolean): Frame[RX, CX, T] = withMonitor {
    val fr = readPandasFrame[RX, CX, T](fileid, name)
    fr.colSliceBy(colFrom, colTo, colInclusive).rowSliceBy(rowFrom, rowTo, rowInclusive)
  }

  // ** writing

  /**
   * Write a Series to an HDF5 file.
   * @param path Path to file to read
   * @param name Name of hdf5 group to hold series data
   * @tparam X Series index type
   * @tparam T Series values type
   */
  def writeSeries[X: ST: ORD, T: ST](path: String, name: String, s: Series[X, T]): Unit = withMonitor {
    writePandasSeries(path, name, s.index, s.values)
  }

  /**
   * Write a Frame to an HDF5 file.
   * @param path Path to file to read
   * @param name Name of hdf5 group to hold frame data
   * @tparam R Frame row index type
   * @tparam C Frame col index type
   * @tparam T Framevalues type
   */
  def writeFrame[R: ST: ORD, C: ST: ORD, T: ST](path: String, name: String, df: Frame[R, C, T]): Unit = withMonitor {
    writePandasFrame(path, name, df)
  }

  /**
   * Write a Series to an already-open HDF5 file.
   * @param fileid HDF5 file handle returned from `openFile`
   * @param name Name of hdf5 group to hold series data
   * @tparam X Series index type
   * @tparam T Series values type
   */
  def writeSeries[X: ST: ORD, T: ST](fileid: Int, name: String, s: Series[X, T]): Unit = withMonitor {
    writePandasSeries(fileid, name, s.index, s.values)
  }

  /**
   * Write a Frame to an HDF5 file.
   * @param fileid HDF5 file handle returned from `openFile`
   * @param name Name of hdf5 group to hold frame data
   * @tparam R Frame row index type
   * @tparam C Frame col index type
   * @tparam T Framevalues type
   */
  def writeFrame[R: ST: ORD, C: ST: ORD, T: ST](fileid: Int, name: String, df: Frame[R, C, T]): Unit = withMonitor {
    writePandasFrame(fileid, name, df)
  }

  /**
   * Read names of the groups at some level of the file hierarchy
   * @param path Path of file
   * @param root Level (group) of the hierarchy
   */
  def readGroupNames(path: String, root: String = "/"): List[String] = withMonitor {
    val fileid = openFile(path)
    assertException(fileid >= 0, "File ID : " + fileid + " does not belong to a valid file")
    H5Reg.save(fileid, H5F)

    val gcount = H5.H5Gn_members(fileid, root)
    assertException(gcount >= 0, "Failed to get iterator at " + root)

    val ab = List.newBuilder[String]
    ab.sizeHint(gcount)

    val oName = Array.fill[String](1)("")
    val oType = Array.fill[Int](1)(0)
    for (i <- Range(0, gcount)) {
      H5.H5Gget_obj_info_idx(fileid, root, i, oName, oType)
      ab += oName(0)
    }

    H5Reg.close(fileid, H5F)
    ab.result()
  }

  /**
   * Read names of the groups at some level of the file hierarchy, given an open file
   * @param fileid File handle from `openFile`
   * @param root Level (group) of the hierarchy
   */
  def readGroupNamesFid(fileid: Int, root: String = "/"): List[String] = withMonitor {
    val gcount = H5.H5Gn_members(fileid, root)
    assertException(gcount >= 0, "Failed to get iterator at " + root)

    val ab = List.newBuilder[String]
    ab.sizeHint(gcount)

    val oName = Array.fill[String](1)("")
    val oType = Array.fill[Int](1)(0)
    for (i <- Range(0, gcount)) {
      H5.H5Gget_obj_info_idx(fileid, root, i, oName, oType)
      ab += oName(0)
    }

    ab.result()
  }

  // ** open / close files

  /**
   * Open an HDF5 file and return an integer handle.
   * @param path Path of file
   */
  def openFile(path: String, readOnly: Boolean = true): Int = withMonitor {
    val rwParam = if (readOnly) HDF5Constants.H5F_ACC_RDONLY else HDF5Constants.H5F_ACC_RDWR
    val fid = H5.H5Fopen(path, rwParam, HDF5Constants.H5P_DEFAULT)
    assertException(fid >= 0, "Could not open file " + path)
    // H5Reg.save(fid, H5F) <-- don't want fid automatically released on error
    fid
  }

  /**
   * Create an HDF5 file and return an integer handle.
   * @param path Path of file
   */
  def createFile(path: String): Int = withMonitor {
    val fid = H5.H5Fcreate(path, HDF5Constants.H5F_ACC_EXCL, HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT)
    assertException(fid >= 0, "Could not create file " + path)
    // H5Reg.save(fid, H5F) <-- don't want fid automatically released on error
    fid
  }

  /**
   * Close an HDF5 file and return an integer handle.
   * @param fileid Integer handle of file
   */
  def closeFile(fileid: Int): Unit = withMonitor {
    assertException(fileid >= 0, "File ID : " + fileid + " does not belong to a valid file")
    H5.H5Fclose(fileid)
  }

  /**
   * Query for number of open resource handles held by HDF5 Library
   */
  def openResourceCount: Int = withMonitor {
    ncsa.hdf.hdf5lib.H5.getOpenIDCount
  }

  // *** private helper functions

  // release resources / cleanup on JVM shutdown
  Runtime.getRuntime.addShutdownHook(new Thread() {
    override def run(): Unit = withMonitor {
      H5Reg.closeAll()
    }
  })

  private val ic = classOf[Int]
  private val lc = classOf[Long]
  private val dc = classOf[Double]
  private val tc = classOf[DateTime]
  private val sc = classOf[String]

  private def wrapHdf5Exception(e: HDF5LibraryException): H5StoreException = {
    H5Reg.closeAll()
    H5StoreException(
      """Exception: %s
        |------------- HDF5 Stack --------------
        |%s
        |---------------------------------------""".stripMargin.format(e.getMessage, e.getStackTraceString))
  }

  private def openGroup(file_id: Int, group: String) = {
    assertException(file_id >= 0, "File ID : " + file_id + " does not belong to a valid file")
    val gid = H5.H5Gopen(file_id, group, HDF5Constants.H5P_DEFAULT)
    H5Reg.save(gid, H5G)
    gid
  }

  private def createGroup(file_id: Int, fullPath: String) = {
    assertException(file_id >= 0, "File ID : " + file_id + " does not belong to a valid file")
    val gid = H5.H5Gcreate(file_id, fullPath, HDF5Constants.H5P_DEFAULT,
      HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT)
    H5Reg.save(gid, H5G)
    gid
  }

  private def closeGroup(group_id: Int) {
    assertException(group_id >= 0, "Group ID : " + group_id + " does not belong to a valid group")

    H5Reg.close(group_id, H5G)
  }

  // attribute readers / writers

  // write a text attribute to a group: unicode warning, converts to ascii
  private def writeAttrText(group_id: Int, attr: String, text: String) {
    // create text constant type with particular size
    val tcsz = H5.H5Tcopy(HDF5Constants.H5T_C_S1)
    H5Reg.save(tcsz, H5T)

    val strB = text.getBytes(UTF8)
    H5.H5Tset_size(tcsz, strB.length.max(1))

    // open new scalar space for string constant
    val spaceToUse = H5.H5Screate(HDF5Constants.H5S_SCALAR)
    assertException(spaceToUse >= 0, "No space to write the attribute: " + attr)

    H5Reg.save(spaceToUse, H5S)

    val attribute_id = try {
      H5.H5Acreate(group_id, attr, tcsz,
        spaceToUse, HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT)
    }
    catch {
      case e: HDF5LibraryException => H5.H5Aopen(group_id, attr, HDF5Constants.H5P_DEFAULT)
    }

    H5Reg.save(attribute_id, H5A)

    // copy character buffer to byte stream
    val data = Array.ofDim[Byte](strB.length + 1)
    var i = 0
    while (i < strB.length) {
      data(i) = strB(i)
      i += 1
    }
    data(i) = 0.toByte

    H5.H5Awrite(attribute_id, tcsz, data)

    H5Reg.close(attribute_id, H5A)
    H5Reg.close(spaceToUse, H5S)
    H5Reg.close(tcsz, H5T)
  }

  private def readAttrText(group_id: Int, attr: String): String = {
    // ptr to attribute
    val attrid = H5.H5Aopen(group_id, attr, HDF5Constants.H5P_DEFAULT)
    assertException(attrid >= 0, "Attribute ID : " + attrid + " does not belong to a valid attribute")

    H5Reg.save(attrid, H5A)

    // make sure it's a text attribute
    val datatype = H5.H5Aget_type(attrid)
    assertException((datatype >= 0) && (H5.H5Tget_class(datatype) == HDF5Constants.H5T_STRING),
      "Attribute is not a text attribute. ")

    H5Reg.save(datatype, H5T)

    // get size for scalar text data
    val sz = H5.H5Tget_size(datatype)

    // open scalar space and make sure it is actually a scalar space
    val attrsp = H5.H5Aget_space(attrid)
    val dims = H5.H5Sget_simple_extent_ndims(attrsp)
    assertException(dims == 0, "Attribute space is not scalar")

    H5Reg.save(attrsp, H5S)

    // say we're storing as C string (null-terminated ascii sequence)
    val memtype_id = H5.H5Tcopy(HDF5Constants.H5T_C_S1)
    assertException(memtype_id >= 0, "Mem type is not valid")
    H5Reg.save(memtype_id, H5T)

    H5.H5Tset_size(memtype_id, sz)

    // allocate space
    val dset_data = Array.ofDim[Byte](sz)

    // read data
    H5.H5Aread(attrid, memtype_id, dset_data)

    // close resources
    H5Reg.close(attrid, H5A)
    H5Reg.close(attrsp, H5S)
    H5Reg.close(datatype, H5T)
    H5Reg.close(memtype_id, H5T)

    // convert to java string
    new String(dset_data, UTF8)
  }

  // write a long attribute to a group
  private def writeAttrLong(group_id: Int, attr: String, datum: Long) {
    // open new scalar space for long constant
    val spaceToUse = H5.H5Screate(HDF5Constants.H5S_SCALAR)
    assertException(spaceToUse >= 0, "No space to write the attribute: " + attr)

    H5Reg.save(spaceToUse, H5S)

    val attribute_id = try {
      H5.H5Acreate(group_id, attr, HDF5Constants.H5T_NATIVE_LONG,
        spaceToUse, HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT)
    }
    catch {
      case e: HDF5LibraryException => H5.H5Aopen(group_id, attr, HDF5Constants.H5P_DEFAULT)
    }
    assertException(attribute_id >= 0, "Bad attribute id")

    H5Reg.save(attribute_id, H5A)

    H5.H5Awrite(attribute_id, HDF5Constants.H5T_NATIVE_LONG, Array(datum))

    H5Reg.close(attribute_id, H5A)
    H5Reg.close(spaceToUse, H5S)
  }

  // read a long attribute from a group
  private def readAttrLong(group_id: Int, attr: String): Long = {
    // ptr to attribute
    val attrid = H5.H5Aopen(group_id, attr, HDF5Constants.H5P_DEFAULT)
    assertException(attrid >= 0, "Attribute ID : " + attrid + " does not belong to a valid attribute")

    H5Reg.save(attrid, H5A)

    // make sure it's a long attribute
    val datatype = H5.H5Aget_type(attrid)
    assertException((datatype >= 0) && (H5.H5Tget_class(datatype) == HDF5Constants.H5T_INTEGER),
      "Attribute is not a long attribute. ")

    H5Reg.save(datatype, H5A)

    // open scalar space and make sure it is actually a scalar space
    val attrsp = H5.H5Aget_space(attrid)
    val dims = H5.H5Sget_simple_extent_ndims(attrsp)
    assertException(dims == 0, "Attribute space is not scalar")

    H5Reg.save(attrsp, H5A)

    // hold result
    val result = Array.ofDim[Long](1)

    // read data
    H5.H5Aread(attrid, HDF5Constants.H5T_NATIVE_LONG, result)

    // close resources
    H5Reg.close(attrid, H5A)
    H5Reg.close(attrsp, H5A)
    H5Reg.close(datatype, H5A)

    result(0)
  }

  // data set readers / writers

  // write a one-dimensional array (dataset) to a group
  private def write1DArray[T: ST](group_id: Int, name: String, data: Array[T],
                                   withAttr: List[(String, String)] = Nil) {

    // create space for array
    val space_id = H5.H5Screate_simple(1, Array[Long](data.length), null)
    assertException(space_id >= 0, "No valid space to write data.")

    H5Reg.save(space_id, H5S)

    val dataset_id = writeArray(group_id, space_id, name, data, Array[Long](data.length))

    H5Reg.save(dataset_id, H5D)

    // write optional attributes to space
    for (attr <- withAttr)
      writeAttrText(dataset_id, attr._1, attr._2)

    H5Reg.close(space_id, H5S)
    H5Reg.close(dataset_id, H5D)
  }

  // write a two-dimensional array (dataset) to a group
  private def write2DArray[T: ST](group_id: Int, name: String, dim1: Int, dim2: Int, data: Array[T],
                                   withAttr: List[(String, String)] = Nil) {
    assertException(data.length == dim1 * dim2, "Data dimensions do not correspond to data length!")

    // create space for array
    val space_id = H5.H5Screate_simple(2, Array(dim1.toLong, dim2.toLong), null)
    assertException(space_id >= 0, "No valid space to write data.")

    H5Reg.save(space_id, H5S)

    val dataset_id = writeArray(group_id, space_id, name, data, Array[Long](dim1, dim2))

    H5Reg.save(dataset_id, H5D)

    // write optional attributes to space
    for (attr <- withAttr)
      writeAttrText(dataset_id, attr._1, attr._2)

    // close space
    H5Reg.close(space_id, H5S)
    H5Reg.close(dataset_id, H5D)
  }

  // common array-writing code
  private def writeArray[T: ST](group_id: Int, space_id: Int, name: String, data: Array[T],
                                 dataDims: Array[Long]): Int = {
    val stag = implicitly[ST[T]]

    // extract the (possibly transformed) data type of the array
    val (datatype_id, databuf) = stag.runtimeClass match {
      // handle the case where it's a string, convert to bytes
      case c if c == sc => {
        // the (necessarily uniform) record length should be the length of max string encoding
        val strid = H5.H5Tcopy(HDF5Constants.H5T_C_S1)
        H5Reg.save(strid, H5T)

        val maxsz = data.foldLeft(1) { (a, b) => a.max(b.asInstanceOf[String].getBytes(UTF8).length) }
        H5.H5Tset_size(strid, maxsz)

        // copy string as utf8 encoded bytes into buffer
        val byteBuff = new Array[Byte](data.length * maxsz)
        var i = 0
        data.foreach { s =>
          val asBytes = s.asInstanceOf[String].getBytes(UTF8) // "ISO-8859-1"
          System.arraycopy(asBytes, 0, byteBuff, i, asBytes.length)
          i += maxsz
        }

        (strid, byteBuff)
      }
      // in the case where it's a datetime, save as nanoseconds since unix epoch (Long)
      case c if c == tc => {
        val valArr = new Array[Long](data.length)

        var i = 0
        while (i < data.length) {
          // number of nanos since unix epoch
          valArr(i) = data(i).asInstanceOf[DateTime].getMillis * 1000000L
          i += 1
        }

        (HDF5Constants.H5T_NATIVE_INT64, valArr)
      }
      // otherwise, don't need a transform on data
      case c if c == ic => (HDF5Constants.H5T_NATIVE_INT32, data)
      case c if c == lc => (HDF5Constants.H5T_NATIVE_INT64, data)
      case c if c == dc => (HDF5Constants.H5T_NATIVE_DOUBLE, data)
      case _            => throw new IllegalArgumentException("Unsupported array type")
    }

    val cparms = H5.H5Pcreate(HDF5Constants.H5P_DATASET_CREATE)
    assertException(cparms >= 0, "Failure during params for dataset creation")
    H5Reg.save(cparms, H5P)

    val dataset_id = H5.H5Dcreate(group_id, name, datatype_id, space_id,
      HDF5Constants.H5P_DEFAULT, cparms, HDF5Constants.H5P_DEFAULT)
    assertException(dataset_id >= 0, "Failure during dataset creation")
    H5Reg.save(dataset_id, H5D)

    // write buffer data
    H5.H5Dwrite(dataset_id, datatype_id, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
      HDF5Constants.H5P_DEFAULT, databuf)

    // close temp resources
    H5Reg.close(datatype_id, H5T)
    H5Reg.close(cparms, H5P)

    // return dataset id; closed later
    dataset_id
  }

  // reads a dataset from file into an array in memory
  private def readArray[X: ST](grpid: Int, dsname: String): Array[X] = {
    // get dataset id
    val dsetid = H5.H5Dopen(grpid, dsname, HDF5Constants.H5P_DEFAULT)
    assertException(dsetid >= 0, "Dataset: " + dsname + " does not exist for this group")

    H5Reg.save(dsetid, H5D)

    val result = readArray[X](grpid, dsetid)

    H5Reg.close(dsetid, H5D)
    result
  }

  private case class Array2D[T](rows: Int, cols: Int, data: Array[T])

  // read a two-dimensional array (dataset) and return (rowcount, colcount, values)
  private def read2DArray[T: ST](
    group_id: Int, dsname: String): Array2D[T] = {
    // get dataset id
    val dsetid = H5.H5Dopen(group_id, dsname, HDF5Constants.H5P_DEFAULT)
    assertException(dsetid >= 0, "Dataset: " + dsname + " does not exist for this group")

    H5Reg.save(dsetid, H5D)

    // get space
    val dspaceid = H5.H5Dget_space(dsetid)
    assertException(dspaceid != 0, "Dataspace does not exist for dataset = " + dsetid)

    H5Reg.save(dspaceid, H5S)

    // assert it's two-dimensional
    val dims = H5.H5Sget_simple_extent_ndims(dspaceid)
    assertException(dims == 2, "Data is not 2 dimensional")

    // get length of arrays
    val sz = Array[Long](0, 0)
    H5.H5Sget_simple_extent_dims(dspaceid, sz, null)

    // get data type to switch on
    val datatype = H5.H5Dget_type(dsetid)
    assertException(datatype >= 0, "Not a valid datatype")

    H5Reg.save(datatype, H5T)

    val result = Array.ofDim[T](sz(0).toInt * sz(1).toInt)

    val stag = implicitly[ST[T]]

    val read_type = stag.runtimeClass match {
      // doubles
      case c if c == dc => {
        assertException(H5.H5Tget_class(datatype) == HDF5Constants.H5T_FLOAT, "Not a valid Double")
        HDF5Constants.H5T_NATIVE_DOUBLE
      }
      // ints
      case c if c == ic => {
        assertException(H5.H5Tget_class(datatype) == HDF5Constants.H5T_INTEGER, "Not a valid Integer")
        HDF5Constants.H5T_NATIVE_INT32
      }
      // longs
      case c if c == lc => {
        assertException(H5.H5Tget_class(datatype) == HDF5Constants.H5T_INTEGER, "Not a valid Long")
        HDF5Constants.H5T_NATIVE_INT64
      }
      case c if c == sc => {
        assertException(H5.H5Tget_class(datatype) == HDF5Constants.H5T_STRING, "Not a valid String")
        HDF5Constants.H5T_C_S1
      }
    }

    try {
      H5.H5Dread(dsetid, read_type,
        HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
        HDF5Constants.H5P_DEFAULT, result)
    } catch {
      case _:NullPointerException ⇒ // ignore
    } finally {
      H5Reg.close(datatype, H5T)
      H5Reg.close(dspaceid, H5S)
      H5Reg.close(dsetid, H5D)
    }

    // it's transposed, ie col-major order
    Array2D(sz(1).toInt, sz(0).toInt, result)
  }

  private def readArray[X: ST](grpid: Int, dsetid: Int): Array[X] = {
    // get space
    val dspaceid = H5.H5Dget_space(dsetid)
    assertException(dspaceid != 0, "Dataspace does not exist for dataset = " + dsetid)

    H5Reg.save(dspaceid, H5S)

    // assert it's one-dimensional
    val dims = H5.H5Sget_simple_extent_ndims(dspaceid)
    assertException(dims == 1, "Data is not 1 dimensional")

    // get length of array
    val sz = Array[Long](0)
    H5.H5Sget_simple_extent_dims(dspaceid, sz, null)
    val arrlen = sz(0).toInt

    // get data type to switch on
    val datatype = H5.H5Dget_type(dsetid)
    assertException(datatype >= 0, "Not a valid datatype")

    H5Reg.save(datatype, H5T)

    val stag = implicitly[ST[X]]

    val read_type = stag.runtimeClass match {
      // doubles
      case c if c == dc => {
        assertException(H5.H5Tget_class(datatype) == HDF5Constants.H5T_FLOAT, "Not a valid Double")
        HDF5Constants.H5T_NATIVE_DOUBLE
      }
      // ints
      case c if c == ic => {
        assertException(H5.H5Tget_class(datatype) == HDF5Constants.H5T_INTEGER, "Not a valid Integer")
        HDF5Constants.H5T_NATIVE_INT32
      }
      // longs
      case c if c == lc => {
        assertException(H5.H5Tget_class(datatype) == HDF5Constants.H5T_INTEGER, "Not a valid Long")
        HDF5Constants.H5T_NATIVE_INT64
      }
      // strings
      case c if c == sc => {
        assertException(H5.H5Tget_class(datatype) == HDF5Constants.H5T_STRING, "Not a valid String")

        // get string record size
        val sdim = H5.H5Tget_size(datatype)

        val memtype_id = H5.H5Tcopy(HDF5Constants.H5T_C_S1)
        H5.H5Tset_size(memtype_id, sdim)

        H5Reg.save(memtype_id, H5T)

        // copy strings as bytes into buffer
        val byteBuff = Array.ofDim[Byte](arrlen * sdim)
        val result = Array.ofDim[X](arrlen)

        H5.H5Dread(dsetid, memtype_id,
          HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
          HDF5Constants.H5P_DEFAULT, byteBuff)

        // copy from raw bytes to strings
        var j = 0
        var i = 0
        while (i < arrlen) {
          var sz = sdim
          while (sz - 1 >= 0 && byteBuff(j + sz - 1) == 0) {
            sz -= 1
          }
          val tmpBuf = Array.ofDim[Byte](sz)
          System.arraycopy(byteBuff, j, tmpBuf, 0, sz)
          result(i) = new String(tmpBuf, UTF8).asInstanceOf[X]
          j += sdim
          i += 1
        }

        // close resources, short-circuit and return
        H5Reg.close(memtype_id, H5T)
        H5Reg.close(datatype, H5T)
        H5Reg.close(dspaceid, H5S)

        return result
      }
      case _ => throw new IllegalArgumentException("Unrecognized array type")
    }

    // construct new array
    val result = Array.ofDim[X](arrlen)

    H5.H5Dread(dsetid, read_type,
      HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
      HDF5Constants.H5P_DEFAULT, result)

    H5Reg.close(datatype, H5T)
    H5Reg.close(dspaceid, H5S)

    result
  }

  // functions to make pandas-compatible readers & writers

  private def writeCommonHeader(rootId: Int) {
    writeAttrText(rootId, "CLASS", "GROUP")
    writeAttrText(rootId, "TITLE", "")
    writeAttrText(rootId, "VERSION", "1.0")
  }

  private def writePytablesHeader(rootId: Int) {
    writeCommonHeader(rootId)
    writeAttrText(rootId, "PYTABLES_FORMAT_VERSION", "2.0")
  }

  private def writeSeriesPandasHeader(grpid: Int) {
    writeCommonHeader(grpid)
    writeAttrText(grpid, "index_variety", "regular")
    writeAttrText(grpid, "name", "N.")
    writeAttrText(grpid, "pandas_type", "series")
  }

  private def writeFramePandasHeader(grpid: Int) {
    writeCommonHeader(grpid)
    writeAttrText(grpid, "axis0_variety", "regular")
    writeAttrText(grpid, "axis1_variety", "regular")
    writeAttrText(grpid, "block0_items_variety", "regular")
    writeAttrLong(grpid, "nblocks", 1)
    writeAttrLong(grpid, "ndim", 2)
    writeAttrText(grpid, "pandas_type", "frame")
  }

  private def getPandasSeriesAttribs = {
    List(("CLASS", "ARRAY"), ("FLAVOR", "numpy"), ("TITLE", ""), ("VERSION", "2.3"))
  }

  private def getPandasIndexAttribs[X: ST](index: Index[X]) = {
    val attribs = getPandasSeriesAttribs ++ List(("name", "N."))
    val stag = implicitly[ST[X]]
    attribs ++ {
      stag.runtimeClass match {
        case c if c == ic => List(("kind", "integer"))
        case c if c == lc => List(("kind", "integer"))
        case c if c == dc => List(("kind", "float"))
        case c if c == sc => List(("kind", "string"))
        // todo: add frequency attribute if one exists
        case c if c == tc => List(("index_class", "datetime"), ("kind", "datetime64"), ("freq", "N."))
        case _            => throw new IllegalArgumentException("Index type not recognized")
      }
    }
  }

  private def writePandasSeries[X: ST, T: ST](
    file: String, name: String, index: Index[X], values: Array[T]): Int = {

    val (fileid, writeHeader) = if (Files.exists(Paths.get(file))) {
      openFile(file, false) -> false
    }
    else {
      createFile(file) -> true
    }

    assertException(fileid >= 0, "File ID : " + fileid + " does not belong to a valid file")

    try {
      if (writeHeader) {
        val grpid = openGroup(fileid, "/")
        writePytablesHeader(grpid)
        closeGroup(grpid)
      }
      writePandasSeries[X, T](fileid, name, index, values)
    }
    finally {
      closeFile(fileid)
    }
  }

  private def writePandasSeries[X: ST, T: ST](
    fileid: Int, name: String, index: Index[X], values: Array[T]): Int = {
    assertException(fileid >= 0, "File ID : " + fileid + " does not belong to a valid file")

    val grpid = createGroup(fileid, "/" + name)
    writeSeriesPandasHeader(grpid)

    write1DArray(grpid, "index", index.toVec.contents, getPandasIndexAttribs(index))
    write1DArray(grpid, "values", values, getPandasSeriesAttribs)

    closeGroup(grpid)
    H5.H5Fflush(fileid, HDF5Constants.H5F_SCOPE_GLOBAL)
  }

  private def readPandasSeries[X: ST: ORD, T: ST](
    file: String, name: String): Series[X, T] = {

    val fileid = openFile(file)
    assertException(fileid >= 0, "File ID : " + fileid + " does not belong to a valid file")

    try {
      readPandasSeries[X, T](fileid, name)
    }
    finally {
      closeFile(fileid)
    }
  }

  private def readPandasSeries[X: ST: ORD, T: ST](
    fileid: Int, name: String): Series[X, T] = {
    val grpid = openGroup(fileid, name)
    assertException(grpid >= 0, "Group : " + name + " is not a valid group")

    val attr = readAttrText(grpid, "pandas_type")
    assertException(attr == "series", "Attribute is not a series")

    val vals = readArray[T](grpid, "values")

    val idxid = H5.H5Dopen(grpid, "index", HDF5Constants.H5P_DEFAULT)
    assertException(idxid >= 0, "index group is not valid")
    H5Reg.save(idxid, H5D)

    val ixtype = implicitly[ST[X]]

    // type-check the index
    readAttrText(idxid, "kind") match {
      case "integer"    => assertException(ixtype.runtimeClass == classOf[Long] ||
                                           ixtype.runtimeClass == classOf[Int], "Index is not a long/int")
      case "string"     => assertException(ixtype.runtimeClass == classOf[String], "Index is not a string")
      case "float"      => assertException(ixtype.runtimeClass == classOf[Double], "Index is not a float")
      case "datetime64" => assertException(ixtype.runtimeClass == classOf[DateTime], "Index is not a datetime64")
      case _@ t         => throw new IllegalArgumentException("Bad index type found: %s".format(t))
    }

    val index = ixtype.runtimeClass match {
      case x if x == tc => {
        val data = Vec(readArray[Long](grpid, idxid))
        new IndexTime(new IndexLong(data / 1000000)).asInstanceOf[Index[X]]
      }
      case _            => Index(readArray[X](grpid, idxid))
    }

    H5Reg.close(idxid, H5D)

    val result = Series(Vec(vals), index)

    closeGroup(grpid)
    result
  }

  private def writePandasFrame[R: ST: ORD, C: ST: ORD, T: ST](
    file: String, name: String, frame: Frame[R, C, T]): Int = {

    val (fileid, writeHeader) = if (Files.exists(Paths.get(file))) {
      openFile(file, false) -> false
    }
    else {
      createFile(file) -> true
    }

    assertException(fileid >= 0, "File ID : " + fileid + " does not belong to a valid file")

    try {
      if (writeHeader) {
        val grpid = openGroup(fileid, "/")
        writePytablesHeader(grpid)
        closeGroup(grpid)
      }
      writePandasFrame[R, C, T](fileid, name, frame)
    }
    finally {
      closeFile(fileid)
    }
  }

  private def writePandasFrame[R: ST: ORD, C: ST: ORD, T: ST](
    fileid: Int, name: String, frame: Frame[R, C, T]): Int = {
    // dissect frame into diffetent types, write to corresponding blocks.
    val dfDouble = frame.colType[Double] // block_0
    val dfInt    = frame.colType[Int]    // block_1
    val dfString = frame.colType[String] // block_2
    val valDouble = dfDouble.toMat
    val valInt    = dfInt.toMat
    val valString = dfString.toMat
    val grpid     = createGroup(fileid, "/" + name)
    writeFramePandasHeader(grpid)

    // axis 0 is column names
    write1DArray(grpid, "axis0", frame.colIx.toVec.contents, getPandasIndexAttribs(frame.colIx))

    // axis 1 is row names
    write1DArray(grpid, "axis1", frame.rowIx.toVec.contents, getPandasIndexAttribs(frame.rowIx))

    // TODO what to do with column contents that is not Double, Int or String?
    write1DArray(grpid, "block0_items", dfDouble.colIx.toVec.contents, getPandasIndexAttribs(dfDouble.colIx))
    write1DArray(grpid, "block1_items", dfInt.colIx.toVec.contents, getPandasIndexAttribs(dfInt.colIx))
    write1DArray(grpid, "block2_items", dfString.colIx.toVec.contents, getPandasIndexAttribs(dfString.colIx))

    // the data itself is stored in a transposed format (col-major order)
    write2DArray(grpid, "block0_values", valDouble.numRows, valDouble.numCols,
      valDouble.contents, getPandasSeriesAttribs)
    write2DArray(grpid, "block1_values", valInt.numRows, valInt.numCols,
      valInt.contents, getPandasSeriesAttribs)
    write2DArray(grpid, "block2_values", valString.numRows, valString.numCols,
      valString.contents, getPandasSeriesAttribs)

    closeGroup(grpid)
    H5.H5Fflush(fileid, HDF5Constants.H5F_SCOPE_GLOBAL)
  }

  private def readPandasFrame[RX: ST: ORD, CX: ST: ORD, T: ST](
    file: String, name: String): Frame[RX, CX, T] = {

    val fileid = openFile(file)
    assertException(fileid >= 0, "File ID : " + fileid + " does not belong to a valid file")

    try {
      readPandasFrame[RX, CX, T](fileid, name)
    } finally {
      closeFile(fileid)
    }
  }

  private def readPandasFrame[RX: ST: ORD, CX: ST: ORD, T: ST](
    fileid: Int, name: String): Frame[RX, CX, T] = {
    val grpid = openGroup(fileid, name)
    assertException(grpid >= 0, "Group : " + name + " is not a valid group")

    val attr = readAttrText(grpid, "pandas_type")
    assertException(attr == "frame", "Attribute is not a Frame")

    // the block manager in pandas has up to four blocks: Int64, Float64, Char (ie Int8), PyObject
    // since we only store doubles in QuantS for now, we assume only one block that comprises all
    // the columns.
    val arrDouble = read2DArray[Double](grpid, "block0_values")
    val arrInt    = read2DArray[Int](grpid,    "block1_values")
    val arrString = read2DArray[String](grpid, "block2_values")

    // data is stored transposed (ie, col-major order, so un-transpose it)
    val mxDouble = Mat(arrDouble.cols, arrDouble.rows, arrDouble.data)
    val mxInt    = Mat(arrInt.cols, arrInt.rows, arrInt.data)
    val mxString = Mat(arrString.cols, arrString.rows, arrString.data)

    val rxtype = implicitly[ST[RX]]
    val cxtype = implicitly[ST[CX]]

    val rowidx = H5.H5Dopen(grpid, "axis1", HDF5Constants.H5P_DEFAULT)
    assertException(rowidx >= 0, "row index group is not valid")

    H5Reg.save(rowidx, H5D)

    // type-check the indices
    readAttrText(rowidx, "kind") match {
      case "integer"    => assertException(rxtype.runtimeClass == classOf[Long] ||
                                           rxtype.runtimeClass == classOf[Int], "Row index is not a long/int")
      case "string"     => assertException(rxtype.runtimeClass == classOf[String], "Row index is not a string")
      case "float"      => assertException(rxtype.runtimeClass == classOf[Double], "Row index is not a float")
      case "datetime64" => assertException(rxtype.runtimeClass == classOf[DateTime], "Row index is not a joda datetime64")
      case _@ t         => throw new IllegalArgumentException("Bad row index type found: %s".format(t))
    }

    val colidx = H5.H5Dopen(grpid, "axis0", HDF5Constants.H5P_DEFAULT)
    val doubleColIdx = H5.H5Dopen(grpid, "block0_items", HDF5Constants.H5P_DEFAULT)
    val intColIdx = H5.H5Dopen(grpid, "block1_items", HDF5Constants.H5P_DEFAULT)
    val strColIdx = H5.H5Dopen(grpid, "block2_items", HDF5Constants.H5P_DEFAULT)
    assertException(colidx >= 0, "column index group is not valid")

    H5Reg.save(colidx, H5D)
    H5Reg.save(doubleColIdx, H5D)
    H5Reg.save(intColIdx, H5D)
    H5Reg.save(strColIdx, H5D)

    // type-check the indices
    readAttrText(colidx, "kind") match {
      case "integer"    => assertException(cxtype.runtimeClass == classOf[Long] ||
                                           cxtype.runtimeClass == classOf[Int], "Col index is not a long/int")
      case "string"     => assertException(cxtype.runtimeClass == classOf[String], "Col index is not a string")
      case "float"      => assertException(cxtype.runtimeClass == classOf[Float], "Col index is not a float")
      case "datetime64" => assertException(cxtype.runtimeClass == classOf[DateTime], "Col index is not a joda datetime64")
      case _@ t         => throw new IllegalArgumentException("Bad index type found: %s".format(t))
    }

    val ix0 = rxtype.runtimeClass match {
      case x if x == tc => {
        val data = Vec(readArray[Long](grpid, rowidx))
        new IndexTime(new IndexLong(data / 1000000)).asInstanceOf[Index[RX]]
      }
      case _            => {
        val data = Vec(readArray[RX](grpid, rowidx))
        Index(data)
      }
    }

    val ix1 = cxtype.runtimeClass match {
      case x if x == tc ⇒ {
        val data = Vec(readArray[Long](grpid, colidx))
        new IndexTime(new IndexLong(data  / 1000000)).asInstanceOf[Index[CX]]
      }
      case _ ⇒ {
        val doubleData = readArray[CX](grpid, doubleColIdx).toList
        val intData = readArray[CX](grpid, intColIdx).toList
        val strData = readArray[CX](grpid, strColIdx).toList
        val data = doubleData ++ intData ++ strData
        Index(Vec(data.toArray))
      }
    }

    H5Reg.close(rowidx, H5D)
    H5Reg.close(colidx, H5D)
    H5Reg.close(doubleColIdx, H5D)
    H5Reg.close(intColIdx, H5D)
    H5Reg.close(strColIdx, H5D)

    // Warning: type coercion madness ahead. I would love a better approach here.
    val doubleCols = mxDouble.cols.asInstanceOf[IndexedSeq[Vec[Any]]]
    val intCols    = mxInt.cols.asInstanceOf[IndexedSeq[Vec[Any]]]
    val strCols    = mxString.cols.asInstanceOf[IndexedSeq[Vec[Any]]]
    val mx = Mat((doubleCols ++ intCols ++ strCols).asInstanceOf[IndexedSeq[Vec[T]]]:_*)
    val result = Frame[RX, CX, T](mx, ix0, ix1)

    closeGroup(grpid)

    result
  }

  def assertException(condition: Boolean, errorMessage: String) {
    if (!condition) {
      H5Reg.closeAll()                          // release any held resources
      throw H5StoreException(errorMessage)      // throw wrapped exception
    }
  }

  /**
   * For wrapping any HDF5 exception
   */
  case class H5StoreException(msg: String) extends RuntimeException(msg)

  /**
   * Sealed trait representing resources that might be opened in an HDF5
   * file.
   */
  sealed trait H5Resource

  case object H5T extends H5Resource
  case object H5A extends H5Resource
  case object H5D extends H5Resource
  case object H5S extends H5Resource
  case object H5P extends H5Resource
  case object H5G extends H5Resource
  case object H5F extends H5Resource

  /**
   * Thread-safe registry for open HDF5 resources within a process.
   *
   * When acquiring resource, immediately register via H5Reg.save(handle, type). Then,
   * if an exception is thrown, closeAll() will free all the acquired H5 resources.
   */
  private object H5Reg {
    private var registry = Set.empty[(Int, H5Resource)]

    /**
     * Registers an open resource if it hasn't been registered already
     */
    def save(v: Int, t: H5Resource): Unit = withMonitor {
      registry += (v -> t)
    }

    /**
     * Closes / de-registers a resource in a thread-safe manner
     */
    def close(v: Int, t: H5Resource): Unit = withMonitor {
      if (registry.contains(v -> t)) {
        t match {
          case H5T => allCatch { H5.H5Tclose(v) }
          case H5A => allCatch { H5.H5Aclose(v) }
          case H5D => allCatch { H5.H5Dclose(v) }
          case H5S => allCatch { H5.H5Sclose(v) }
          case H5P => allCatch { H5.H5Pclose(v) }
          case H5G => allCatch { H5.H5Gclose(v) }
          case H5F => allCatch { H5.H5Fclose(v) }
        }
        registry -= (v -> t)
      }
    }

    /**
     * Releases all (open) resources
     */
    def closeAll(): Unit = withMonitor {
      registry.map { case (v, t) => close(v, t) }
    }
  }
}

