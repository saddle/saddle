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

import java.io.File
import ncsa.hdf.hdf5lib.{ H5, HDF5Constants }
import ncsa.hdf.hdf5lib.exceptions.HDF5LibraryException

import org.saddle._
import org.joda.time.DateTime

/**
 * Implements HDF5 I/O functionality for Series and Frames
 */
object H5Store {
  // *** public functions

  // ** reading

  /**
   * Read a Series from an HDF5 file.
   * @param path Path to file to read
   * @param name Name of hdf5 group holding series data
   * @tparam X Series index type
   * @tparam T Series values type
   */
  def readSeries[X: CLM: ORD, T: CLM](
    path: String, name: String) = readPandasSeries[X, T](path, name)

  /**
   * Read a Frame from an HDF5 file.
   * @param path Path to file to read
   * @param name Name of hdf5 group holding frame data
   * @tparam RX Frame row index type
   * @tparam CX Frame col index type
   * @tparam T Frame values type
   */
  def readFrame[RX: CLM: ORD, CX: CLM: ORD, T: CLM](
    path: String, name: String) = readPandasFrame[RX, CX, T](path, name)

  // read from already-open file

  /**
   * Read a Series from an already-open HDF5 file.
   * @param fileid HDF5 file handle returned from `openFile`
   * @param name Name of hdf5 group holding series data
   * @tparam X Series index type
   * @tparam T Series values type
   */
  def readSeries[X: CLM: ORD, T: CLM](
    fileid: Int, name: String) = readPandasSeries[X, T](fileid, name)

  /**
   * Read a Frame from an HDF5 file.
   * @param fileid HDF5 file handle returned from `openFile`
   * @param name Name of hdf5 group holding frame data
   * @tparam RX Frame row index type
   * @tparam CX Frame col index type
   * @tparam T Frame values type
   */
  def readFrame[RX: CLM: ORD, CX: CLM: ORD, T: CLM](
    fileid: Int, name: String) = readPandasFrame[RX, CX, T](fileid, name)

  // ** writing

  /**
   * Write a Series to an HDF5 file.
   * @param path Path to file to read
   * @param name Name of hdf5 group to hold series data
   * @tparam X Series index type
   * @tparam T Series values type
   */
  def writeSeries[X: CLM: ORD, T: CLM](
    path: String, name: String, s: Series[X, T]): Int = {
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
  def writeFrame[R: CLM: ORD, C: CLM: ORD, T: CLM](
    path: String, name: String, df: Frame[R, C, T]): Int = {
    writePandasFrame(path, name, df.rowIx, df.colIx, df.toMat)
  }

  // write to already-open file

  /**
   * Write a Series to an already-open HDF5 file.
   * @param fileid HDF5 file handle returned from `openFile`
   * @param name Name of hdf5 group to hold series data
   * @tparam X Series index type
   * @tparam T Series values type
   */
  def writeSeries[X: CLM: ORD, T: CLM](
    fileid: Int, name: String, s: Series[X, T]) =
    writePandasSeries(fileid, name, s.index, s.values)

  /**
   * Write a Frame to an HDF5 file.
   * @param fileid HDF5 file handle returned from `openFile`
   * @param name Name of hdf5 group to hold frame data
   * @tparam R Frame row index type
   * @tparam C Frame col index type
   * @tparam T Framevalues type
   */
  def writeFrame[R: CLM: ORD, C: CLM: ORD, T: CLM](
    fileid: Int, name: String, df: Frame[R, C, T]) =
    writePandasFrame(fileid, name, df.rowIx, df.colIx, df.toMat)

  /**
   * Read names of the groups at some level of the file hierarchy
   * @param path Path of file
   * @param root Level (group) of the hierarchy
   */
  def readGroupNames(path: String, root: String = "/"): List[String] = {
    val fileid = openFile(path)
    assertException(fileid >= 0, "File ID : " + fileid + " does not belong to a valid file")

    H5Reg(fileid, "F")

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

    closeFile(H5Reg(fileid))

    ab.result()
  }

  /**
   * Read names of the groups at some level of the file hierarchy, given an open file
   * @param fileid File handle from `openFile`
   * @param root Level (group) of the hierarchy
   */
  def readGroupNamesFid(fileid: Int, root: String = "/"): List[String] = {
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
  def openFile(path: String): Int = {
    // commented code supposedly sets up a memory-mapped file, but I don't see that it has any effect
    // val fapl_id = H5.H5Pcreate(HDF5Constants.H5P_FILE_ACCESS)
    // val acc_id = H5.H5Pset_fapl_core(fapl_id, 1024 * 1024, false)
    val fid = H5.H5Fopen(path, HDF5Constants.H5F_ACC_RDWR, HDF5Constants.H5P_DEFAULT /* acc_id */ )
    assertException(fid >= 0, "Could not open file " + path)
    H5Reg(fid, "F")
    fid
  }

  /**
   * Create an HDF5 file and return an integer handle.
   * @param path Path of file
   */
  def createFile(path: String): Int = {
    val fid = H5.H5Fcreate(path, HDF5Constants.H5F_ACC_TRUNC, HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT)
    assertException(fid >= 0, "Could not create file " + path)
    H5Reg(fid, "F")
    fid
  }

  /**
   * Close an HDF5 file and return an integer handle.
   * @param fileid Integer handle of file
   */
  def closeFile(fileid: Int): Int = {
    assertException(fileid >= 0, "File ID : " + fileid + " does not belong to a valid file")
    H5.H5Fclose(H5Reg(fileid))
  }

  // *** private helper functions

  private val ic = classOf[Int]
  private val lc = classOf[Long]
  private val dc = classOf[Double]
  private val tc = classOf[DateTime]
  private val sc = classOf[String]

  private def openGroup(file_id: Int, group: String) = {
    assertException(file_id >= 0, "File ID : " + file_id + " does not belong to a valid file")
    val gid = H5.H5Gopen(file_id, group, HDF5Constants.H5P_DEFAULT)
    H5Reg(gid, "G")
    gid
  }

  private def createGroup(file_id: Int, fullPath: String) = {
    assertException(file_id >= 0, "File ID : " + file_id + " does not belong to a valid file")
    val gid = H5.H5Gcreate(file_id, fullPath, HDF5Constants.H5P_DEFAULT,
      HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT)
    H5Reg(gid, "G")
    gid
  }

  private def closeGroup(group_id: Int) = {
    assertException(group_id >= 0, "Group ID : " + group_id + " does not belong to a valid group")
    H5.H5Gclose(H5Reg(group_id))
  }

  // attribute readers / writers

  // write a text attribute to a group: unicode warning, converts to ascii
  private def writeAttrText(group_id: Int, attr: String, text: String) = {
    // create text constant type with particular size
    val tcsz = H5.H5Tcopy(HDF5Constants.H5T_C_S1)
    val strB = text.getBytes("UTF-8")
    H5.H5Tset_size(tcsz, strB.length.max(1))

    H5Reg(tcsz, "T")

    // open new scalar space for string constant
    val spaceToUse = H5.H5Screate(HDF5Constants.H5S_SCALAR)
    assertException(spaceToUse >= 0, "No space to write the attribute: " + attr)

    H5Reg(spaceToUse, "S")

    val attribute_id = try {
      H5.H5Acreate(group_id, attr, tcsz,
        spaceToUse, HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT)
    }
    catch {
      case e: HDF5LibraryException => H5.H5Aopen(group_id, attr, HDF5Constants.H5P_DEFAULT)
    }

    H5Reg(attribute_id, "A")

    // copy character buffer to byte stream
    val data = Array.ofDim[Byte](strB.length + 1)
    var i = 0
    while (i < strB.length) {
      data(i) = strB(i)
      i += 1
    }
    data(i) = 0.toByte

    H5.H5Awrite(attribute_id, tcsz, data)

    H5.H5Aclose(H5Reg(attribute_id))
    H5.H5Sclose(H5Reg(spaceToUse))
    H5.H5Tclose(H5Reg(tcsz))
  }

  private def readAttrText(group_id: Int, attr: String): String = {
    // ptr to attribute
    val attrid = H5.H5Aopen(group_id, attr, HDF5Constants.H5P_DEFAULT)
    assertException(attrid >= 0, "Attribute ID : " + attrid + " does not belong to a valid attribute")

    H5Reg(attrid, "A")

    // make sure it's a text attribute
    val datatype = H5.H5Aget_type(attrid)
    assertException((datatype >= 0) && (H5.H5Tget_class(datatype) == HDF5Constants.H5T_STRING),
      "Attribute is not a text attribute. ")

    H5Reg(datatype, "A")

    // get size for scalar text data
    val sz = H5.H5Tget_size(datatype)

    // open scalar space and make sure it is actually a scalar space
    val attrsp = H5.H5Aget_space(attrid)
    val dims = H5.H5Sget_simple_extent_ndims(attrsp)
    assertException(dims == 0, "Attribute space is not scalar")

    H5Reg(attrsp, "A")

    // say we're storing as C string (null-terminated ascii sequence)
    val memtype_id = H5.H5Tcopy(HDF5Constants.H5T_C_S1)
    assertException(memtype_id >= 0, "Mem type is not valid")
    H5.H5Tset_size(memtype_id, sz)

    H5Reg(memtype_id, "T")

    // allocate space
    val dset_data = Array.ofDim[Byte](sz)

    // read data
    H5.H5Aread(attrid, memtype_id, dset_data)

    // close resources
    H5.H5Aclose(H5Reg(attrid))
    H5.H5Sclose(H5Reg(attrsp))
    H5.H5Tclose(H5Reg(datatype))
    H5.H5Tclose(H5Reg(memtype_id))

    // convert to java string
    new String(dset_data, "UTF-8")
  }

  // write a long attribute to a group
  private def writeAttrLong(group_id: Int, attr: String, datum: Long) = {
    // open new scalar space for long constant
    val spaceToUse = H5.H5Screate(HDF5Constants.H5S_SCALAR)
    assertException(spaceToUse >= 0, "No space to write the attribute: " + attr)

    H5Reg(spaceToUse, "S")

    val attribute_id = try {
      H5.H5Acreate(group_id, attr, HDF5Constants.H5T_NATIVE_LONG,
        spaceToUse, HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT)
    }
    catch {
      case e: HDF5LibraryException => H5.H5Aopen(group_id, attr, HDF5Constants.H5P_DEFAULT)
    }
    assertException(attribute_id >= 0, "Bad attribute id")

    H5Reg(attribute_id, "A")

    H5.H5Awrite(attribute_id, HDF5Constants.H5T_NATIVE_LONG, Array(datum))

    H5.H5Aclose(H5Reg(attribute_id))
    H5.H5Sclose(H5Reg(spaceToUse))
  }

  // read a long attribute from a group
  private def readAttrLong(group_id: Int, attr: String): Long = {
    // ptr to attribute
    val attrid = H5.H5Aopen(group_id, attr, HDF5Constants.H5P_DEFAULT)
    assertException(attrid >= 0, "Attribute ID : " + attrid + " does not belong to a valid attribute")

    H5Reg(attrid, "A")

    // make sure it's a long attribute
    val datatype = H5.H5Aget_type(attrid)
    assertException((datatype >= 0) && (H5.H5Tget_class(datatype) == HDF5Constants.H5T_INTEGER),
      "Attribute is not a long attribute. ")

    H5Reg(datatype, "A")

    // open scalar space and make sure it is actually a scalar space
    val attrsp = H5.H5Aget_space(attrid)
    val dims = H5.H5Sget_simple_extent_ndims(attrsp)
    assertException(dims == 0, "Attribute space is not scalar")

    H5Reg(attrsp, "A")

    // hold result
    val result = Array.ofDim[Long](1)

    // read data
    H5.H5Aread(attrid, HDF5Constants.H5T_NATIVE_LONG, result)

    // close resources
    H5.H5Aclose(H5Reg(attrid))
    H5.H5Sclose(H5Reg(attrsp))
    H5.H5Tclose(H5Reg(datatype))

    result(0)
  }

  // data set readers / writers

  // write a one-dimensional array (dataset) to a group
  private def write1DArray[T: CLM](group_id: Int, name: String, data: Array[T],
                                   withAttr: List[(String, String)] = Nil) = {

    assertException(data.length > 0, "No data to write!")

    // create space for array, with possible unlimited extent resizing
    val space_id = H5.H5Screate_simple(1, Array[Long](data.size), Array[Long](HDF5Constants.H5S_UNLIMITED))
    assertException(space_id >= 0, "No valid space to write data.")

    H5Reg(space_id, "S")

    val dataset_id = writeArray(group_id, space_id, name, data, Array[Long](data.size))

    H5Reg(dataset_id, "D")

    // write optional attributes to space
    for (attr <- withAttr)
      writeAttrText(dataset_id, attr._1, attr._2)

    H5.H5Sclose(H5Reg(space_id))
    H5.H5Dclose(H5Reg(dataset_id))
  }

  // write a two-dimensional array (dataset) to a group
  private def write2DArray[T: CLM](group_id: Int, name: String, dim1: Int, dim2: Int, data: Array[T],
                                   withAttr: List[(String, String)] = Nil) = {
    assertException(data.length > 0, "No data to write!")
    assertException(data.length == dim1 * dim2, "Data dimensions do not correspond to data length!")

    // create space for array, with possible unlimited extent for resizing
    val space_id = H5.H5Screate_simple(2, Array(dim1.toLong, dim2.toLong),
      Array[Long](HDF5Constants.H5S_UNLIMITED, HDF5Constants.H5S_UNLIMITED))
    assertException(space_id >= 0, "No valid space to write data.")

    H5Reg(space_id, "S")

    val dataset_id = writeArray(group_id, space_id, name, data, Array[Long](dim1, dim2))

    H5Reg(dataset_id, "D")

    // write optional attributes to space
    for (attr <- withAttr)
      writeAttrText(dataset_id, attr._1, attr._2)

    // close space
    H5.H5Sclose(H5Reg(space_id))
    H5.H5Dclose(H5Reg(dataset_id))
  }

  // common array-writing code
  private def writeArray[T: CLM](group_id: Int, space_id: Int, name: String, data: Array[T],
                                 dataDims: Array[Long]): Int = {
    val clm = implicitly[CLM[T]]

    // extract the (possibly transformed) data type of the array
    val (datatype_id, databuf) = clm.erasure match {
      // handle the case where it's a string, convert to bytes
      case c if c == sc => {
        // the (necessarily uniform) record length should be the length of max string encoding
        val strid = H5.H5Tcopy(HDF5Constants.H5T_C_S1)
        val maxsz = data.foldLeft(1) { (a, b) => a.max(b.asInstanceOf[String].getBytes("UTF-8").length) }
        H5.H5Tset_size(strid, maxsz)

        H5Reg(strid, "T")

        // copy string as utf8 encoded bytes into buffer
        val byteBuff = new Array[Byte](data.size * maxsz)
        var i = 0
        data.foreach { s =>
          val asBytes = s.asInstanceOf[String].getBytes("UTF-8") // "ISO-8859-1"
          System.arraycopy(asBytes, 0, byteBuff, i, asBytes.size)
          i += maxsz
        }

        (strid, byteBuff)
      }
      // in the case where it's a datetime, save as nanoseconds since unix epoch (Long)
      case c if c == tc => {
        val valArr = new Array[Long](data.size)

        var i = 0
        while (i < data.size) {
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

    var cparms = -1

    val dataset_id = try {
      // try to open data set and change its extent to the right size
      val dsetid = H5.H5Dopen(group_id, name, HDF5Constants.H5P_DEFAULT)
      H5.H5Dset_extent(dsetid, dataDims)
      dsetid
    }
    catch {
      // or it doesn't exist
      case e: HDF5LibraryException => {
        // create dataset & modify dataset creation properties, i.e. enable chunking
        cparms = H5.H5Pcreate(HDF5Constants.H5P_DATASET_CREATE)

        val chunks = Array.fill[Long](dataDims.size)(1024)
        val status = H5.H5Pset_chunk(cparms, dataDims.size, chunks)

        assertException(status >= 0, "Failure during dataset properties")

        H5.H5Dcreate(group_id, name, datatype_id, space_id,
          HDF5Constants.H5P_DEFAULT, cparms, HDF5Constants.H5P_DEFAULT)
      }
    }

    assertException(dataset_id >= 0, "Failure during dataset creation")

    // write buffer data
    H5.H5Dwrite(dataset_id, datatype_id, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
      HDF5Constants.H5P_DEFAULT, databuf)

    // close datatype resource if we created one
    if (H5.H5Tget_class(datatype_id) == HDF5Constants.H5T_STRING)
      H5.H5Tclose(H5Reg(datatype_id))

    if (cparms != -1)
      H5.H5Pclose(H5Reg(cparms))

    // return dataset id
    dataset_id
  }

  // reads a dataset from file into an array in memory
  private def readArray[X: CLM](grpid: Int, dsname: String): Array[X] = {
    // get dataset id
    val dsetid = H5.H5Dopen(grpid, dsname, HDF5Constants.H5P_DEFAULT)
    assertException(dsetid >= 0, "Dataset: " + dsname + " does not exist for this group")

    H5Reg(dsetid, "D")

    val result = readArray[X](grpid, dsetid)

    H5.H5Dclose(H5Reg(dsetid))
    result
  }

  private case class Array2D[T](rows: Int, cols: Int, data: Array[T])

  // read a two-dimensional array (dataset) and return (rowcount, colcount, values)
  private def read2DArray[T: CLM](
    group_id: Int, dsname: String): Array2D[T] = {
    // get dataset id
    val dsetid = H5.H5Dopen(group_id, dsname, HDF5Constants.H5P_DEFAULT)
    assertException(dsetid >= 0, "Dataset: " + dsname + " does not exist for this group")

    H5Reg(dsetid, "D")

    // get space
    val dspaceid = H5.H5Dget_space(dsetid)
    assertException(dspaceid != 0, "Dataspace does not exist for dataset = " + dsetid)

    H5Reg(dspaceid, "S")

    // assert it's two-dimensional
    val dims = H5.H5Sget_simple_extent_ndims(dspaceid)
    assertException(dims == 2, "Data is not 2 dimensional")

    // get length of arrays
    val sz = Array[Long](0, 0)
    H5.H5Sget_simple_extent_dims(dspaceid, sz, null)

    // get data type to switch on
    val datatype = H5.H5Dget_type(dsetid)
    assertException(datatype >= 0, "Not a valid datatype")

    H5Reg(datatype, "T")

    val result = Array.ofDim[T](sz(0).toInt * sz(1).toInt)

    val cm = implicitly[CLM[T]]

    val read_type = cm.erasure match {
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
    }

    H5.H5Dread(dsetid, read_type,
      HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
      HDF5Constants.H5P_DEFAULT, result)

    H5.H5Tclose(H5Reg(datatype))
    H5.H5Sclose(H5Reg(dspaceid))
    H5.H5Dclose(H5Reg(dsetid))

    // it's transposed, ie col-major order
    Array2D(sz(1).toInt, sz(0).toInt, result)
  }

  private def readArray[X: CLM](grpid: Int, dsetid: Int): Array[X] = {
    // get space
    val dspaceid = H5.H5Dget_space(dsetid)
    assertException(dspaceid != 0, "Dataspace does not exist for dataset = " + dsetid)

    H5Reg(dspaceid, "S")

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

    H5Reg(datatype, "T")

    val cm = implicitly[CLM[X]]

    val read_type = cm.erasure match {
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

        H5Reg(memtype_id, "T")

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
          result(i) = new String(tmpBuf, "UTF-8").asInstanceOf[X]
          j += sdim
          i += 1
        }

        // close resources, short-circuit and return
        H5.H5Tclose(H5Reg(memtype_id))
        H5.H5Tclose(H5Reg(datatype))
        H5.H5Sclose(H5Reg(dspaceid))

        return result
      }
      // datetimes
      case c if c == tc => {

        assertException(H5.H5Tget_class(datatype) == HDF5Constants.H5T_INTEGER, "Not a valid Long")

        val newarr = Array.ofDim[Long](arrlen)

        H5.H5Dread(dsetid, HDF5Constants.H5T_NATIVE_INT64,
          HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
          HDF5Constants.H5P_DEFAULT, newarr)

        val result = Array.ofDim[X](arrlen)
        for (i <- Range(0, arrlen)) {
          result(i) = new DateTime(newarr(i) / 1000000L).asInstanceOf[X]
        }

        // close resources, short-circuit and return
        H5.H5Tclose(H5Reg(datatype))
        H5.H5Sclose(H5Reg(dspaceid))

        return result
      }
      case _ => throw new IllegalArgumentException("Unrecognized array type")
    }

    // construct new array
    val result = Array.ofDim[X](arrlen)

    H5.H5Dread(dsetid, read_type,
      HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
      HDF5Constants.H5P_DEFAULT, result)

    H5.H5Tclose(H5Reg(datatype))
    H5.H5Sclose(H5Reg(dspaceid))

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

  private def getPandasIndexAttribs[X: CLM](index: Index[X]) = {
    assertException(index.length > 0, "Index has size of 0.")
    val attribs = getPandasSeriesAttribs ++ List(("name", "N."))
    val clm = implicitly[CLM[X]]
    attribs ++ {
      clm.erasure match {
        case c if c == ic => List(("kind", "integer"))
        case c if c == lc => List(("kind", "integer"))
        case c if c == dc => List(("kind", "float"))
        case c if c == sc => List(("kind", "string"))
        // todo: add frequency attribute if one exists
        case c if c == tc => List(("index_class", "datetime"), ("kind", "datetime64"), ("freq", "N."))
        case _           => throw new IllegalArgumentException("Index type not recognized")
      }
    }
  }

  private def writePandasSeries[X: CLM, T: CLM](
    file: String, name: String, index: Index[X], values: Array[T]): Int = {

    val fileid = if (new File(file).exists) {
      openFile(file)
    }
    else {
      val id = createFile(file)
      val grpid = openGroup(id, "/")
      writePytablesHeader(grpid)
      closeGroup(grpid)
      id
    }

    val result: Int = writePandasSeries[X, T](fileid, name, index, values)

    closeFile(fileid)

    result
  }

  private def writePandasSeries[X: CLM, T: CLM](
    fileid: Int, name: String, index: Index[X], values: Array[T]): Int = {

    val grpid = try {
      createGroup(fileid, "/" + name)
    }
    catch {
      case e: HDF5LibraryException => openGroup(fileid, "/" + name)
    }

    writeSeriesPandasHeader(grpid)

    write1DArray(grpid, "index", index.toVec, getPandasIndexAttribs(index))
    write1DArray(grpid, "values", values, getPandasSeriesAttribs)

    closeGroup(grpid)
    H5.H5Fflush(fileid, HDF5Constants.H5F_SCOPE_GLOBAL)
  }

  private def readPandasSeries[X: CLM: ORD, T: CLM](
    file: String, name: String): Series[X, T] = {

    val fileid = openFile(file)
    assertException(fileid >= 0, "File ID : " + fileid + " does not belong to a valid file")

    val result = readPandasSeries[X, T](fileid, name)

    closeFile(fileid)

    result
  }

  private def readPandasSeries[X: ORD: CLM, T: CLM](
    fileid: Int, name: String): Series[X, T] = {
    val grpid = openGroup(fileid, name)
    assertException(grpid >= 0, "Group : " + name + " is not a valid group")

    val attr = readAttrText(grpid, "pandas_type")
    assertException(attr == "series", "Attribute is not a series")

    val vals = readArray[T](grpid, "values")

    val idxid = H5.H5Dopen(grpid, "index", HDF5Constants.H5P_DEFAULT)
    assertException(idxid >= 0, "index group is not valid")

    val ix = implicitly[CLM[X]]

    // type-check the index
    readAttrText(idxid, "kind") match {
      case "integer"    => assertException(ix.erasure == classOf[Long] || ix.erasure == classOf[Int], "Index is not a long/int")
      case "string"     => assertException(ix.erasure == classOf[String], "Index is not a string")
      case "float"      => assertException(ix.erasure == classOf[Double], "Index is not a float")
      case "datetime64" => assertException(ix.erasure == classOf[DateTime], "Index is not a datetime64")
      case _@ t         => throw new IllegalArgumentException("Bad index type found: %s".format(t))
    }

    val index = readArray[X](grpid, idxid)

    val result = Series(Vec(vals), Index(index))

    closeGroup(grpid)
    result
  }

  private def writePandasFrame[R: ORD: CLM, C: ORD: CLM, T: CLM](
    file: String, name: String, rx: Index[R], cx: Index[C], values: Mat[T]): Int = {

    val fileid = if (new File(file).exists) {
      openFile(file)
    }
    else {
      val id = createFile(file)
      val grpid = openGroup(id, "/")
      writePytablesHeader(grpid)
      closeGroup(grpid)
      id
    }

    assertException(fileid >= 0, "File ID : " + fileid + " does not belong to a valid file")

    val result: Int = writePandasFrame[R, C, T](fileid, name, rx, cx, values)

    closeFile(fileid)

    result
  }

  private def writePandasFrame[R: CLM: ORD, C: CLM: ORD, T: CLM](
    fileid: Int, name: String, rx: Index[R], cx: Index[C], values: Mat[T]): Int = {

    val grpid = try {
      createGroup(fileid, "/" + name)
    }
    catch {
      case e: HDF5LibraryException => openGroup(fileid, "/" + name)
    }

    writeFramePandasHeader(grpid)

    // axis 0 is column names
    write1DArray(grpid, "axis0", cx.toVec, getPandasIndexAttribs(cx))

    // axis 1 is row names
    write1DArray(grpid, "axis1", rx.toVec, getPandasIndexAttribs(rx))

    // the block manager in pandas has up to four blocks: Int64, Float64, Char (ie Int8), PyObject
    // since we only store doubles in QuantS for now, we assume only one block that comprises all
    // the columns.
    write1DArray(grpid, "block0_items", cx.toVec, getPandasIndexAttribs(cx))

    // the data itself is stored in a transposed format (col-major order)
    write2DArray(grpid, "block0_values", values.numRows, values.numCols,
      values.contents, getPandasSeriesAttribs)

    closeGroup(grpid)
    H5.H5Fflush(fileid, HDF5Constants.H5F_SCOPE_GLOBAL)
  }

  private def readPandasFrame[RX: CLM: ORD, CX: CLM: ORD, T: CLM](
    file: String, name: String): Frame[RX, CX, T] = {

    val fileid = openFile(file)
    assertException(fileid >= 0, "File ID : " + fileid + " does not belong to a valid file")

    val result = readPandasFrame[RX, CX, T](fileid, name)

    closeFile(fileid)

    result
  }

  private def readPandasFrame[RX: CLM: ORD, CX: CLM: ORD, T: CLM](
    fileid: Int, name: String): Frame[RX, CX, T] = {
    val grpid = openGroup(fileid, name)
    assertException(grpid >= 0, "Group : " + name + " is not a valid group")

    val attr = readAttrText(grpid, "pandas_type")
    assertException(attr == "frame", "Attribute is not a Frame")

    // the block manager in pandas has up to four blocks: Int64, Float64, Char (ie Int8), PyObject
    // since we only store doubles in QuantS for now, we assume only one block that comprises all
    // the columns.
    val arr2d = read2DArray[T](grpid, "block0_values")

    // data is stored transposed (ie, col-major order, so un-transpose it)
    val mx = Mat(arr2d.cols, arr2d.rows, arr2d.data)

    val rx = implicitly[CLM[RX]]
    val cx = implicitly[CLM[CX]]

    val rowidx = H5.H5Dopen(grpid, "axis1", HDF5Constants.H5P_DEFAULT)
    assertException(rowidx >= 0, "row index group is not valid")

    H5Reg(rowidx, "D")

    // type-check the indices
    readAttrText(rowidx, "kind") match {
      case "integer"    => assertException(rx.erasure == classOf[Long] || rx.erasure == classOf[Int], "Index is not a long/int")
      case "string"     => assertException(rx.erasure == classOf[String], "Index is not a string")
      case "float"      => assertException(rx.erasure == classOf[Double], "Index is not a float")
      case "datetime64" => assertException(rx.erasure == classOf[DateTime], "Index is not a joda datetime64")
      case _@ t         => throw new IllegalArgumentException("Bad index type found: %s".format(t))
    }

    val colidx = H5.H5Dopen(grpid, "axis0", HDF5Constants.H5P_DEFAULT)
    assertException(colidx >= 0, "column index group is not valid")

    H5Reg(colidx, "D")

    // type-check the indices
    readAttrText(colidx, "kind") match {
      case "integer"    => assertException(cx.erasure == classOf[Long] || cx.erasure == classOf[Int], "Index is not a long/int")
      case "string"     => assertException(cx.erasure == classOf[String], "Index is not a string")
      case "float"      => assertException(cx.erasure == classOf[Float], "Index is not a float")
      case "datetime64" => assertException(cx.erasure == classOf[DateTime], "Index is not a joda datetime64")
      case _@ t         => throw new IllegalArgumentException("Bad index type found: %s".format(t))
    }

    val ix0 = Index[RX](Vec(readArray[RX](grpid, rowidx)))
    val ix1 = Index[CX](Vec(readArray[CX](grpid, colidx)))

    H5.H5Dclose(H5Reg(rowidx))
    H5.H5Dclose(H5Reg(colidx))

    val result = Frame[RX, CX, T](mx, ix0, ix1)

    closeGroup(grpid)

    result
  }

  def assertException(condition: Boolean, errorMessage: String) {
    if (!condition) {
      throw new H5StoreException(errorMessage)
    }
  }
}

// Exception, releases all acquired HDF5 resources on instantiation
class H5StoreException(msg: String) extends RuntimeException(msg) {
  H5Reg.release()
}

// Registry for H5 resources. When acquiring H5 resource, register via H5Reg(resource, "type").
// If exception is thrown, release() will free all the acquired H5 resources.
object H5Reg {
  private var registry = Map[Int, String]()

  // for accessing resources we have
  def resourceMap = registry

  // registers resource
  def apply(v: Int, t: String): Int = {
    // println("Registering " + (v -> t))
    registry ++= Map(v -> t)
    v
  }

  // un-registers resource
  def apply(v: Int): Int = {
    // println("Unregistering " + v)
    registry --= List(v)
    v
  }

  // releases resources
  def release() {
    // println("Releasing resources")
    registry.foreach {
      x =>
        try {
          x match {
            case (v, "T") => H5.H5Tclose(v)
            case (v, "A") => H5.H5Aclose(v)
            case (v, "D") => H5.H5Dclose(v)
            case (v, "S") => H5.H5Sclose(v)
            case (v, "P") => H5.H5Pclose(v)
            case (v, "G") => H5.H5Gclose(v)
            case (v, "F") => H5.H5Fclose(v)
            case _        => throw new RuntimeException("Could not recognize resource tag")
          }
        }
        finally {}
    }

    registry = Map[Int, String]().empty
  }
}