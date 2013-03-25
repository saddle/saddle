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

import org.saddle._
import java.io.File
import org.joda.time.DateTime

import scala.util.control.Exception.allCatch

import org.specs2.mutable.Specification
import org.specs2.execute.{AsResult, Skipped}
import org.specs2.specification.Fragment

/**
 * Tests the H5Store implementation
 */
class H5StoreSpec extends Specification {
  val tmpDir = new File(System.getProperty("java.io.tmpdir"))
  val random = new java.util.Random

  def tmpFilePath(ext: String): String = {
    val randomName = "tmp" + java.lang.Integer.toHexString(random.nextInt) + ext
    val fn = System.getProperty("java.io.tmpdir") + "/" + randomName
    fn
  }

  def hdfTest[T: AsResult](test: String)(logic: => T): Fragment = {
    allCatch either {
      java.lang.Runtime.getRuntime.loadLibrary("jhdf5")
    }
    match {
      case Left(exception) => Skipped("Could not import HDF5")
      case Right(result)   => test ! logic
    }
  }

  "H5Store" should {
    hdfTest("create, open, and close a file") {
      val tmp = tmpFilePath(".h5")

      var fid = H5Store.createFile(tmp)
      fid must be greaterThan 0
      H5Store.closeFile(fid)

      fid = H5Store.openFile(tmp)
      fid must be greaterThan 0
      H5Store.closeFile(fid)

      new File(tmp).delete()
    }

    hdfTest("read, write series") {
      val tmp = tmpFilePath(".h5")

      val d1 = new DateTime(2005, 1, 1, 0, 0, 0, 0)
      val d2 = new DateTime(2005, 1, 2, 0, 0, 0, 0)
      val d3 = new DateTime(2005, 1, 3, 0, 0, 0, 0)

      val s1 = Series(vec.rand(3), Index(d1, d2, d3))
      val s2 = Series(vec.rand(3), Index("a", "b", "c"))
      val s3 = Series(vec.rand(3), Index(1, 2, 3))
      val s4 = Series(vec.rand(3), Index(1L, 2, 3))
      val s5 = Series(vec.rand(3), Index(1d, 2, 3))

      H5Store.writeSeries(tmp, "s1", s1)
      H5Store.writeSeries(tmp, "s2", s2)
      H5Store.writeSeries(tmp, "s3", s3)
      H5Store.writeSeries(tmp, "s4", s4)
      H5Store.writeSeries(tmp, "s5", s5)

      H5Store.readSeries[DateTime, Double](tmp, "s1") must_== s1
      H5Store.readSeries[String, Double](tmp, "s2") must_== s2
      H5Store.readSeries[Int, Double](tmp, "s3") must_== s3
      H5Store.readSeries[Long, Double](tmp, "s4") must_== s4
      H5Store.readSeries[Double, Double](tmp, "s5") must_== s5

      new File(tmp).delete()

      // same tests, with holding file open

      var fid = H5Store.createFile(tmp)

      H5Store.writeSeries(fid, "s1", s1)
      H5Store.writeSeries(fid, "s2", s2)
      H5Store.writeSeries(fid, "s3", s3)
      H5Store.writeSeries(fid, "s4", s4)
      H5Store.writeSeries(fid, "s5", s5)

      H5Store.closeFile(fid)

      fid = H5Store.openFile(tmp)

      H5Store.readSeries[DateTime, Double](fid, "s1") must_== s1
      H5Store.readSeries[String, Double](fid, "s2") must_== s2
      H5Store.readSeries[Int, Double](fid, "s3") must_== s3
      H5Store.readSeries[Long, Double](fid, "s4") must_== s4
      H5Store.readSeries[Double, Double](fid, "s5") must_== s5

      H5Store.closeFile(fid)

      new File(tmp).delete()
    }

    hdfTest("read, write Frame") {
      val tmp = tmpFilePath(".h5")

      val d1 = new DateTime(2005, 1, 1, 0, 0, 0, 0)
      val d2 = new DateTime(2005, 1, 2, 0, 0, 0, 0)
      val d3 = new DateTime(2005, 1, 3, 0, 0, 0, 0)

      val df1 = Frame(mat.rand(3, 3), Index(d1, d2, d3), Index(1, 2, 3))
      val df2 = Frame(mat.rand(3, 3), Index("a", "b", "c"), Index(1, 2, 3))
      val df3 = Frame(mat.rand(3, 3), Index(1, 2, 3), Index(1, 2, 3))
      val df4 = Frame(mat.rand(3, 3), Index(1L, 2, 3), Index(1, 2, 3))
      val df5 = Frame(mat.rand(3, 3), Index(1d, 2, 3), Index(1, 2, 3))

      H5Store.writeFrame(tmp, "df1", df1)
      H5Store.writeFrame(tmp, "df2", df2)
      H5Store.writeFrame(tmp, "df3", df3)
      H5Store.writeFrame(tmp, "df4", df4)
      H5Store.writeFrame(tmp, "df5", df5)

      H5Store.readFrame[DateTime, Int, Double](tmp, "df1") must_== df1
      H5Store.readFrame[String, Int, Double](tmp, "df2") must_== df2
      H5Store.readFrame[Int, Int, Double](tmp, "df3") must_== df3
      H5Store.readFrame[Long, Int, Double](tmp, "df4") must_== df4
      H5Store.readFrame[Double, Int, Double](tmp, "df5") must_== df5

      new File(tmp).delete()

      // same tests, with holding file open

      var fid = H5Store.createFile(tmp)

      H5Store.writeFrame(fid, "df1", df1)
      H5Store.writeFrame(fid, "df2", df2)
      H5Store.writeFrame(fid, "df3", df3)
      H5Store.writeFrame(fid, "df4", df4)
      H5Store.writeFrame(fid, "df5", df5)

      H5Store.closeFile(fid)

      fid = H5Store.openFile(tmp)

      H5Store.readFrame[DateTime, Int, Double](fid, "df1") must_== df1
      H5Store.readFrame[String, Int, Double](fid, "df2") must_== df2
      H5Store.readFrame[Int, Int, Double](fid, "df3") must_== df3
      H5Store.readFrame[Long, Int, Double](fid, "df4") must_== df4
      H5Store.readFrame[Double, Int, Double](fid, "df5") must_== df5

      H5Store.closeFile(fid)

      new File(tmp).delete()
    }

    hdfTest("update/overwrite series, Frame") {
      val tmp = tmpFilePath(".h5")

      val d1 = new DateTime(2005, 1, 1, 0, 0, 0, 0)
      val d2 = new DateTime(2005, 1, 2, 0, 0, 0, 0)
      val d3 = new DateTime(2005, 1, 3, 0, 0, 0, 0)

      val df1 = Frame(mat.rand(3, 3), Index(d1, d2, d3), Index(1, 2, 3))
      val df2 = Frame(mat.rand(3, 4), Index(d1, d2, d3), Index(1, 2, 3, 4))

      H5Store.writeFrame(tmp, "df1", df1)
      H5Store.writeFrame(tmp, "df1", df2)

      H5Store.readFrame[DateTime, Int, Double](tmp, "df1") must_== df2

      new File(tmp).delete()
    }
  }
}