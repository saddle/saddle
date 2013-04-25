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

import org.joda.time.DateTime

import java.io.File
import java.util.concurrent._
import java.nio.file.{ Files, Paths }

import org.specs2.mutable.Specification
import org.specs2.execute.{AsResult, Skipped}
import org.specs2.specification.Fragment

import scala.util.control.Exception.allCatch

/**
 * Tests the H5Store implementation
 */
class H5StoreSpec extends Specification {

  // We run only one HDF5 test at a time. It would be fine to run multiple
  // tests in parallel, but then the invariant won't hold that at the end
  // of a test we have zero outstanding HDF5 handles (ie open resources).

  args(sequential=true)

  val tmpDir = new File(System.getProperty("java.io.tmpdir"))
  val random = new java.util.Random


  val d1 = new DateTime(2005, 1, 1, 0, 0, 0, 0)
  val d2 = new DateTime(2005, 1, 2, 0, 0, 0, 0)
  val d3 = new DateTime(2005, 1, 3, 0, 0, 0, 0)
  val d4 = new DateTime(2005, 1, 4, 0, 0, 0, 0)

  private def tmpFilePath(ext: String): String = {
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
      case Right(result)   => {
        test in {
          logic
          H5Store.openResourceCount must_== 0  // check for any resource leaks
        }
      }
    }
  }

  "H5Store" should {
    hdfTest("create, open, and close a file") {
      val tmp = tmpFilePath(".h5")

      val fid1 = H5Store.createFile(tmp)

      fid1 must be greaterThan 0

      H5Store.closeFile(fid1)

      val fid2 = H5Store.openFile(tmp)
      fid2 must be greaterThan 0
      H5Store.closeFile(fid2)

      Files.deleteIfExists(Paths.get(tmp))
    }

    hdfTest("read, write series") {
      val tmp = tmpFilePath(".h5")

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

      Files.deleteIfExists(Paths.get(tmp))

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

      // try slicing
      H5Store.readSeriesSlice[DateTime, Double](fid, "s1", d2, d3, true) must_== s1.sliceBy(d2, d3)

      H5Store.closeFile(fid)

      Files.deleteIfExists(Paths.get(tmp))
    }

    hdfTest("write/read empty Series") {
      val tmp = tmpFilePath(".h5")

      H5Store.writeSeries(tmp, "s1", Series.empty[DateTime, Double])

      val res = H5Store.readSeries[DateTime, Double](tmp, "s1")

      res must_== Series.empty[DateTime, Double]

      Files.deleteIfExists(Paths.get(tmp))
    }

    hdfTest("read, write Frame") {
      val tmp = tmpFilePath(".h5")

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

      Files.deleteIfExists(Paths.get(tmp))

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

      // try slicing
      H5Store.readFrameSlice[DateTime, Int, Double](fid, "df1", d2, d3, 2, 3, true, true) must_== df1.colSliceBy(2, 3).rowSliceBy(d2, d3)

      H5Store.closeFile(fid)

      Files.deleteIfExists(Paths.get(tmp))
    }

    hdfTest("update/overwrite Frame fails safely") {
      val tmp = tmpFilePath(".h5")

      val d1 = new DateTime(2005, 1, 1, 0, 0, 0, 0)
      val d2 = new DateTime(2005, 1, 2, 0, 0, 0, 0)
      val d3 = new DateTime(2005, 1, 3, 0, 0, 0, 0)
      val d4 = new DateTime(2005, 1, 4, 0, 0, 0, 0)

      val df1 = Frame(mat.rand(3, 3), Index(d1, d2, d3), Index(1, 2, 3))
      val df2 = Frame(mat.rand(4, 4), Index(d1, d2, d3, d4), Index(1, 2, 3, 4))

      H5Store.writeFrame(tmp, "df1", df1)
      H5Store.readFrame[DateTime, Int, Double](tmp, "df1") must_== df1

      H5Store.writeFrame(tmp, "df1", df2) must throwAn[H5Store.H5StoreException]
      H5Store.readFrame[DateTime, Int, Double](tmp, "df1") must_== df1

      Files.deleteIfExists(Paths.get(tmp))
    }

    hdfTest("update/overwrite Series fails safely") {
      val tmp = tmpFilePath(".h5")

      val s1 = Series(vec.rand(3), Index(d1, d2, d3))
      val s2 = Series(vec.rand(4), Index(d1, d2, d3, d4))

      H5Store.writeSeries(tmp, "s1", s1)
      H5Store.readSeries[DateTime, Double](tmp, "s1") must_== s1

      H5Store.writeSeries(tmp, "s1", s2) must throwAn[H5Store.H5StoreException]
      H5Store.readSeries[DateTime, Double](tmp, "s1") must_== s1

      Files.deleteIfExists(Paths.get(tmp))
    }

    hdfTest("write/read empty Frame") {
      val tmp = tmpFilePath(".h5")

      H5Store.writeFrame(tmp, "f1", Frame.empty[DateTime, Int, Double])

      val res = H5Store.readFrame[DateTime, Int, Double](tmp, "f1")

      res must_== Frame.empty[DateTime, Int, Double]

      Files.deleteIfExists(Paths.get(tmp))
    }

    hdfTest("H5Store exceptions are handled properly") {
      val tmp = tmpFilePath(".h5")

      H5Store.readSeries[DateTime, Double](tmp, "s1") must throwAn[H5Store.H5StoreException]
      H5Store.readFrame[DateTime, Int, Double](tmp, "f1") must throwAn[H5Store.H5StoreException]

      // try reading wrong data type
      val s1 = Series(vec.rand(3), Index(d1, d2, d3))
      H5Store.writeSeries(tmp, "s1", s1)
      H5Store.readSeries[Int, Double](tmp, "s1") must throwAn[H5Store.H5StoreException]
      H5Store.readSeries[DateTime, Int](tmp, "s1") must throwAn[H5Store.H5StoreException]

      Files.deleteIfExists(Paths.get(tmp))
    }

    hdfTest("H5Store is thread-safe") {
      import scala.collection.JavaConversions._

      val tmp = tmpFilePath(".h5")

      val d1 = new DateTime(2005, 1, 1, 0, 0, 0, 0)
      val d2 = new DateTime(2005, 1, 2, 0, 0, 0, 0)
      val d3 = new DateTime(2005, 1, 3, 0, 0, 0, 0)
      val d4 = new DateTime(2005, 1, 4, 0, 0, 0, 0)

      val pool = new ForkJoinPool()

      val df1 = Frame(mat.rand(3, 3), Index(d1, d2, d3), Index(1, 2, 3))
      val df2 = Frame(mat.rand(4, 4), Index(d1, d2, d3, d4), Index(1, 2, 3, 4))

      // simultaneous writes

      val taskListW = for (i <- 1 to 100) yield new Callable[Unit] {
        def call() {
          H5Store.writeFrame(tmp, "f%s".format(i), df1)
          H5Store.writeFrame(tmp, "f%s".format(100 + i), df2)
        }
      }

      pool.invokeAll(taskListW)

      // simultaneous reads

      val taskListR = for (i <- 1 to 100) yield new Callable[Unit] {
        def call() {
          H5Store.readFrame[DateTime, Int, Double](tmp, "f%s".format(100 + i)) must_== df2
          H5Store.readFrame[DateTime, Int, Double](tmp, "f%s".format(i)) must_== df1
        }
      }

      pool.invokeAll(taskListR.toSeq)

      // interleaved reads & writes

      val taskListRW = for (i <- 1 to 100) yield new Callable[Unit] {
        def call() {
          if (i % 2 == 0)
            H5Store.writeFrame(tmp, "f%s".format(100 + i), df1)
          else
            H5Store.readFrame[DateTime, Int, Double](tmp, "f%s".format(i)) must_== df1
        }
      }

      pool.invokeAll(taskListRW.toSeq)

      pool.shutdown()

      Files.deleteIfExists(Paths.get(tmp))
   }
  }
}