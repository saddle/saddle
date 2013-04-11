package org.saddle.mat

import org.saddle._
import org.saddle.Mat
import org.saddle.scalar.ScalarTagString
import scala.{ specialized => spec }

class MatString private (data: Array[Byte], offsets: Mat[Int], lengths: Mat[Int]) extends Mat[String] {
  require(offsets.numRows == lengths.numRows && offsets.numCols == lengths.numCols,
          "Offsets matrix and lengths matrix do not match dimensions")

  def scalarTag = ScalarTagString

  /**
   * Returns number of rows in the matrix shape
   *
   */
  def numRows = offsets.numRows

  /**
   * Returns number of columns in the matrix shape
   *
   */
  def numCols = offsets.numCols

  /**
   * Maps a function over each element in the matrix
   */
  def map[@spec(Boolean, Int, Long, Double) B: ST](f: (String) => B) = null

  /**
   * Changes the shape of matrix without changing the underlying data
   */
  def reshape(r: Int, c: Int) = null

  /**
   * Transpose of original matrix
   */
  def transpose = null

  /**
   * Create Mat comprised of same values in specified rows
   */
  def takeRows(locs: Array[Int]) = null

  /**
   * Create Mat comprised of same values without the specified rows
   *
   * @param locs Row locations to exclude
   */
  def withoutRows(locs: Array[Int]) = null

  /**
   * Concatenate all rows into a single row-wise Vec instance
   */
  def toVec = null

  // access like vector in row-major order
  private[saddle] def apply(i: Int) = null

  // implement access like matrix(i, j)
  private[saddle] def apply(r: Int, c: Int) = {
    val len = lengths(r, c)
    val off = offsets(r, c)
    if (off < 0)
      scalarTag.missing
    else {
      val bytes = Array.ofDim[Byte](len)
      System.arraycopy(data, off, bytes, 0, len)
      new String(bytes, UTF8)
    }
  }

  // use with caution, may not return copy
  private[saddle] def toArray = { cols.foldLeft(Vec.empty[String]) { case (x, y) => x concat y } }.toArray

  // use with caution, may not return copy
  private[saddle] def toDoubleArray(implicit ev: NUM[String]) = null

  // use with caution, for destructive matrix ops
  private[saddle] def update(i: Int, v: String) {}

  /**
   * Copy of original matrix
   *
   */
  protected def copy = null
}

object MatString {
  def apply(cols: Seq[Vec[String]]): Mat[String] = {
    // flatten all vecs into one buffer
    // make mats of offset/lengths
    null
  }
}