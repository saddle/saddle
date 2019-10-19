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
package org.saddle.mat

import scala.{specialized => spec}
import org.saddle.{Mat, Vec, ST, NUM, Frame, util}
import org.saddle.scalar.{ScalarTag, Scalar}
import org.saddle.index.Slice
import org.saddle.index.IndexIntRange
import java.io.OutputStream

/**
  * A Mat instance containing elements of type Long
  */
class MatDefault[@spec(Boolean, Int, Long, Double) T](
    r: Int,
    c: Int,
    values: Array[T],
    val scalarTag: ScalarTag[T]
) extends Mat[T] {
  implicit private[this] def st = scalarTag

  def numRows = r

  def numCols = c

  /** Returns the backing array of this Mat
    * Mutations to this array are visible to this Mat
    *
    * Elements are laid out in row-major order
    */
  def toArray = values

  /**
    * Return unboxed value of matrix at an offset from zero in row-major order
    *
    * @param i index
    */
  def raw(i: Int): T = values(i)

  /**
    * Return unboxed value of matrix at row/column
    *
    * @param r row index
    * @param c col index
    */
  def raw(r: Int, c: Int): T = values(r * numCols + c)

  /**
    * Concatenate all rows into a single row-wise Vec instance
    *
    * Underlying array is shared between the two instances
    */
  def toVec = scalarTag.makeVec(toArray)

  /**
    * Returns (a copy of) the contents of matrix as a single array in
    * row-major order
    *
    */
  def contents: Array[T] = values.clone()

  /**
    * Makes a copy of this Mat
    *
    */
  def copy: Mat[T] = new MatDefault(r, c, values.clone, scalarTag)

  def map[@spec(Boolean, Int, Long, Double) B: ST](f: (T) => B): Mat[B] =
    MatImpl.map(this)(f)

  // Cache the transpose: it's much faster to transpose and slice a continuous
  // bound than to take large strides, especially on large matrices where it
  // seems to eject cache lines on each stride (something like 10x slowdown)
  lazy val cachedT = {
    val arrT = values.clone()

    if (this.isSquare)
      MatMath.squareTranspose(numCols, arrT)
    else
      MatMath.blockTranspose(numRows, numCols, this.toArray, arrT)

    new MatDefault(numCols, numRows, arrT, scalarTag)
  }

  def transpose = cachedT

  def T = transpose

  def takeRows(locs: Array[Int]): Mat[T] = MatImpl.takeRows(this, locs)

  def takeRows(locs: Vec[Int]): Mat[T] = MatImpl.takeRows(this, locs.toArray)

  /**
    * Create Mat comprised of same values without the specified rows
    *
    * @param locs Row locations to exclude
    */
  def withoutRows(locs: Array[Int]): Mat[T] = MatImpl.withoutRows(this, locs)

  /**
    * Changes the shape of matrix without changing the underlying data
    *
    * Backing array will be shared between the two instances!
    */
  def reshape(r: Int, c: Int): Mat[T] = new MatDefault(r, c, values, scalarTag)

  private[saddle] def toDoubleArray(implicit ev: NUM[T]): Array[Double] = {
    val arr = Array.ofDim[Double](values.length)
    var i = 0
    val n = arr.length
    while (i < n) {
      arr(i) = scalarTag.toDouble(values(i))
      i += 1
    }
    arr
  }

  /** Row-by-row equality check of all values. */
  override def equals(o: Any): Boolean = o match {
    case rv: Mat[_] =>
      (this eq rv) || this.numRows == rv.numRows && this.numCols == rv.numCols && {
        var i = 0
        var eq = true
        while (eq && i < length) {
          eq &&= (raw(i) == rv.raw(i) || this.scalarTag
            .isMissing(raw(i)) && rv.scalarTag.isMissing(rv.raw(i)))
          i += 1
        }
        eq
      }
    case _ => super.equals(o)
  }

  /**
    * Returns total number of entries in the matrix
    *
    */
  def length: Int = numRows * numCols

  /**
    * Returns true if rows == cols
    *
    */
  def isSquare: Boolean = numCols == numRows

  /**
    * Returns true if the matrix is empty
    *
    */
  def isEmpty: Boolean = length == 0

  /**
    * Return scalar value of matrix at offset from zero in row-major order
    *
    * @param i index
    */
  def at(i: Int): Scalar[T] =
    Scalar(raw(i))(scalarTag)

  /**
    * Return scalar value of Mat at at row/column
    * @param r row index
    * @param c col index
    */
  def at(r: Int, c: Int): Scalar[T] = {
    Scalar(raw(r, c))(scalarTag)
  }

  /**
    * Access a slice of the Mat by integer offsets
    * @param r Array of row offsets
    * @param c Array of col offsets
    */
  def at(r: Array[Int], c: Array[Int]): Mat[T] = {
    row(r).col(c)
  }

  /**
    * Access a slice of the Mat by integer offsets
    * @param r Array of row offsets
    * @param c Integer col offset
    */
  def at(r: Array[Int], c: Int): Vec[T] = {
    row(r).col(c)
  }

  /**
    * Access a slice of the Mat by integer offsets
    * @param r Integer row offset
    * @param c Array of col offsets
    */
  def at(r: Int, c: Array[Int]): Vec[T] = {
    col(c).row(r)
  }

  /**
    * Access a slice of the Mat by Slice parameters
    * @param r Slice to apply to rows
    * @param c Slice to apply to cols
    */
  def at(r: Slice[Int], c: Slice[Int]): Mat[T] =
    row(r).col(c)

  /**
    * Create Mat comprised of same values in specified rows
    */
  def takeRows(locs: Int*): Mat[T] = takeRows(locs.toArray)

  /**
    * Create Mat comprised of same values in specified columns
    */
  def takeCols(locs: Array[Int]): Mat[T] = T.takeRows(locs).T

  /**
    * Create Mat comprised of same values in specified columns
    */
  def takeCols(locs: Int*): Mat[T] = takeCols(locs.toArray)

  /**
    * Create Mat comprised of same values without the specified rows
    *
    * @param locs Row locations to exclude
    */
  def withoutRows(locs: Int*): Mat[T] = withoutRows(locs.toArray)

  /**
    * Create Mat comprised of same values without the specified columns
    *
    * @param locs Col locations to exclude
    */
  def withoutCols(locs: Array[Int]): Mat[T] = T.withoutRows(locs).T

  /**
    * Create Mat comprised of same values without the specified columns
    *
    * @param locs Col locations to exclude
    */
  def withoutCols(locs: Int*): Mat[T] = withoutCols(locs.toArray)

  /**
    * Yields row indices where row has some NA value
    */
  def rowsWithNA: Set[Int] = {
    val builder = Set.newBuilder[Int]
    var i = 0
    while (i < numRows) {
      if (row(i).hasNA) builder += i
      i += 1
    }
    builder.result()
  }

  /**
    * Yields column indices where column has some NA value
    */
  def colsWithNA: Set[Int] = T.rowsWithNA

  /**
    * Yields a matrix without those rows that have NA
    */
  def dropRowsWithNA: Mat[T] = withoutRows(rowsWithNA.toArray)

  /**
    * Yields a matrix without those cols that have NA
    */
  def dropColsWithNA: Mat[T] = withoutCols(colsWithNA.toArray)

  /**
    * Returns a specific column of the Mat as a Vec
    *
    * @param c Column index
    */
  def col(c: Int): Vec[T] = {
    assert(c >= 0 && c < numCols, "Array index %d out of bounds" format c)
    flattenT.slice(c * numRows, (c + 1) * numRows)
  }

  /**
    * Access Mat columns at a particular integer offsets
    * @param locs a sequence of integer offsets
    */
  def col(locs: Int*): Mat[T] = takeCols(locs.toArray)

  /**
    * Access Mat columns at a particular integer offsets
    * @param locs an array of integer offsets
    */
  def col(locs: Array[Int]): Mat[T] = takeCols(locs)

  /**
    * Access mat columns specified by a slice
    * @param slice a slice specifier
    */
  def col(slice: Slice[Int]): Mat[T] = {
    val (a, b) = slice(IndexIntRange(numCols))
    takeCols(a until b toArray)
  }

  /**
    * Returns columns of Mat as an indexed sequence of Vec instances
    */
  def cols(): IndexedSeq[Vec[T]] = Range(0, numCols).map(col)

  /**
    * Returns columns of Mat as an indexed sequence of Vec instances
    */
  def cols(seq: IndexedSeq[Int]): IndexedSeq[Vec[T]] = seq.map(col)

  /**
    * Returns a specific row of the Mat as a Vec
    *
    * @param r Row index
    */
  def row(r: Int): Vec[T] = {
    assert(r >= 0 && r < numRows, "Array index %d out of bounds" format r)
    toVec.slice(r * numCols, (r + 1) * numCols)
  }

  /**
    * Access Mat rows at a particular integer offsets
    * @param locs a sequence of integer offsets
    */
  def row(locs: Int*): Mat[T] = takeRows(locs.toArray)

  /**
    * Access Mat rows at a particular integer offsets
    * @param locs an array of integer offsets
    */
  def row(locs: Array[Int]): Mat[T] = takeRows(locs)

  /**
    * Access Mat rows specified by a slice
    * @param slice a slice specifier
    */
  def row(slice: Slice[Int]): Mat[T] = {
    val (a, b) = slice(IndexIntRange(numCols))
    takeRows(a until b toArray)
  }

  /**
    * Returns rows of matrix as an indexed sequence of Vec instances
    */
  def rows(): IndexedSeq[Vec[T]] = Range(0, numRows).map(row)

  /**
    * Returns rows of matrix as an indexed sequence of Vec instances
    */
  def rows(seq: IndexedSeq[Int]): IndexedSeq[Vec[T]] = seq.map(row)

  /**
    * Rounds elements in the matrix (which must be numeric) to
    * a significance level
    *
    * @param sig Significance level to round to (e.g., 2 decimal places)
    */
  def roundTo(sig: Int = 2)(implicit ev: NUM[T]): Mat[Double] = {
    val pwr = math.pow(10, sig)
    val rounder = (x: T) => math.round(scalarTag.toDouble(x) * pwr) / pwr
    map(rounder)
  }

  private def flattenT: Vec[T] = T.toVec

  /**
    * Creates a string representation of Mat
    * @param nrows Max number of rows to include
    * @param ncols Max number of cols to include
    */
  def stringify(nrows: Int = 8, ncols: Int = 8): String = {
    val halfr = nrows / 2
    val halfc = ncols / 2

    val buf = new StringBuilder()
    buf.append("[%d x %d]\n".format(numRows, numCols))

    implicit val st = scalarTag

    val maxStrLen = (a: Int, b: String) => a.max(b.length)
    val maxColLen = (c: Vec[T]) =>
      (c.head(halfr) concat c.tail(halfr))
        .map(scalarTag.show(_))
        .foldLeft(0)(maxStrLen)
    val colIdx = util.grab(Range(0, numCols), halfc)
    val lenSeq = colIdx.map { c =>
      c -> maxColLen(col(c))
    }
    val lenMap = lenSeq.toMap.withDefault(_ => 1)

    // function to build a row
    def createRow(r: Int) = {
      val buf = new StringBuilder()
      val strFn = (col: Int) => {
        val l = lenMap(col)
        "%" + { if (l > 0) l else 1 } + "s " format scalarTag.show(raw(r, col))
      }
      buf.append(util.buildStr(ncols, numCols, strFn))
      buf.append("\n")
      buf.toString()
    }

    // build all rows
    buf.append(util.buildStr(nrows, numRows, createRow, "...\n"))
    buf.toString()
  }

  override def toString = stringify()

  /**
    * Pretty-printer for Mat, which simply outputs the result of stringify.
    * @param nrows Number of elements to display
    */
  def print(
      nrows: Int = 8,
      ncols: Int = 8,
      stream: OutputStream = System.out
  ) = {
    stream.write(stringify(nrows, ncols).getBytes)
  }

  /** Default hashcode is simple rolling prime multiplication of sums of hashcodes for all values. */
  override def hashCode(): Int = toVec.foldLeft(1)(_ * 31 + _.hashCode())

  /**
    * Converst to Frame
    */
  def toFrame = Frame(this)

  def mutateSetCell(r: Int, c: Int, v: T): Unit = {
    values(r * numCols + c) = v
  }
  def mutateSetRow(r: Int, v: T): Unit = {
    var i = 0
    val rr = r * numCols
    while (i < numCols) {
      values(rr + i) = v
      i += 1
    }
  }
  def mutateSetColumn(c: Int, v: T): Unit = {
    var i = 0
    while (i < numRows) {
      values(i * numCols + c) = v
      i += 1
    }
  }
  def mutateSetDiagonal(v: T): Unit = {
    val n = math.min(numCols, numRows)
    var i = 0
    while (i < n) {
      values(i * numCols + i) = v
      i += 1
    }
  }
  def mutateSetLowerTriangle(v: T): Unit = {
    val n = math.min(numCols, numRows)
    var i = 0
    var j = 0
    while (i < n) {
      while (j < i) {
        values(i * numCols + j) = v
        j += 1
      }
      j = 0
      i += 1
    }
  }
  def mutateSetUpperTriangle(v: T): Unit = {
    val n = math.min(numCols, numRows)
    var i = 0
    var j = i + 1
    while (i < n) {
      while (j < n) {
        values(i * numCols + j) = v
        j += 1
      }
      j = i + 1
      i += 1
    }
  }

}
