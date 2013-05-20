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

package org.saddle

import mat._
import scalar.{Scalar, ScalarTag}
import ops.{BinOpMat, NumericOps}
import scala.{specialized => spec}
import java.io.OutputStream
import org.saddle.index.{IndexIntRange, Slice}

/**
 * `Mat` is an immutable container for 2D homogeneous data (a "matrix"). It is
 * backed by a single array. Data is stored in row-major order.
 *
 * Several element access methods are provided.
 *
 * The `at` method returns an instance of a [[org.saddle.scalar.Scalar]], which behaves
 * much like an `Option` in that it can be either an instance of [[org.saddle.scalar.NA]]
 * or a [[org.saddle.scalar.Value]] case class:
 *
 * {{{
 *   val m = Mat(2,2,Array(1,2,3,4))
 *   m.at(0,0) == Value(1)
 * }}}
 *
 * The method `raw` accesses the underlying value directly.
 *
 * {{{
 *   val m = Mat(2,2,Array(1,2,3,4))
 *   m.raw(0,0) == 1d
 * }}}
 *
 * `Mat` may be used in arithemetic expressions which operate on two `Mat`s or on a
 * `Mat` and a primitive value. A fe examples:
 *
 * {{{
 *   val m = Mat(2,2,Array(1,2,3,4))
 *   m * m == Mat(2,2,Array(1,4,9,16))
 *   m dot m == Mat(2,2,Array(7d,10,15,22))
 *   m * 3 == Mat(2, 2, Array(3,6,9,12))
 * }}}
 *
 * Note, Mat is generally compatible with EJML's DenseMatrix. It may be convenient
 * to induce this conversion to do more complex linear algebra, or to work with a
 * mutable data structure.
 *
 * @tparam A Type of elements within the Mat
 */
trait Mat[@spec(Boolean, Int, Long, Double) A] extends NumericOps[Mat[A]] {
  def scalarTag: ScalarTag[A]

  /**
   * Returns number of rows in the matrix shape
   *
   */
  def numRows: Int

  /**
   * Returns number of columns in the matrix shape
   *
   */
  def numCols: Int

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
   * Return unboxed value of matrix at an offset from zero in row-major order
   *
   * @param i index
   */
  def raw(i: Int): A = apply(i)

  /**
   * Return unboxed value of matrix at row/column
   *
   * @param r row index
   * @param c col index
   */
  def raw(r: Int, c: Int): A = apply(r, c)

  /**
   * Return scalar value of matrix at offset from zero in row-major order
   *
   * @param i index
   */
  def at(i: Int)(implicit st: ScalarTag[A]): Scalar[A] = {
    Scalar(raw(i))
  }

  /**
   * Return scalar value of Mat at at row/column
   * @param r row index
   * @param c col index
   */
  def at(r: Int, c: Int)(implicit st: ScalarTag[A]): Scalar[A] = {
    Scalar(raw(r, c))
  }

  /**
   * Access a slice of the Mat by integer offsets
   * @param r Array of row offsets
   * @param c Array of col offsets
   */
  def at(r: Array[Int], c: Array[Int])(implicit st: ScalarTag[A]): Mat[A] = {
    row(r).col(c)
  }

  /**
   * Access a slice of the Mat by integer offsets
   * @param r Array of row offsets
   * @param c Integer col offset
   */
  def at(r: Array[Int], c: Int)(implicit st: ScalarTag[A]): Vec[A] = {
    row(r).col(c)
  }

  /**
   * Access a slice of the Mat by integer offsets
   * @param r Integer row offset
   * @param c Array of col offsets
   */
  def at(r: Int, c: Array[Int])(implicit st: ScalarTag[A]): Vec[A] = {
    col(c).row(r)
  }

  /**
   * Access a slice of the Mat by Slice parameters
   * @param r Slice to apply to rows
   * @param c Slice to apply to cols
   */
  def at(r: Slice[Int], c: Slice[Int])(implicit st: ScalarTag[A]): Mat[A] =
    row(r).col(c)

  /**
   * Returns (a copy of) the contents of matrix as a single array in
   * row-major order
   *
   */
  def contents: Array[A] = toVec.toArray

  // Must implement specialized methods using non-specialized subclasses as workaround to
  // https://issues.scala-lang.org/browse/SI-5281

  /**
   * Maps a function over each element in the matrix
   */
  def map[@spec(Boolean, Int, Long, Double) B: ST](f: A => B): Mat[B]

  /**
   * Changes the shape of matrix without changing the underlying data
   */
  def reshape(r: Int, c: Int): Mat[A]

  /**
   * Transpose of original matrix
   */
  def transpose: Mat[A]

  /**
   * Transpose of original matrix
   */
  def T = transpose

  /**
   * Create Mat comprised of same values in specified rows
   */
  def takeRows(locs: Array[Int]): Mat[A]

  /**
   * Create Mat comprised of same values in specified rows
   */
  def takeRows(locs: Int*): Mat[A] = takeRows(locs.toArray)

  /**
   * Create Mat comprised of same values in specified columns
   */
  def takeCols(locs: Array[Int]): Mat[A] = T.takeRows(locs).T

  /**
   * Create Mat comprised of same values in specified columns
   */
  def takeCols(locs: Int*): Mat[A] = takeCols(locs.toArray)

  /**
   * Create Mat comprised of same values without the specified rows
   *
   * @param locs Row locations to exclude
   */
  def withoutRows(locs: Array[Int]): Mat[A]

  /**
   * Create Mat comprised of same values without the specified rows
   *
   * @param locs Row locations to exclude
   */
  def withoutRows(locs: Int*): Mat[A] = withoutRows(locs.toArray)

  /**
   * Create Mat comprised of same values without the specified columns
   *
   * @param locs Col locations to exclude
   */
  def withoutCols(locs: Array[Int]): Mat[A] = T.withoutRows(locs).T

  /**
   * Create Mat comprised of same values without the specified columns
   *
   * @param locs Col locations to exclude
   */
  def withoutCols(locs: Int*): Mat[A] = withoutCols(locs.toArray)

  /**
   * Yields row indices where row has some NA value
   */
  def rowsWithNA(implicit ev: ST[A]): Set[Int] = {
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
  def colsWithNA(implicit ev: ST[A]): Set[Int] = T.rowsWithNA

  /**
   * Yields a matrix without those rows that have NA
   */
  def dropRowsWithNA(implicit ev: ST[A]): Mat[A] = withoutRows(rowsWithNA.toArray)

  /**
   * Yields a matrix without those cols that have NA
   */
  def dropColsWithNA(implicit ev: ST[A]): Mat[A] = withoutCols(colsWithNA.toArray)

  /**
   * Returns a specific column of the Mat as a Vec
   *
   * @param c Column index
   */
  def col(c: Int)(implicit ev: ST[A]): Vec[A] = {
    assert(c >= 0 && c < numCols, "Array index %d out of bounds" format c)
    flattenT.slice(c * numRows, (c + 1) * numRows)
  }

  /**
   * Access Mat columns at a particular integer offsets
   * @param locs a sequence of integer offsets
   */
  def col(locs: Int*)(implicit ev: ST[A]): Mat[A] = takeCols(locs.toArray)

  /**
   * Access Mat columns at a particular integer offsets
   * @param locs an array of integer offsets
   */
  def col(locs: Array[Int])(implicit ev: ST[A]): Mat[A] = takeCols(locs)

  /**
   * Access mat columns specified by a slice
   * @param slice a slice specifier
   */
  def col(slice: Slice[Int]): Mat[A] = {
    val (a, b) = slice(IndexIntRange(numCols))
    takeCols(a until b toArray)
  }

  /**
   * Returns columns of Mat as an indexed sequence of Vec instances
   */
  def cols()(implicit ev: ST[A]): IndexedSeq[Vec[A]] = Range(0, numCols).map(col _)

  /**
   * Returns columns of Mat as an indexed sequence of Vec instances
   */
  def cols(seq: IndexedSeq[Int])(implicit ev: ST[A]): IndexedSeq[Vec[A]] = seq.map(col _)

  /**
   * Returns a specific row of the Mat as a Vec
   *
   * @param r Row index
   */
  def row(r: Int)(implicit ev: ST[A]): Vec[A] = {
    assert(r >= 0 && r < numRows, "Array index %d out of bounds" format r)
    flatten.slice(r * numCols, (r + 1) * numCols)
  }

  /**
   * Access Mat rows at a particular integer offsets
   * @param locs a sequence of integer offsets
   */
  def row(locs: Int*)(implicit ev: ST[A]): Mat[A] = takeRows(locs.toArray)

  /**
   * Access Mat rows at a particular integer offsets
   * @param locs an array of integer offsets
   */
  def row(locs: Array[Int])(implicit ev: ST[A]): Mat[A] = takeRows(locs)

  /**
   * Access Mat rows specified by a slice
   * @param slice a slice specifier
   */
  def row(slice: Slice[Int]): Mat[A] = {
    val (a, b) = slice(IndexIntRange(numCols))
    takeRows(a until b toArray)
  }

  /**
   * Returns rows of matrix as an indexed sequence of Vec instances
   */
  def rows()(implicit ev: ST[A]): IndexedSeq[Vec[A]] = Range(0, numRows).map(row _)

  /**
   * Returns rows of matrix as an indexed sequence of Vec instances
   */
  def rows(seq: IndexedSeq[Int])(implicit ev: ST[A]): IndexedSeq[Vec[A]] = seq.map(row _)

  /**
   * Multiplies this matrix against another
   *
   */
  def mult[B](m: Mat[B])(implicit evA: NUM[A], evB: NUM[B]): Mat[Double] = {
    if (numCols != m.numRows) {
      val errMsg = "Cannot multiply (%d %d) x (%d %d)".format(numRows, numCols, m.numRows, m.numCols)
      throw new IllegalArgumentException(errMsg)
    }

    MatMath.mult(this, m)
  }

  /**
   * Rounds elements in the matrix (which must be numeric) to
   * a significance level
   *
   * @param sig Significance level to round to (e.g., 2 decimal places)
   */
  def roundTo(sig: Int = 2)(implicit ev: NUM[A]): Mat[Double] = {
    val pwr = math.pow(10, sig)
    val rounder = (x: A) => math.round(scalarTag.toDouble(x) * pwr) / pwr
    map(rounder)
  }

  /**
   * Concatenate all rows into a single row-wise Vec instance
   */
  def toVec: Vec[A]

  private var flatCache: Option[Vec[A]] = None
  private def flatten(implicit st: ST[A]): Vec[A] = flatCache.getOrElse {
    this.synchronized {
      flatCache = Some(toVec)
      flatCache.get
    }
  }

  private var flatCacheT: Option[Vec[A]] = None
  private def flattenT(implicit st: ST[A]): Vec[A] = flatCacheT.getOrElse {
    this.synchronized {
      flatCacheT = Some(T.toVec)
      flatCacheT.get
    }
  }

  // access like vector in row-major order
  private[saddle] def apply(i: Int): A

  // implement access like matrix(i, j)
  private[saddle] def apply(r: Int, c: Int): A

  // use with caution, may not return copy
  private[saddle] def toArray: Array[A]

  // use with caution, may not return copy
  private[saddle] def toDoubleArray(implicit ev: NUM[A]): Array[Double]

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
    val maxColLen = (c: Vec[A]) => (c.head(halfr) concat c.tail(halfr)).map(scalarTag.show(_)).foldLeft(0)(maxStrLen)
    val colIdx = util.grab(Range(0, numCols), halfc)
    val lenSeq = colIdx.map { c => c -> maxColLen(col(c)) }
    val lenMap = lenSeq.toMap.withDefault(_ => 1)

    // function to build a row
    def createRow(r: Int) = {
      val buf = new StringBuilder()
      val strFn = (col: Int) => {
        val l = lenMap(col)
        "%" + { if (l > 0) l else 1 } + "s " format scalarTag.show(apply(r, col))
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
  def print(nrows: Int = 8, ncols: Int = 8, stream: OutputStream = System.out) {
    stream.write(stringify(nrows, ncols).getBytes)
  }

  /** Default hashcode is simple rolling prime multiplication of sums of hashcodes for all values. */
  override def hashCode(): Int = toVec.foldLeft(1)(_ * 31 + _.hashCode())

  /**
   * Row-by-row equality check of all values.
   * NB: to avoid boxing, overwrite in child classes
   */
  override def equals(o: Any): Boolean = o match {
    case rv: Mat[_] => (this eq rv) || this.numRows == rv.numRows && this.numCols == rv.numCols && {
      var i = 0
      var eq = true
      while(eq && i < length) {
        eq &&= (apply(i) == rv(i) || this.scalarTag.isMissing(apply(i)) && rv.scalarTag.isMissing(rv(i)))
        i += 1
      }
      eq
    }
    case _ => false
  }
}

object Mat extends BinOpMat {
  /**
   * Factory method to create a new Mat from raw materials
   * @param rows Number of rows in Mat
   * @param cols Number of cols in Mat
   * @param arr A 1D array of backing data in row-major order
   * @tparam T Type of data in array
   */
  def apply[T](rows: Int, cols: Int, arr: Array[T])(implicit st: ST[T]): Mat[T] = {
    val (r, c, a) = if (rows == 0 || cols == 0) (0, 0, Array.empty[T]) else (rows, cols, arr)
    st.makeMat(r, c, a)
  }

  /**
   * Allows implicit promoting from a Mat to a Frame instance
   * @param m Mat instance
   * @tparam T The type of elements in Mat
   */
  implicit def matToFrame[T: ST](m: Mat[T]) = Frame(m)

  /**
   * Factory method to create an empty Mat
   * @tparam T Type of Mat
   */
  def empty[T: ST]: Mat[T] = apply(0, 0, Array.empty[T])

  /**
   * Factory method to create an zero Mat (all zeros)
   * @param numRows Number of rows in Mat
   * @param numCols Number of cols in Mat
   * @tparam T Type of elements in Mat
   */
  def apply[T: ST](numRows: Int, numCols: Int): Mat[T] =
    apply(numRows, numCols, Array.ofDim[T](numRows * numCols))

  /**
   * Factory method to create a Mat from an array of arrays. Each inner array
   * will become a column of the new Mat instance.
   * @param values Array of arrays, each of which is to be a column
   * @tparam T Type of elements in inner array
   */
  def apply[T: ST](values: Array[Array[T]]): Mat[T] = implicitly[ST[T]].makeMat(values.map(Vec(_)))

  /**
   * Factory method to create a Mat from an array of Vec. Each inner Vec
   * will become a column of the new Mat instance.
   * @param values Array of Vec, each of which is to be a column
   * @tparam T Type of elements in Vec
   */
  def apply[T: ST](values: Array[Vec[T]]): Mat[T] = implicitly[ST[T]].makeMat(values)

  /**
   * Factory method to create a Mat from a sequence of Vec. Each inner Vec
   * will become a column of the new Mat instance.
   * @param values Sequence of Vec, each of which is to be a column
   * @tparam T Type of elements in array
   */
  def apply[T: ST](values: Vec[T]*): Mat[T] = implicitly[ST[T]].makeMat(values.toArray)

  /**
   * Factory method to create an identity matrix; ie with ones along the
   * diagonal and zeros off-diagonal.
   * @param n The width of the square matrix
   */
  def ident(n: Int): Mat[Double] = mat.ident(n)
}

