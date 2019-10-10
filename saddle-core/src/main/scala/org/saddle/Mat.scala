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

import scalar.{Scalar, ScalarTag}
import ops.{BinOpMat, NumericOps}
import scala.{specialized => spec}
import java.io.OutputStream
import org.saddle.index.Slice
import org.saddle.mat.MatDefault

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
  def length: Int

  /**
    * Returns true if rows == cols
    *
    */
  def isSquare: Boolean

  /**
    * Returns true if the matrix is empty
    *
    */
  def isEmpty: Boolean

  /**
    * Return unboxed value of matrix at an offset from zero in row-major order
    *
    * @param i index
    */
  def raw(i: Int): A

  /**
    * Return unboxed value of matrix at row/column
    *
    * @param r row index
    * @param c col index
    */
  def raw(r: Int, c: Int): A

  /**
    * Return scalar value of matrix at offset from zero in row-major order
    *
    * @param i index
    */
  def at(i: Int): Scalar[A]

  /**
    * Return scalar value of Mat at at row/column
    * @param r row index
    * @param c col index
    */
  def at(r: Int, c: Int): Scalar[A]

  /**
    * Access a slice of the Mat by integer offsets
    * @param r Array of row offsets
    * @param c Array of col offsets
    */
  def at(r: Array[Int], c: Array[Int]): Mat[A]

  /**
    * Access a slice of the Mat by integer offsets
    * @param r Array of row offsets
    * @param c Integer col offset
    */
  def at(r: Array[Int], c: Int): Vec[A]

  /**
    * Access a slice of the Mat by integer offsets
    * @param r Integer row offset
    * @param c Array of col offsets
    */
  def at(r: Int, c: Array[Int]): Vec[A]

  /**
    * Access a slice of the Mat by Slice parameters
    * @param r Slice to apply to rows
    * @param c Slice to apply to cols
    */
  def at(r: Slice[Int], c: Slice[Int]): Mat[A]

  /**
    * Returns (a copy of) the contents of matrix as a single array in
    * row-major order
    *
    */
  def contents: Array[A]

  // Must implement specialized methods using non-specialized subclasses as workaround to
  // https://issues.scala-lang.org/browse/SI-5281

  /**
    * Maps a function over each element in the matrix
    */
  def map[@spec(Boolean, Int, Long, Double) B: ST](f: A => B): Mat[B]

  /**
    * Changes the shape of matrix without changing the underlying data
    *
    * Backing array will be shared between the two instances!
    */
  def reshape(r: Int, c: Int): Mat[A]

  /**
    * Transpose of original matrix
    */
  def transpose: Mat[A]

  /**
    * Transpose of original matrix
    */
  def T: Mat[A]

  /**
    * Create Mat comprised of same values in specified rows
    */
  def takeRows(locs: Array[Int]): Mat[A]
  
  /**
    * Create Mat comprised of same values in specified rows
    */
  def takeRows(locs: Vec[Int]): Mat[A]

  /**
    * Create Mat comprised of same values in specified rows
    */
  def takeRows(locs: Int*): Mat[A]

  /**
    * Create Mat comprised of same values in specified columns
    */
  def takeCols(locs: Array[Int]): Mat[A]

  /**
    * Create Mat comprised of same values in specified columns
    */
  def takeCols(locs: Int*): Mat[A]

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
  def withoutRows(locs: Int*): Mat[A]

  /**
    * Create Mat comprised of same values without the specified columns
    *
    * @param locs Col locations to exclude
    */
  def withoutCols(locs: Array[Int]): Mat[A]

  /**
    * Create Mat comprised of same values without the specified columns
    *
    * @param locs Col locations to exclude
    */
  def withoutCols(locs: Int*): Mat[A]

  /**
    * Yields row indices where row has some NA value
    */
  def rowsWithNA: Set[Int]

  /**
    * Yields column indices where column has some NA value
    */
  def colsWithNA: Set[Int]

  /**
    * Yields a matrix without those rows that have NA
    */
  def dropRowsWithNA: Mat[A]

  /**
    * Yields a matrix without those cols that have NA
    */
  def dropColsWithNA: Mat[A]

  /**
    * Returns a specific column of the Mat as a Vec
    *
    * @param c Column index
    */
  def col(c: Int): Vec[A]

  /**
    * Access Mat columns at a particular integer offsets
    * @param locs a sequence of integer offsets
    */
  def col(locs: Int*): Mat[A]

  /**
    * Access Mat columns at a particular integer offsets
    * @param locs an array of integer offsets
    */
  def col(locs: Array[Int]): Mat[A]

  /**
    * Access mat columns specified by a slice
    * @param slice a slice specifier
    */
  def col(slice: Slice[Int]): Mat[A]

  /**
    * Returns columns of Mat as an indexed sequence of Vec instances
    */
  def cols(): IndexedSeq[Vec[A]]

  /**
    * Returns columns of Mat as an indexed sequence of Vec instances
    */
  def cols(seq: IndexedSeq[Int]): IndexedSeq[Vec[A]]

  /**
    * Returns a specific row of the Mat as a Vec
    *
    * @param r Row index
    */
  def row(r: Int): Vec[A]

  /**
    * Access Mat rows at a particular integer offsets
    * @param locs a sequence of integer offsets
    */
  def row(locs: Int*): Mat[A]

  /**
    * Access Mat rows at a particular integer offsets
    * @param locs an array of integer offsets
    */
  def row(locs: Array[Int]): Mat[A]

  /**
    * Access Mat rows specified by a slice
    * @param slice a slice specifier
    */
  def row(slice: Slice[Int]): Mat[A]

  /**
    * Returns rows of matrix as an indexed sequence of Vec instances
    */
  def rows(): IndexedSeq[Vec[A]]

  /**
    * Returns rows of matrix as an indexed sequence of Vec instances
    */
  def rows(seq: IndexedSeq[Int]): IndexedSeq[Vec[A]]

  /**
    * Rounds elements in the matrix (which must be numeric) to
    * a significance level
    *
    * @param sig Significance level to round to (e.g., 2 decimal places)
    */
  def roundTo(sig: Int = 2)(implicit ev: NUM[A]): Mat[Double]

  /**
    * Concatenate all rows into a single row-wise Vec instance
    *
    * Underlying array is shared between the two instances
    */
  def toVec: Vec[A]

  /**
    * Makes a copy of this Mat
    *
    */
  def copy: Mat[A]

  /** Returns the backing array of this Mat
    * Mutations to this array are visible to this Mat
    *
    * Elements are laid out in row-major order
    * Constant time operation
    */
  def toArray: Array[A]

  // use with caution, may not return copy
  private[saddle] def toDoubleArray(implicit ev: NUM[A]): Array[Double]

  /**
    * Creates a string representation of Mat
    * @param nrows Max number of rows to include
    * @param ncols Max number of cols to include
    */
  def stringify(nrows: Int = 8, ncols: Int = 8): String

  /**
    * Pretty-printer for Mat, which simply outputs the result of stringify.
    * @param nrows Number of elements to display
    */
  def print(
      nrows: Int = 8,
      ncols: Int = 8,
      stream: OutputStream = System.out
  ): Unit

  def toFrame: Frame[Int, Int, A]

  def mutateSetCell(r: Int, c: Int, v: A): Unit
  def mutateSetRow(r: Int, v: A): Unit
  def mutateSetColumn(c: Int, v: A): Unit
  def mutateSetDiagonal(v: A): Unit
  def mutateSetUpperTriangle(v: A): Unit
  def mutateSetLowerTriangle(v: A): Unit
}

object Mat extends BinOpMat {

  /**
    * Factory method to create a new Mat from raw materials
    * @param rows Number of rows in Mat
    * @param cols Number of cols in Mat
    * @param arr A 1D array of backing data in row-major order
    * @tparam T Type of data in array
    */
  def apply[@spec(Boolean, Int, Long, Double) T](
      rows: Int,
      cols: Int,
      arr: Array[T]
  )(implicit st: ST[T]): Mat[T] =
    if (rows == 0 || cols == 0) new MatDefault(0, 0, Array.empty[T], st)
    else new MatDefault(rows, cols, arr, st)

  def apply[@spec(Boolean, Int, Long, Double) T](
      rows: Int,
      cols: Int,
      vec: Vec[T]
  )(implicit st: ST[T]): Mat[T] =
    if (rows == 0 || cols == 0) new MatDefault(0, 0, Array.empty[T], st)
    else new MatDefault(rows, cols, vec.toArray, st)

  /**
    * Factory method to create an empty Mat
    * @tparam T Type of Mat
    */
  def empty[@spec(Boolean, Int, Long, Double) T: ST]: Mat[T] =
    apply(0, 0, Array.empty[T])

  /**
    * Factory method to create an zero Mat (all zeros)
    * @param numRows Number of rows in Mat
    * @param numCols Number of cols in Mat
    * @tparam T Type of elements in Mat
    */
  def apply[@spec(Boolean, Int, Long, Double) T](numRows: Int, numCols: Int)(
      implicit st: ST[T]
  ): Mat[T] =
    apply(numRows, numCols, Array.ofDim[T](numRows * numCols))(st)

  /**
    * Factory method to create a Mat from an array of arrays. Each inner array
    * will become a column of the new Mat instance.
    * @param values Array of arrays, each of which is to be a column
    * @tparam T Type of elements in inner array
    */
  def apply[T: ST](values: Array[Array[T]]): Mat[T] =
    implicitly[ST[T]].makeMat(values.map(Vec(_)))

  /**
    * Factory method to create a Mat from an array of Vec. Each inner Vec
    * will become a column of the new Mat instance.
    * @param values Array of Vec, each of which is to be a column
    * @tparam T Type of elements in Vec
    */
  def apply[T: ST](values: Array[Vec[T]]): Mat[T] =
    implicitly[ST[T]].makeMat(values)

  /**
    * Factory method to create a Mat from a sequence of Vec. Each inner Vec
    * will become a column of the new Mat instance.
    * @param values Sequence of Vec, each of which is to be a column
    * @tparam T Type of elements in array
    */
  def apply[T: ST](values: Vec[T]*): Mat[T] =
    implicitly[ST[T]].makeMat(values.toArray)

  /**
    * Factory method to create an identity matrix; ie with ones along the
    * diagonal and zeros off-diagonal.
    * @param n The width of the square matrix
    */
  def ident(n: Int): Mat[Double] = mat.ident(n)
}
