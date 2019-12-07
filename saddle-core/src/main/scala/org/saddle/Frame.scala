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

import scala.{specialized => spec}
import org.saddle.vec.VecImpl
import org.saddle.index.{
  JoinType,
  LeftJoin,
  OuterJoin,
  IndexIntRange,
  Slice,
  RightJoin,
  Melter,
  Stacker,
  Splitter
}
import org.saddle.groupby.{FrameGrouper, IndexGrouper}
import org.saddle.ops.{NumericOps, BinOpFrame}
import scalar.Scalar
import java.io.OutputStream
import org.saddle.mat.MatCols

/**
  * `Frame` is an immutable container for 2D data which is indexed along both axes
  * (rows, columns) by associated keys (i.e., indexes).
  *
  * The primary use case is homogeneous data, but a secondary concern is to support
  * heterogeneous data that is homogeneous ony within any given column.
  *
  * The row index, column index, and constituent value data are all backed ultimately
  * by arrays.
  *
  * `Frame` is effectively a doubly-indexed associative map whose row keys and col keys
  * each have an ordering provided by the natural (provided) order of their backing
  * arrays.
  *
  * Several factory and access methods are provided. In the following examples, assume
  * that:
  *
  * {{{
  *   val f = Frame('a'->Vec(1,2,3), 'b'->Vec(4,5,6))
  * }}}
  *
  * The `apply` method takes a row and col key returns a slice of the original Frame:
  *
  * {{{
  *   f(0,'a') == Frame('a'->Vec(1))
  * }}}
  *
  * `apply` also accepts a [[org.saddle.index.Slice]]:
  *
  * {{{
  *   f(0->1, 'b') == Frame('b'->Vec(4,5))
  *   f(0, *) == Frame('a'->Vec(1), 'b'->Vec(4))
  * }}}
  *
  * You may slice using the `col` and `row` methods respectively, as follows:
  *
  * {{{
  *   f.col('a') == Frame('a'->Vec(1,2,3))
  *   f.row(0) == Frame('a'->Vec(1), 'b'->Vec(4))
  *   f.row(0->1) == Frame('a'->Vec(1,2), 'b'->Vec(4,5))
  * }}}
  *
  * You can achieve a similar effect with `rowSliceBy` and `colSliceBy`
  *
  * The `colAt` and `rowAt` methods take an integer offset i into the Frame, and
  * return a Series indexed by the opposing axis:
  *
  * {{{
  *   f.rowAt(0) == Series('a'->1, 'b'->4)
  * }}}
  *
  * If there is a one-to-one relationship between offset i and key (ie, no duplicate
  * keys in the index), you may achieve the same effect via key as follows:
  *
  * {{{
  *   f.first(0) == Series('a'->1, 'b'->4)
  *   f.firstCol('a') == Series(1,2,3)
  * }}}
  *
  * The `at` method returns an instance of a [[org.saddle.scalar.Scalar]], which behaves
  * much like an `Option`; it can be either an instance of [[org.saddle.scalar.NA]] or a
  * [[org.saddle.scalar.Value]] case class:
  *
  * {{{
  *   f.at(0, 0) == scalar.Scalar(1)
  * }}}
  *
  * The `rowSlice` and `colSlice` methods allows slicing the Frame for locations in [i, j)
  * irrespective of the value of the keys at those locations.
  *
  * {{{
  *   f.rowSlice(0,1) == Frame('a'->Vec(1), 'b'->Vec(4))
  * }}}
  *
  * Finally, the method `raw` accesses a value directly, which may reveal the underlying
  * representation of a missing value (so be careful).
  *
  * {{{
  *   f.raw(0,0) == 1
  * }}}
  *
  * `Frame` may be used in arithmetic expressions which operate on two `Frame`s or on a
  * `Frame` and a scalar value. In the former case, the two Frames will automatically
  * align along their indexes:
  *
  * {{{
  *   f + f.shift(1) == Frame('a'->Vec(NA,3,5), 'b'->Vec(NA,9,11))
  * }}}
  *
  * @param values A sequence of Vecs which comprise the columns of the Frame
  * @param rowIx An index for the rows
  * @param colIx An index for the columns
  * @tparam RX The type of row keys
  * @tparam CX The type of column keys
  * @tparam T The type of entries in the frame
  */
class Frame[RX: ST: ORD, CX: ST: ORD, @spec(Int, Long, Double) T](
    private[saddle] val values: MatCols[T],
    val rowIx: Index[RX],
    val colIx: Index[CX],
    private var cachedMat: Option[Mat[T]]
)(implicit st: ST[T])
    extends NumericOps[Frame[RX, CX, T]] {

  require(values.numRows == rowIx.length, "Row index length is incorrect")
  require(values.numCols == colIx.length, "Col index length is incorrect")

  /**
    * Number of rows in the Frame
    */
  def numRows: Int = values.numRows

  /**
    * Number of cols in the Frame
    */
  def numCols: Int = values.numCols

  /**
    * Returns true if there are no values in the Frame
    */
  def isEmpty: Boolean = (values.numRows == 0)

  /**
    * The transpose of the frame (swapping the axes)
    */
  def T: Frame[CX, RX, T] =
    Frame(MatCols(toMat.rows(): _*), colIx, rowIx)

  // ---------------------------------------------------------------
  // extract columns by associated key(s); ignore non-existent keys

  /**
    * Given one or more column keys, slice out the corresponding column(s)
    * @param keys Column key(s) (sequence)
    */
  def col(keys: CX*): Frame[RX, CX, T] = col(keys.toArray)

  /**
    * Given a Slice of type of column key, slice out corresponding column(s)
    * @param slice Slice containing appropriate key bounds
    */
  def col(slice: Slice[CX]): Frame[RX, CX, T] = {
    val (a, b) = slice(colIx)
    Frame(values.slice(a, b), rowIx, colIx.sliceBy(slice))
  }

  /**
    * Given an array of column keys, slice out the corresponding column(s)
    * @param keys Array of keys
    */
  def col(keys: Array[CX]): Frame[RX, CX, T] = {
    if (values.numCols == 0)
      Frame.empty[RX, CX, T]
    else {
      val locs = array.filter[Int](_ != -1)(colIx(keys))
      colAt(locs)
    }
  }

  /**
    * Slice out a set of columns from the frame
    * @param from Key from which to begin slicing
    * @param to Key at which to end slicing
    * @param inclusive Whether to include 'to' key; true by default
    */
  def colSliceBy(
      from: CX,
      to: CX,
      inclusive: Boolean = true
  ): Frame[RX, CX, T] = {
    val tmp = Series(values: _*).setIndex(colIx)
    val res = tmp.sliceBy(from, to, inclusive)
    Frame(res.values.toArray, rowIx, res.index)
  }

  // -----------------------------------------
  // access columns by particular location(s)

  /**
    * Access frame column at a particular integer offset
    * @param loc integer offset
    */
  def colAt(loc: Int): Series[RX, T] = Series(values(loc), rowIx)

  /**
    * Access frame columns at a particular integer offsets
    * @param locs a sequence of integer offsets
    */
  def colAt(locs: Int*): Frame[RX, CX, T] = colAt(locs.toArray)

  /**
    * Access frame columns at a particular integer offsets
    * @param locs an array of integer offsets
    */
  def colAt(locs: Array[Int]): Frame[RX, CX, T] =
    if (values.numCols == 0)
      Frame.empty[RX, CX, T]
    else
      Frame(values.take(locs), rowIx, colIx.take(locs))

  /**
    * Access frame columns specified by a slice
    * @param slice a slice specifier
    */
  def colAt(slice: Slice[Int]): Frame[RX, CX, T] = {
    val idx = IndexIntRange(numCols)
    val pair = slice(idx)
    Frame(values.slice(pair._1, pair._2), rowIx, colIx.slice(pair._1, pair._2))
  }

  /**
    * Access frame columns between two integer offsets, [from, until)
    * @param from Beginning offset
    * @param until One past ending offset
    * @param stride Optional increment between offsets
    */
  def colSlice(from: Int, until: Int, stride: Int = 1): Frame[RX, CX, T] = {
    val lb = math.max(0, from)
    val ub = math.min(numCols, until)
    val taker = array.range(lb, ub, stride)
    Frame(values.take(taker), rowIx, colIx.take(taker))
  }

  /**
    * Split Frame into two frames at column position c
    * @param c Position at which to split Frame
    */
  def colSplitAt(c: Int): (Frame[RX, CX, T], Frame[RX, CX, T]) =
    (colSlice(0, c), colSlice(c, numCols))

  /**
    * Split Frame into two frames at column key k
    * @param k Key at which to split Frame
    * `k` is included in the right Frame
    * [1,2,3,4] split at 2 yields [1] and [2,3,4]
    */
  def colSplitBy(k: CX): (Frame[RX, CX, T], Frame[RX, CX, T]) =
    colSplitAt(colIx.lsearch(k))

  // ---------------------------------------------------------------
  // extract rows by associated key(s); ignore non-existent keys

  /**
    * Given one or more row keys, slice out the corresponding row(s)
    * @param keys Row key(s) (sequence)
    */
  def row(keys: RX*): Frame[RX, CX, T] = row(keys.toArray)

  /**
    * Given a Slice of type of row key, slice out corresponding row(s)
    * @param slice Slice containing appropriate key bounds
    */
  def row(slice: Slice[RX]): Frame[RX, CX, T] = {
    val (a, b) = slice(rowIx)
    Frame(values.map(v => v.slice(a, b)), rowIx.sliceBy(slice), colIx)
  }

  /**
    * Given an array of row keys, slice out the corresponding row(s)
    * @param keys Array of keys
    */
  def row(keys: Array[RX]): Frame[RX, CX, T] = {
    if (values.numRows == 0)
      Frame.empty[RX, CX, T]
    else {
      val locs = array.filter[Int](_ != -1)(rowIx(keys))
      rowAt(locs)
    }
  }

  /**
    * Slice out a set of rows from the frame
    * @param from Key from which to begin slicing
    * @param to Key at which to end slicing
    * @param inclusive Whether to include 'to' key; true by default
    */
  def rowSliceBy(
      from: RX,
      to: RX,
      inclusive: Boolean = true
  ): Frame[RX, CX, T] = {
    val start = rowIx.lsearch(from)
    val end = if (inclusive) rowIx.rsearch(to) else rowIx.lsearch(to)
    Frame(values.map(v => v.slice(start, end)), rowIx.slice(start, end), colIx)
  }

  // -----------------------------------------
  // access rows by particular location(s)

  /**
    * Access frame row at a particular integer offset
    * @param loc integer offset
    */
  def rowAt(loc: Int): Series[CX, T] = Series(values.rowAt(loc), colIx)

  /**
    * Access frame rows at a particular integer offsets
    * @param locs a sequence of integer offsets
    */
  def rowAt(locs: Int*): Frame[RX, CX, T] = rowAt(locs.toArray)

  /**
    * Access frame rows at a particular integer offsets
    * @param locs an array of integer offsets
    */
  def rowAt(locs: Array[Int]): Frame[RX, CX, T] =
    Frame(values.map(v => v.take(locs)), rowIx.take(locs), colIx)

  /**
    * Access frame rows specified by a slice
    * @param slice a slice specifier
    */
  def rowAt(slice: Slice[Int]): Frame[RX, CX, T] = {
    val idx = IndexIntRange(numRows)
    val pair = slice(idx)
    Frame(
      values.map(_.slice(pair._1, pair._2)),
      rowIx.slice(pair._1, pair._2),
      colIx
    )
  }

  /**
    * Access frame rows between two integer offsets, [from, until)
    * @param from Beginning offset
    * @param until One past ending offset
    * @param stride Optional increment between offsets
    */
  def rowSlice(from: Int, until: Int, stride: Int = 1): Frame[RX, CX, T] = {
    Frame(
      values.map(v => v.slice(from, until, stride)),
      rowIx.slice(from, until, stride),
      colIx
    )
  }

  /**
    * Split Frame into two frames at row position r
    * @param r Position at which to split Frame
    */
  def rowSplitAt(r: Int): (Frame[RX, CX, T], Frame[RX, CX, T]) =
    (rowSlice(0, r), rowSlice(r, numRows))

  /**
    * Split Frame into two frames at row key k
    * @param k Key at which to split Frame
    */
  def rowSplitBy(k: RX): (Frame[RX, CX, T], Frame[RX, CX, T]) =
    rowSplitAt(rowIx.lsearch(k))

  // --------------------------------------------
  // access a two dimensional sub-block by key(s)

  /**
    * Slice frame by row and column slice specifiers
    * @param rix A row slice
    * @param cix A col slice
    */
  def apply(rix: Slice[RX], cix: Slice[CX]): Frame[RX, CX, T] =
    col(cix).row(rix)

  /**
    * Slice frame by row slice and array of column keys
    * @param rix A row slice
    * @param cix An array of column keys
    */
  def apply(rix: Slice[RX], cix: Array[CX]): Frame[RX, CX, T] =
    col(cix).row(rix)

  /**
    * Slice frame by array of row keys and a col slice
    * @param rix An array of row keys
    * @param cix A col slice
    */
  def apply(rix: Array[RX], cix: Slice[CX]): Frame[RX, CX, T] =
    col(cix).row(rix)

  /**
    * Slice from by an array of row keys and an array of col keys
    * @param rix An array of row keys
    * @param cix An array of col keys
    */
  def apply(rix: Array[RX], cix: Array[CX]): Frame[RX, CX, T] =
    col(cix).row(rix)

  // -----------------------------------------
  // access grid by particular location(s)

  /**
    * Access a (Scalar-boxed) value from within the Frame
    * @param r Integer row offset
    * @param c Integer col offset
    */
  def at(r: Int, c: Int): Scalar[T] = values.at(r, c)

  /**
    * Access a slice of the Frame by integer offsets
    * @param r Array of row offsets
    * @param c Array of col offsets
    */
  def at(r: Array[Int], c: Array[Int]): Frame[RX, CX, T] = rowAt(r).colAt(c)

  /**
    * Access a slice of the Frame by integer offsets
    * @param r Array of row offsets
    * @param c Integer col offset
    */
  def at(r: Array[Int], c: Int): Series[RX, T] = rowAt(r).colAt(c)

  /**
    * Access a slice of the Frame by integer offsets
    * @param r Integer row offset
    * @param c Array of col offsets
    */
  def at(r: Int, c: Array[Int]): Series[CX, T] = colAt(c).rowAt(r)

  /**
    * Access a slice of the Frame by Slice parameters
    * @param r Slice to apply to rows
    * @param c Slice to apply to cols
    */
  def at(r: Slice[Int], c: Slice[Int]): Frame[RX, CX, T] = rowAt(r).colAt(c)

  /**
    * Access the raw (unboxed) value at an offset within the Frame
    * @param r Integer row offset
    * @param c Integer col offset
    */
  def raw(r: Int, c: Int): T = values(r, c)

  // -----------------------------------------
  // re-index frame; non-existent keys map to NA

  /**
    * Create a new Frame whose indexes are formed from the provided arguments, and whose values
    * are derived from the original Frame. Keys in the provided indices which do not map to
    * existing values will map to NA in the new Frame.
    * @param rix Sequence of keys to be the row index of the result Frame
    * @param cix Sequence of keys to be the col index of the result Frame
    */
  def reindex(rix: Index[RX], cix: Index[CX]): Frame[RX, CX, T] =
    reindexRow(rix).reindexCol(cix)

  /**
    * Create a new Frame whose row index is formed of the provided argument, and whose values
    * are derived from the original Frame.
    * @param rix Sequence of keys to be the row index of the result Frame
    */
  def reindexRow(rix: Index[RX]): Frame[RX, CX, T] = {
    val ixer = rowIx.getIndexer(rix)
    ixer.map { i =>
      Frame(
        values.map(v => Vec(array.take(v.toArray, i, v.scalarTag.missing))),
        rix,
        colIx
      )
    } getOrElse this
  }

  /**
    * Create a new Frame whose col index is formed of the provided argument, and whose values
    * are derived from the original Frame.
    * @param cix Sequence of keys to be the col index of the result Frame
    */
  def reindexCol(cix: Index[CX]): Frame[RX, CX, T] = {
    val ixer = colIx.getIndexer(cix)
    ixer.map { i =>
      Frame(values.take(i), rowIx, cix)
    } getOrElse this
  }

  // -----------------------------------------
  // access columns by type

  /**
    * Extract columns from a heterogeneous Frame which match the provided type.
    * The result is a homogeneous frame consisting of the selected data.
    * @tparam U The type of columns to extract
    */
  def colType[U: ST]: Frame[RX, CX, U] = {
    val (columns, locs) = values.takeType[U]
    Frame(columns, rowIx, colIx.take(locs))
  }

  /**
    * Extract columns from a heterogeneous Frame which match either of the provided
    * types. The result is a heterogeneous frame consisting of the selected data.
    * @tparam U1 First type of columns to extract
    * @tparam U2 Second type of columns to extract
    */
  def colType[U1: ST, U2: ST]: Frame[RX, CX, Any] = {
    val (columns1, locs1) = values.takeType[U1]
    val (columns2, locs2) = values.takeType[U2]

    val frm = Panel(
      columns1 ++ columns2,
      rowIx,
      colIx.take(locs1) concat colIx.take(locs2)
    )
    val tkr = array.argsort(array.flatten(Seq(locs1, locs2)))

    frm.colAt(tkr)
  }

  // ----------------------------------------
  // generate or use a new index

  /**
    * Create a new Frame using the current values but with the new row index. Positions
    * of the values do not change. Length of new index must be equal to number of rows.
    * @param newIx A new Index
    * @tparam Y Type of elements of new Index
    */
  def setRowIndex[Y: ST: ORD](newIx: Index[Y]): Frame[Y, CX, T] =
    new Frame(values, newIx, colIx, cachedMat)

  /**
    * Create a new Frame using the current values but with the new row index specified
    * by the column at a particular offset, and with that column removed from the frame
    * data body.
    */
  def withRowIndex(col: Int)(implicit ordT: ORD[T]): Frame[T, CX, T] =
    this.setRowIndex(Index(this.colAt(col).toVec)).filterAt(_ != col)

  /**
    * Overloaded method to create hierarchical index from two cols.
    */
  def withRowIndex(col1: Int, col2: Int)(
      implicit ordT: ORD[T]
  ): Frame[(T, T), CX, T] = {
    val newIx: Index[(T, T)] =
      Index.make((this.colAt(col1).toVec, this.colAt(col2).toVec))
    this.setRowIndex(newIx).filterAt { case c => !Set(col1, col2).contains(c) }
  }

  /**
    * Map a function over the row index, resulting in a new Frame
    *
    * @param fn The function RX => Y with which to map
    * @tparam Y Result type of index, ie Index[Y]
    */
  def mapRowIndex[Y: ST: ORD](fn: RX => Y): Frame[Y, CX, T] =
    new Frame(values, rowIx.map(fn), colIx, cachedMat)

  /**
    * Map a function over the rows, resulting in a new Frame
    *
    * @param fn The function (RX,Vec[T]) => Vec[Y] with which to map
    * @tparam Y Result type of mapped value
    */
  def mapRows[Y: ST](fn: (RX, Vec[T]) => Vec[Y]): Frame[RX, CX, Y] =
    toRowSeq
      .map {
        case (rix, row) =>
          val mappedVec = fn(rix, row.toVec)
          (rix, Series(row.index, mappedVec))
      }
      .toFrame
      .T

  /**
    * Map a function over the columns, resulting in a new Frame
    *
    * @param fn The function (CX,Vec[T]) => Vec[Y] with which to map
    * @tparam Y Result type of mapped value
    */
  def mapCols[Y: ST](fn: (CX, Vec[T]) => Vec[Y]): Frame[RX, CX, Y] =
    toColSeq.map {
      case (cix, col) =>
        val mappedVec = fn(cix, col.toVec)
        (cix, Series(col.index, mappedVec))
    }.toFrame

  /**
    * Create a new Frame using the current values but with the new col index. Positions
    * of the values do not change. Length of new index must be equal to number of cols.
    * @param newIx A new Index
    * @tparam Y Type of elements of new Index
    */
  def setColIndex[Y: ST: ORD](newIx: Index[Y]): Frame[RX, Y, T] =
    new Frame(values, rowIx, newIx, cachedMat)

  /**
    * Create a new Frame using the current values but with the new col index specified
    * by the row at a particular offset, and with that row removed from the frame
    * data body.
    */
  def withColIndex(row: Int)(implicit ordT: ORD[T]): Frame[RX, T, T] =
    this.setColIndex(Index(this.rowAt(row).toVec)).rfilterAt(_ != row)

  /**
    * Overloaded method to create hierarchical index from two rows.
    */
  def withColIndex(row1: Int, row2: Int)(
      implicit ordT: ORD[T]
  ): Frame[RX, (T, T), T] = {
    val newIx: Index[(T, T)] =
      Index.make((this.rowAt(row1).toVec, this.rowAt(row2).toVec))
    this.setColIndex(newIx).rfilterAt { case r => !Set(row1, row2).contains(r) }
  }

  /**
    * Map a function over the col index, resulting in a new Frame
    *
    * @param fn The function CX => Y with which to map
    * @tparam Y Result type of index, ie Index[Y]
    */
  def mapColIndex[Y: ST: ORD](fn: CX => Y): Frame[RX, Y, T] =
    new Frame(values, rowIx, colIx.map(fn), cachedMat)

  /**
    * Create a new Frame whose values are the same, but whose row index has been changed
    * to the bound [0, numRows - 1), as in an array.
    */
  def resetRowIndex: Frame[Int, CX, T] =
    new Frame(values, IndexIntRange(numRows), colIx, cachedMat)

  /**
    * Create a new Frame whose values are the same, but whose col index has been changed
    * to the bound [0, numCols - 1), as in an array.
    */
  def resetColIndex: Frame[RX, Int, T] =
    new Frame(values, rowIx, IndexIntRange(numCols), cachedMat)

  // ----------------------------------------
  // some helpful ops

  /**
    * Extract first n rows
    *
    * @param n number of rows to extract
    */
  def head(n: Int): Frame[RX, CX, T] = transform(_.head(n))

  /**
    * Extract last n rows
    *
    * @param n number of rows to extract
    */
  def tail(n: Int): Frame[RX, CX, T] = transform(_.tail(n))

  /**
    * Extract first n columns
    *
    * @param n number of columns to extract
    */
  def headCol(n: Int) = Frame(values.take(n), rowIx, colIx.head(n))

  /**
    * Extract last n columns
    *
    * @param n number of columns to extract
    */
  def tailCol(n: Int) = Frame(values.takeRight(n), rowIx, colIx.tail(n))

  /**
    * Extract first row matching a particular key
    *
    * @param k Key to match
    */
  def first(k: RX): Series[CX, T] = {
    val loc = rowIx.getFirst(k)
    if (loc == -1) emptyRow else rowAt(loc)
  }

  /**
    * Extract last row matching a particular key
    *
    * @param k Key to match
    */
  def last(k: RX): Series[CX, T] = {
    val loc = rowIx.getLast(k)
    if (loc == -1) Series.empty[CX, T] else rowAt(loc)
  }

  /**
    * Extract first col matching a particular key
    *
    * @param k Key to match
    */
  def firstCol(k: CX): Series[RX, T] = {
    val loc = colIx.getFirst(k)
    if (loc == -1) emptyCol else colAt(loc)
  }

  /**
    * Extract first col matching a particular key
    *
    * @param k Key to match
    */
  def lastCol(k: CX): Series[RX, T] = {
    val loc = colIx.getLast(k)
    if (loc == -1) emptyCol else colAt(loc)
  }

  /**
    * Return empty series of type equivalent to a row of frame
    *
    */
  def emptyRow: Series[CX, T] = Series.empty[CX, T]

  /**
    * Return empty series of type equivalent to a column of frame
    *
    */
  def emptyCol: Series[RX, T] = Series.empty[RX, T]

  /**
    * Create a new Frame whose rows are sorted according to the row
    * index keys
    */
  def sortedRIx: Frame[RX, CX, T] =
    if (rowIx.isMonotonic) this
    else {
      val taker = rowIx.argSort
      Frame(values.map(_.take(taker)), rowIx.take(taker), colIx)
    }

  /**
    * Create a new Frame whose cols are sorted according to the col
    * index keys
    */
  def sortedCIx: Frame[RX, CX, T] =
    if (colIx.isMonotonic) this
    else {
      val taker = colIx.argSort
      Frame(values.take(taker), rowIx, colIx.take(taker))
    }

  /**
    * Create a new Frame whose rows are sorted primarily on the values
    * in the first column specified in the argument list, and then on
    * the values in the next column, etc.
    * @param locs Location of columns containing values to sort on
    */
  def sortedRows(locs: Int*)(implicit ev: ORD[T]) = {
    var order = array.range(0, numRows)

    var j = locs.length - 1
    while (j >= 0) {
      val tosort = colAt(locs(j)).values.take(order)
      val reordr = Index(tosort).argSort
      order = array.take(order, reordr, sys.error("Logic error"))
      j -= 1
    }

    Frame(values.map(_.take(order)), rowIx.take(order), colIx)
  }

  /**
    * Create a new Frame whose cols are sorted primarily on the values
    * in the first row specified in the argument list, and then on
    * the values in the next row, etc.
    * @param locs Location of rows containing values to sort on
    */
  def sortedCols(locs: Int*)(implicit ev: ORD[T]) = {
    var order = array.range(0, numCols)

    var j = locs.length - 1
    while (j >= 0) {
      val tosort = rowAt(locs(j)).values.take(order)
      val reordr = Index(tosort).argSort
      order = array.take(order, reordr, sys.error("Logic error"))
      j -= 1
    }

    Frame(values.take(order), rowIx, colIx.take(order))
  }

  /**
    * Create a new Frame whose rows are sorted by the result of a function
    * acting on each row.
    * @param f Function from a single row (represented as series) to a value having an
    *          ordering
    * @tparam Q Result type of the function
    */
  def sortedRowsBy[Q: ORD](f: Series[CX, T] => Q): Frame[RX, CX, T] = {
    val perm = array.range(0, numRows).sortBy((i: Int) => f(rowAt(i)))
    rowAt(perm)
  }

  /**
    * Create a new Frame whose cols are sorted by the result of a function
    * acting on each col.
    * @param f Function from a single col (represented as series) to a value having an
    *          ordering
    * @tparam Q Result type of the function
    */
  def sortedColsBy[Q: ORD](f: Series[RX, T] => Q): Frame[RX, CX, T] = {
    val perm = array.range(0, numCols).sortBy((i: Int) => f(colAt(i)))
    colAt(perm)
  }

  /**
    * Map over each triple (r, c, v) in the Frame, returning a new frame from the resulting
    * triples.
    */
  def map[SX: ST: ORD, DX: ST: ORD, U: ST](
      f: ((RX, CX, T)) => (SX, DX, U)
  ): Frame[SX, DX, U] = {
    Series(toSeq.map(f).map { case (sx, dx, u) => ((sx, dx) -> u) }: _*).pivot
  }

  /**
    * Map over each triple (r, c, v) in the Frame, flattening results, and returning a new frame from
    * the resulting triples.
    */
  def flatMap[SX: ST: ORD, DX: ST: ORD, U: ST](
      f: ((RX, CX, T)) => Traversable[(SX, DX, U)]
  ): Frame[SX, DX, U] = {
    Series(toSeq.flatMap(f).map { case (sx, dx, u) => ((sx, dx) -> u) }: _*).pivot
  }

  /**
    * Map over the values of the Frame. Applies a function to each (non-na) value in the frame,
    * returning a new frame whose indices remain the same.
    *
    * @param f Function from T to U
    * @tparam U The type of the resulting values
    */
  def mapValues[U: ST](f: T => U): Frame[RX, CX, U] =
    Frame(values.map(v => v.map(f)), rowIx, colIx)

  /**
    * Create a new Frame that, whenever the mask predicate function evaluates to
    * true on a value, is masked with NA
    * @param f Function from T to Boolean
    */
  def mask(f: T => Boolean): Frame[RX, CX, T] =
    Frame(values.map(v => v.mask(f)), rowIx, colIx)

  /**
    * Create a new Frame whose columns follow the rule that, wherever the mask Vec is true,
    * the column value is masked with NA
    * @param m Mask Vec[Boolean]
    */
  def mask(m: Vec[Boolean]): Frame[RX, CX, T] =
    Frame(values.map(v => v.mask(m)), rowIx, colIx)

  /**
    * Joins two frames along both their indexes and applies a function to each pair
    * of values; when either value is NA, the result of the function is forced to be NA.
    * @param other Other Frame
    * @param rhow The type of join to effect on the rows
    * @param chow The type of join to effect on the cols
    * @param f The function to apply
    * @tparam U The type of other frame values
    * @tparam V The result type of the function
    */
  def joinMap[U: ST, V: ST](
      other: Frame[RX, CX, U],
      rhow: JoinType = LeftJoin,
      chow: JoinType = RightJoin
  )(f: (T, U) => V): Frame[RX, CX, V] = {
    val (l, r) = align(other, rhow, chow)
    val result =
      l.values.zip(r.values).map { case (v1, v2) => VecImpl.zipMap(v1, v2)(f) }
    Frame(result, l.rowIx, l.colIx)
  }

  /**
    * Map a function over each column vector and collect the results into a Frame respecting
    * the original indexes.
    * @param f Function acting on Vec[T] and producing another Vec
    * @tparam U Type of result Vec of the function
    */
  def mapVec[U: ST](f: Vec[T] => Vec[U]): Frame[RX, CX, U] =
    Frame(values.map(f), rowIx, colIx)

  /**
    * Apply a function to each column series which results in a single value, and return the
    * series of results indexed by original column index.
    * @param f Function taking a column (series) to a value
    * @tparam U The output type of the function
    */
  def reduce[U: ST](f: Series[RX, T] => U): Series[CX, U] =
    Series(Vec(values.map(v => f(Series(v, rowIx))): _*), colIx)

  /**
    * Apply a function to each column series which results in another series (having possibly
    * a different index); return new frame whose row index is the the full outer join of all
    * the intermediately produced series (fast when all series have the same index), and having
    * the original column index.
    * @param f Function to operate on each column as a series
    * @tparam U Type of values of result series of function
    * @tparam SX Type of index of result series of function
    */
  def transform[U: ST, SX: ST: ORD](
      f: Series[RX, T] => Series[SX, U]
  ): Frame[SX, CX, U] =
    Frame(values.map(v => f(Series(v, rowIx))), colIx)

  // groupBy functionality (on rows)

  /**
    * Construct a [[org.saddle.groupby.FrameGrouper]] with which further computations, such
    * as combine or transform, may be performed. The groups are constructed from the keys of
    * the row index, with each unique key corresponding to a group.
    */
  def groupBy = FrameGrouper(this)

  /**
    * Construct a [[org.saddle.groupby.FrameGrouper]] with which further computations, such
    * as combine or transform, may be performed. The groups are constructed from the result
    * of the function applied to the keys of the row index; each unique result of calling the
    * function on elements of the row index corresponds to a group.
    * @param fn Function from RX => Y
    * @tparam Y Type of function codomain
    */
  def groupBy[Y: ST: ORD](fn: RX => Y) = FrameGrouper(this.rowIx.map(fn), this)

  /**
    * Construct a [[org.saddle.groupby.FrameGrouper]] with which further computations, such
    * as combine or transform, may be performed. The groups are constructed from the keys of
    * the provided index, with each unique key corresponding to a group.
    * @param ix Index with which to perform grouping
    * @tparam Y Type of elements of ix
    */
  def groupBy[Y: ST: ORD](ix: Index[Y]) = FrameGrouper(ix, this)

  // concatenate two frames together (vertically), must have same number of columns

  /**
    * Concatenate the Frame instances together (vertically, i.e. concatenate as lists of rows)
    * whose indexes share the same type
    * of elements, and where there exists some way to join the values of the Frames. For
    * instance, Frame[X, Y, Double] `concat` Frame[X, Y, Int] will promote Int to Double as
    * a result of the implicit existence of a Promoter[Double, Int, Double] instance.
    * The resulting row index will simply be the concatenation of the input row indexes, and
    * the column index will be the joint index (with join type specified as argument).
    *
    * A1 A2  concat B1 B2  =  A1 A2
    * A3 A4         B3 B4     A3 A4
    *                         B1 B2
    *                         B3 B4
    *
    * @param other  Frame[RX, CX, U] to concat
    * @param pro Implicit evidence of Promoter
    * @tparam U type of other Frame values
    * @tparam V type of resulting Frame values
    */
  def concat(
      other: Frame[RX, CX, T],
      how: JoinType = OuterJoin
  ): Frame[RX, CX, T] = {

    val reindexer = colIx.join(other.colIx, how)

    val left = reindexer.lTake.map(x => values.take(x)) getOrElse values
    val right = reindexer.rTake.map(x => other.values.take(x)) getOrElse other.values

    val data = (left zip right).map { case (top, bot) => top concat bot }
    val rowIdx = rowIx concat other.rowIx

    Frame(data, rowIdx, reindexer.index)
  }

  /**
    * Create Frame whose rows satisfy the rule that their keys and values are chosen
    * via a Vec[Boolean] or a Series[_, Boolean] predicate when the latter contains a
    * true value.
    * @param pred Series[_, Boolean] (or Vec[Boolean] which will implicitly convert)
    */
  def where(pred: Series[_, Boolean]): Frame[RX, CX, T] =
    where(pred.values)

  /**
    * Create Frame whose rows satisfy the rule that their keys and values are chosen
    * via a Vec[Boolean] or a Series[_, Boolean] predicate when the latter contains a
    * true value.
    * @param pred Series[_, Boolean] (or Vec[Boolean] which will implicitly convert)
    */
  def where(pred: Vec[Boolean]): Frame[RX, CX, T] = {
    val newVals = values.zipWithIndex.flatMap(z =>
      if (pred.raw(z._2)) Seq(z._1) else Seq.empty[Vec[T]]
    )
    val newIdx = VecImpl.where(Vec(this.colIx.toArray))(pred.toArray)
    Frame(newVals, rowIx, Index(newIdx))
  }

  /**
    * Shift the sequence of values relative to the row index by some offset,
    * dropping those values which no longer associate with a key, and having
    * those keys which no longer associate to a value instead map to NA.
    * @param n Number to shift
    */
  def shift(n: Int = 1): Frame[RX, CX, T] =
    Frame(values.map(_.shift(n)), rowIx, colIx)

  /**
    * Return Frame whose columns satisfy a predicate function operating on that
    * column
    * @param pred Predicate function from Series[RX, T] => Boolean
    */
  def filter(pred: Series[RX, T] => Boolean) = where(reduce(v => pred(v)))

  /**
    * Return Frame whose columns satisfy a predicate function operating on the
    * column index
    * @param pred Predicate function from CX => Boolean
    */
  def filterIx(pred: CX => Boolean) = where(colIx.toVec.map(pred))

  /**
    * Return Frame whose columns satisfy a predicate function operating on the
    * column index offset
    * @param pred Predicate function from CX => Boolean
    */
  def filterAt(pred: Int => Boolean) = where(vec.range(0, numCols).map(pred))

  /**
    * Return Frame excluding any of those columns which have an NA value
    */
  def dropNA: Frame[RX, CX, T] = filter(s => !s.hasNA)

  /**
    * Produce a Frame each of whose columns are the result of executing a function
    * on a sliding window of each column series.
    * @param winSz Window size
    * @param f Function Series[X, T] => B to operate on sliding window
    * @tparam B Result type of function
    */
  def rolling[B: ST](
      windowSize: Int,
      f: Series[RX, T] => B
  ): Frame[RX, CX, B] = {
    val tmp = values.map { v =>
      Series(v, rowIx).rolling(windowSize, f).values
    }
    Frame(tmp, rowIx.slice(windowSize - 1, values.numRows), colIx)
  }

  /**
    * Create a Series by rolling over winSz number of rows of the Frame at a
    * time, and applying a function that takes those rows to a single value.
    *
    * @param winSz Window size to roll with
    * @param f Function taking the (sub) frame to B
    * @tparam B Result element type of Series
    */
  def rollingFtoS[B: ST](
      windowSize: Int,
      f: Frame[RX, CX, T] => B
  ): Series[RX, B] = {
    val buf = new Array[B](numRows - windowSize + 1)
    var i = windowSize
    while (i <= numRows) {
      buf(i - windowSize) = f(rowSlice(i - windowSize, i))
      i += 1
    }
    Series(Vec(buf), rowIx.slice(windowSize - 1, numRows))
  }

  // ----------------------------------------
  // joining

  /**
    * Same as `addCol`, but preserve the column index, adding the specified index value,
    * `newColIx` as an index for the `other` Series.
    */
  def addCol(
      other: Series[RX, T],
      newColIx: CX,
      how: JoinType = OuterJoin
  ): Frame[RX, CX, T] = {
    val resultingFrame = addCol(other, how)
    val newColIndex = colIx.concat(Index(newColIx))
    resultingFrame.setColIndex(newColIndex)
  }

  /**
    * Add a new column. Resets column index
    *
    * The result is a Frame whose row index is the result of the join, and whose column
    * index has been reset to [0, numcols], and whose values are sourced from the original
    * Frame and Series.
    *
    * @param other Series to join with
    * @param how How to perform the join
    */
  def addCol(
      other: Series[RX, T],
      how: JoinType
  ): Frame[RX, Int, T] = {
    val indexer = rowIx.join(other.index, how)
    val lft = indexer.lTake.map { loc =>
      values.map(_.take(loc))
    } getOrElse values
    val rgt = indexer.rTake.map { loc =>
      other.values.take(loc)
    } getOrElse other.values
    Frame(lft :+ rgt, indexer.index, IndexIntRange(colIx.length + 1))
  }

  /**
    * Aligns this frame with another frame, returning the left and right frames aligned
    * to each others indexes according to the the provided parameters
    *
    * @param other Other frame to align with
    * @param rhow How to perform the join on the row indexes
    * @param chow How to perform the join on the col indexes
    */
  def align[U: ST](
      other: Frame[RX, CX, U],
      rhow: JoinType = OuterJoin,
      chow: JoinType = OuterJoin
  ): (Frame[RX, CX, T], Frame[RX, CX, U]) = {
    val rJoin = rowIx.join(other.rowIx, rhow)
    val cJoin = colIx.join(other.colIx, chow)

    val lvals: MatCols[T] =
      cJoin.lTake.map(locs => values.take(locs)).getOrElse(values)
    val rvals: MatCols[U] =
      cJoin.rTake.map(locs => other.values.take(locs)).getOrElse(other.values)

    val vecs = for (i <- 0 until lvals.length) yield {
      val lvec: Vec[T] =
        rJoin.lTake.map(locs => lvals(i).take(locs)).getOrElse(lvals(i))
      val rvec: Vec[U] =
        rJoin.rTake.map(locs => rvals(i).take(locs)).getOrElse(rvals(i))
      (lvec, rvec)
    }

    val (lvecs, rvecs) = vecs.unzip

    (
      Frame(lvecs, rJoin.index, cJoin.index),
      Frame(rvecs, rJoin.index, cJoin.index)
    )
  }

  // ------------------------------------------------
  // reshaping

  /**
    * Drop all columns from the Frame which have nothing but NA values.
    */
  def squeeze: Frame[RX, CX, T] = filter(s => !VecImpl.isAllNA(s.toVec))

  /**
    * Melt stacks the row index of arity N with the column index of arity M to form a result index
    * of arity N + M, producing a 1D Series whose values are from the original Frame as indexed by
    * the corresponding keys.
    *
    * For example, given:
    *
    * {{{
    *   Frame(1 -> Series('a' -> 1, 'b' -> 3), 2 -> Series('a' -> 2, 'b' -> 4)).melt
    * }}}
    *
    * produces:
    *
    * {{{
    * res0: org.saddle.Series[(Char, Int),Int] =
    * [4 x 1]
    *  a 1 => 1
    *    2 => 2
    *  b 1 => 3
    *    2 => 4
    * }}}
    *
    *
    * @param melter Implicit evidence for a Melter for the two indexes
    * @tparam W Output type (tuple of arity N + M)
    */
  def melt[W](implicit melter: Melter[RX, CX, W]): Series[W, T] = {
    val ix = Array.ofDim[W](numRows * numCols)(melter.tag)

    var k = 0
    var i = 0
    while (i < numRows) {
      var j = 0
      while (j < numCols) {
        ix(k) = melter(rowIx.raw(i), colIx.raw(j))
        k += 1
        j += 1
      }
      i += 1
    }

    implicit val ord = melter.ord
    implicit val tag = melter.tag

    Series[W, T](toMat.toVec, Index(ix))
  }

  /**
    * Stack pivots the innermost column labels to the innermost row labels. That is, it splits
    * a col index of tuple keys of arity N into a new col index having arity N-1 and a remaining
    * index C, and forms a new row index by stacking the existing row index with C. The
    * resulting Frame has values as in the original Frame indexed by the corresponding keys. It
    * does the reverse of unstack.
    *
    * @param splt An implicit instance of Splitter to do the splitting
    * @param stkr An implicit instance of Stacker to do the stacking
    * @tparam O1 The N-1 arity column index type
    * @tparam O2 The 1-arity type of split-out index C
    * @tparam V The type of the stacked row index
    */
  def stack[O1, O2, V](
      implicit splt: Splitter[CX, O1, O2],
      stkr: Stacker[RX, O2, V],
      ord1: ORD[O1],
      ord2: ORD[O2],
      m1: ST[O1],
      m2: ST[O2]
  ): Frame[V, O1, T] = {
    T.unstack.T
  }

  /**
    * Unstack pivots the innermost row labels to the innermost col labels. That is, it splits
    * a row index of tuple keys of arity N into a new row index having arity N-1 and a remaining
    * index R, and forms a new col index by stacking the existing col index with R. The
    * resulting Frame has values as in the original Frame indexed by the corresponding keys.
    *
    * For example:
    *
    * {{{
    * scala> Frame(Series(Vec(1,2,3,4), Index(('a',1),('a',2),('b',1),('b',2))), Series(Vec(5,6,7,8), Index(('a',1),('a',2),('b',1),('b',2))))
    * res1: org.saddle.Frame[(Char, Int),Int,Int] =
    * [4 x 2]
    *         0  1
    *        -- --
    * a 1 ->  1  5
    *   2 ->  2  6
    * b 1 ->  3  7
    *   2 ->  4  8
    *
    * scala> res1.unstack
    * res2: org.saddle.Frame[Char,(Int, Int),Int] =
    * [2 x 4]
    *       0     1
    *       1  2  1  2
    *      -- -- -- --
    * a ->  1  2  5  6
    * b ->  3  4  7  8
    * }}}
    *
    * @param splt An implicit instance of Splitter to do the splitting
    * @param stkr An implicit instance of Stacker to do the stacking
    * @tparam O1 The N-1 arity row index type
    * @tparam O2 The 1-arity type of split-out index R
    * @tparam V The type of the stacked col index
    */
  def unstack[O1, O2, V](
      implicit splt: Splitter[RX, O1, O2],
      stkr: Stacker[CX, O2, V],
      ord1: ORD[O1],
      ord2: ORD[O2],
      m1: ST[O1],
      m2: ST[O2]
  ): Frame[O1, V, T] = {
    implicit def ordV = stkr.ord
    implicit def clmV = stkr.tag

    val (lft, rgt) = splt(rowIx) // lft = row index w/o pivot level; rgt = pivot level

    val rix = lft.uniques // Final row index
    val uix = rgt.uniques
    val cix = stkr(colIx, uix) // Final col index (colIx stacked w/unique pivot labels)

    val grps = IndexGrouper(rgt, sorted = false).groups // Group by pivot label. Each unique label will get its
    //   own column in the final frame.
    if (values.length > 0) {
      val len = uix.length
      var off = 0
      var loc = 0

      val result = Array.ofDim[Vec[T]](cix.length) // accumulates result columns

      for ((_, taker) <- grps) { // For each pivot label grouping,
        val gIdx = lft.take(taker) //   use group's (lft) row index labels
        val ixer = rix.join(gIdx) //   to compute map to final (rix) locations;

        for (currVec <- values) { // For each column vec of original frame
          val vals = currVec.take(taker) //   take values corresponding to current pivot label
          val v = ixer.rTake.map(vals.take(_)).getOrElse(vals) //   map values to be in correspondence to rix
          result(loc) = v //   and save vec in array.

          loc += len // Increment offset into result array
          if (loc >= cix.length) {
            off += 1; loc = off
          }
        }
      }

      Frame[O1, V, T](result, rix, cix)
    } else Frame.empty[O1, V, T]
  }

  /**
    * Extract the Mat embodied in the values of the Frame (dropping any indexing
    * information)
    */
  def toMat: Mat[T] = {
    synchronized {
      if (cachedMat.isEmpty) {
        val m = Mat(values.numCols, values.numRows, st.concat(values)).T
        cachedMat = Some(m)
      }
      cachedMat.get
    }
  }

  // ---------------------------------------------------------------
  // Row-wise versions of all the ops that operate on cols by default

  /**
    * See mask; operates row-wise
    */
  def rmask(b: Vec[Boolean]): Frame[RX, CX, T] = {
    val missing = {
      val arr = array.empty[T](numRows)
      array.fill(arr, st.missing)
      Vec(arr)
    }
    Frame(values.zipWithIndex.map {
      case (v, idx) =>
        if (b.raw(idx)) missing else v
    }, rowIx, colIx)
  }

  /**
    * See mapVec; operates row-wise
    */
  def rmapVec[U: ST](f: Vec[T] => Vec[U]): Frame[RX, CX, U] = {
    val vecs = 0 until numRows map { i =>
      f(values.rowAt(i))
    }

    Frame(vecs, colIx, rowIx).T
  }

  /**
    * See reduce; operates row-wise
    */
  def rreduce[U: ST](f: Series[CX, T] => U): Series[RX, U] = {
    val vec = 0 until numRows map { i =>
      f(Series(values.rowAt(i), colIx))
    } toVec

    Series(vec, rowIx)
  }

  /**
    * See transform; operates row-wise
    */
  def rtransform[U: ST, SX: ST: ORD](
      f: Series[CX, T] => Series[SX, U]
  ): Frame[RX, SX, U] = {
    val transformed = 0 until numRows map { i =>
      f(Series(values.rowAt(i), colIx))
    }

    Frame(transformed, rowIx).T
  }

  /**
    * See concat; operates row-wise.
    * Concetanates two Frames by concatenating their lists of columns
    * A1 A2 rconcat B1 B2  =  A1 A2 B1 B2
    * A3 A4         B3 B4     A3 A4 B3 B4
    */
  def rconcat(
      other: Frame[RX, CX, T],
      how: JoinType = OuterJoin
  ): Frame[RX, CX, T] = {
    val reindexer = rowIx.join(other.rowIx, how)

    val left = reindexer.lTake.map(x => values.takeRows(x)) getOrElse values
    val right = reindexer.rTake.map(x => other.values.takeRows(x)) getOrElse other.values

    val data = left ++ right
    val colIdx = colIx concat other.colIx

    Frame(data, reindexer.index, colIdx)
  }

  /**
    * Same as rconcat. Concatenates two Frames by concatenating their lists of columns
    * A1 A2 rconcat B1 B2  =  A1 A2 B1 B2
    * A3 A4         B3 B4     A3 A4 B3 B4
    */
  def cbind(
      other: Frame[RX, CX, T],
      how: JoinType = OuterJoin
  ): Frame[RX, CX, T] = rconcat(other, how)

  /**
    * Same as concat. Concatenates two Frames by concatenating their lists of rows
    * A1 A2  concat B1 B2  =  A1 A2
    * A3 A4         B3 B4     A3 A4
    *                         B1 B2
    *                         B3 B4
    */
  def rbind(
      other: Frame[RX, CX, T],
      how: JoinType = OuterJoin
  ): Frame[RX, CX, T] =
    this.concat(other, how)

  /** Return the frame with the first occurence of each column key.
    * Rows are not changed.
    */
  def distinct = {
    val newColIx = colIx.distinct
    col(newColIx.toArray)
  }

  /** Return the series with the first occurence of each row key.
    * Columns are not changed.
    */
  def rdistinct = {
    val newRowIx = rowIx.distinct
    row(newRowIx.toArray)
  }

  /**
    * See where; operates row-wise
    */
  def rwhere(pred: Series[_, Boolean]): Frame[RX, CX, T] =
    rwhere(pred.values)

  /**
    * See where; operates row-wise
    */
  def rwhere(pred: Vec[Boolean]): Frame[RX, CX, T] = {
    val predv = pred
    new Frame(
      new MatCols(values.map(v => v.where(predv))),
      Index(rowIx.toVec.where(predv)),
      colIx,
      None
    )
  }

  /**
    * See shift; operates col-wise
    */
  def cshift(n: Int = 1): Frame[RX, CX, T] = T.shift(n).T

  /**
    * See filter; operates row-wise
    */
  def rfilter(pred: Series[CX, T] => Boolean) = rwhere(rreduce(v => pred(v)))

  /**
    * See filterIx; operates row-wise
    */
  def rfilterIx(pred: RX => Boolean) = rwhere(rowIx.toVec.map(pred))

  /**
    * See filterAt; operates row-wise
    */
  def rfilterAt(pred: Int => Boolean) = rwhere(vec.range(0, numRows).map(pred))

  /**
    * See addCol, operates row-wise.
    */
  def addRow(
      other: Series[CX, T],
      newRowIx: RX,
      how: JoinType = OuterJoin
  ): Frame[RX, CX, T] = {
    val resultingFrame = addRow(other, how)
    val newRowIndex = rowIx.concat(Index(newRowIx))
    resultingFrame.setRowIndex(newRowIndex)
  }

  /**
    * See addRow; operates row-wise.
    */
  def addRow(
      other: Series[CX, T],
      how: JoinType
  ): Frame[Int, CX, T] = {
    val indexer = colIx.join(other.index, how)

    val rgt = indexer.rTake.map { loc =>
      other.values.take(loc)
    } getOrElse other.values
    val lft = indexer.lTake.map { loc =>
      values.take(loc)
    } getOrElse values
    Frame(lft.appendRow(rgt), IndexIntRange(rowIx.length + 1), indexer.index)
  }

  /**
    * See dropNA; operates row-wise
    */
  def rdropNA: Frame[RX, CX, T] = rfilter(v => !v.hasNA)

  /**
    * See squeeze; operates row-wise
    */
  def rsqueeze: Frame[RX, CX, T] = rfilter(s => !VecImpl.isAllNA(s.toVec))

  // --------------------------------------
  // for iterating over rows/cols/elements

  /**
    * Produce an indexed sequence of pairs of row index value and
    * row Series
    */
  def toRowSeq: IndexedSeq[(RX, Series[CX, T])] =
    for (i <- array.range(0, numRows)) yield (rowIx.raw(i), rowAt(i))

  /**
    * Produce an indexed sequence of pairs of column index value and
    * column Series.
    */
  def toColSeq: IndexedSeq[(CX, Series[RX, T])] =
    for (i <- array.range(0, numCols)) yield (colIx.raw(i), colAt(i))

  /**
    * Produce an indexed sequence of triples of values in the Frame
    * in row-major order.
    */
  def toSeq: IndexedSeq[(RX, CX, T)] =
    (Range(0, numRows) zip rowIx.toSeq).flatMap {
      case (i, rx) =>
        rowAt(i).toSeq.map {
          case (cx, t) =>
            (rx, cx, t)
        }
    }

  // ------------------------------------------------------
  // internal contiguous caching of row data for efficiency

  // --------------------------------------
  // pretty-printing

  override def toString: String = stringify()

  /**
    * Creates a string representation of Frame
    * @param nrows Max number of rows to include
    * @param ncols Max number of rows to include
    */
  def stringify(nrows: Int = 10, ncols: Int = 10): String = {
    val buf = new StringBuilder()

    if (numCols == 0 || numRows == 0)
      buf.append("Empty Frame")
    else {
      buf.append("[%d x %d]\n".format(numRows, numCols))

      val rhalf = nrows / 2

      val maxf = (a: List[Int], b: List[String]) =>
        (a zip b).map(v => math.max(v._1, v._2.length))

      // calc row index width
      val rsca = rowIx.scalarTag
      val rarr = rowIx.toArray
      val rinit = rsca.strList(rarr(0)).map(_.length)
      val rlens =
        util.grab(rarr, rhalf).map(rsca.strList(_)).foldLeft(rinit)(maxf)
      val maxrl = rlens.sum + (rlens.length - 1)

      // calc each col str width
      val clens = MatCols.colLens(values, numCols, ncols)

      val csca = colIx.scalarTag
      def clen(c: Int) = clens(c) max {
        val lst = csca.strList(colIx.raw(c)).map(_.length)
        if (lst.length > 0) lst.max else 0
      }

      var prevColMask = clens.map(x => (x._1, false)) // recalls whether we printed a column's label at level L-1
      var prevColLabel = "" // recalls previous column's label at level L

      // build columns header
      def createColHeader(l: Int) = (c: Int) => {
        val labs = csca.strList(colIx.raw(c))
        val currLab = labs(l)

        val fmt = "%" + clen(c) + "s "
        val res =
          if (l == labs.length - 1 || currLab != prevColLabel || prevColMask
                .get(c)
                .getOrElse(false)) {
            prevColMask = prevColMask.updated(c, true)
            currLab.formatted(fmt)
          } else {
            prevColMask = prevColMask.updated(c, false)
            "".formatted(fmt)
          }
        prevColLabel = currLab
        res
      }

      def colBreakStr = {
        prevColLabel = ""
        " " * 5
      }

      val spacer = " " * (maxrl + 4)

      val sz = colIx.scalarTag.strList(colIx.raw(0)).size
      for (i <- 0 until sz) {
        buf.append(spacer)
        buf.append(
          util.buildStr(ncols, numCols, createColHeader(i), colBreakStr)
        )
        buf.append("\n")
      }

      def createColDivide(c: Int) = "-" * clen(c) + " "

      buf.append(spacer)
      buf.append(util.buildStr(ncols, numCols, createColDivide))
      buf.append("\n")

      // for building row labels
      def enumZip[A, B](a: List[A], b: List[B]): List[(Int, A, B)] =
        for (v <- (a.zipWithIndex zip b)) yield (v._1._2, v._1._1, v._2)

      val prevRowLabels =
        Array.fill(rowIx.scalarTag.strList(rowIx.raw(0)).size)("")
      def resetRowLabels(k: Int) = {
        for (i <- k until prevRowLabels.length) prevRowLabels(i) = ""
      }

      def createIx(r: Int) = {
        val vls = rsca.strList(rowIx.raw(r))
        val lst = for ((i, l, v) <- enumZip(rlens, vls)) yield {
          val fmt = "%" + l + "s"
          val res = if (i == vls.length - 1 || prevRowLabels(i) != v) {
            resetRowLabels(i + 1)
            v.formatted(fmt)
          } else "".formatted(fmt)
          prevRowLabels(i) = v
          res
        }
        lst.mkString(" ")
      }

      // for building frame entries
      def createVals(r: Int) = {
        val elem = (col: Int) =>
          "%" + clen(col) + "s " format values(col).scalarTag
            .show(values(r, col))
        util.buildStr(ncols, numCols, elem) + "\n"
      }

      def rowBreakStr = {
        resetRowLabels(0)
        "...\n"
      }

      // now build row strings
      buf.append(
        util.buildStr(
          nrows,
          numRows,
          (r: Int) => createIx(r) + " -> " + createVals(r),
          rowBreakStr
        )
      )
    }
    buf.toString()
  }

  /**
    * Pretty-printer for Frame, which simply outputs the result of stringify.
    * @param nrows Number of rows to display
    * @param ncols Number of cols to display
    */
  def print(
      nrows: Int = 10,
      ncols: Int = 10,
      stream: OutputStream = System.out
  ): Unit = {
    stream.write(stringify(nrows, ncols).getBytes)
  }

  override def hashCode(): Int =
    values.hashCode() * 31 * 31 + rowIx.hashCode() * 31 + colIx.hashCode()

  override def equals(other: Any): Boolean = other match {
    case f: Frame[_, _, _] =>
      (this eq f) || rowIx == f.rowIx && colIx == f.colIx && values == f.values
    case _ => false
  }

  /**
    * Sum of the elements of each column, ignoring NA values
    */
  def sum(implicit num: NUM[T]): Series[CX, T] = reduce(_.sum)

  /**
    * Count of the elements of each column, ignoring NA values
    */
  def count: Series[CX, Int] = reduce(_.count)

  /**
    * Min of the elements of each column, ignoring NA values
    */
  def min(implicit num: NUM[T]): Series[CX, T] =
    reduce(_.toVec.min.getOrElse(implicitly[ST[T]].missing))

  /**
    * Median of each column
    */
  def median(implicit num: NUM[T]): Series[CX, Double] =
    reduce(_.toVec.median)

  /**
    * Mean of each column
    */
  def mean(implicit num: NUM[T]): Series[CX, Double] =
    reduce(_.toVec.mean)

  /**
    * Max of the elements of each column, ignoring NA values
    */
  def max(implicit num: NUM[T]): Series[CX, T] =
    reduce(_.toVec.max.getOrElse(implicitly[ST[T]].missing))

  /**
    * Product of the elements of each column, ignoring NA values
    */
  def prod(implicit num: NUM[T]): Series[CX, T] = reduce(_.toVec.prod)
}

object Frame extends BinOpFrame {
  // --------------------------------

  // --------------------------------
  // instantiations

  /**
    * Factory method to create an empty Frame
    * @tparam RX Type of row keys
    * @tparam CX Type of col keys
    * @tparam T Type of values
    */
  def empty[RX: ST: ORD, CX: ST: ORD, @spec(Int, Long, Double) T: ST]
      : Frame[RX, CX, T] =
    new Frame[RX, CX, T](
      MatCols.empty[T],
      Index.empty[RX],
      Index.empty[CX],
      None
    )

  // --------------------------------
  // Construct using sequence of vectors

  /**
    * Factory method to create a Frame from a sequence of Vec objects
    */
  def apply[@spec(Int, Long, Double) T: ST](
      values: Vec[T]*
  ): Frame[Int, Int, T] =
    if (values.isEmpty) empty[Int, Int, T]
    else {
      val asIdxSeq = values.toIndexedSeq
      apply(
        asIdxSeq,
        IndexIntRange(asIdxSeq(0).length),
        IndexIntRange(asIdxSeq.length)
      )
    }

  /**
    * Factory method to create a Frame from a sequence of Vec objects,
    * a row index, and a column index.
    */
  def apply[RX: ST: ORD, CX: ST: ORD, @spec(Int, Long, Double) T: ST](
      values: Seq[Vec[T]],
      rowIx: Index[RX],
      colIx: Index[CX]
  ): Frame[RX, CX, T] =
    if (values.isEmpty) empty[RX, CX, T]
    else
      new Frame[RX, CX, T](MatCols[T](values: _*), rowIx, colIx, None)

  /**
    * Factory method to create a Frame from a sequence of Vec objects
    * and a column index.
    */
  def apply[CX: ST: ORD, @spec(Int, Long, Double) T: ST](
      values: Seq[Vec[T]],
      colIx: Index[CX]
  ): Frame[Int, CX, T] =
    if (values.isEmpty) empty[Int, CX, T]
    else {
      val asIdxSeq = values.toIndexedSeq
      apply(asIdxSeq, IndexIntRange(asIdxSeq(0).length), colIx)
    }

  /**
    * Factory method to create a Frame from tuples whose first element is
    * the column label and the second is a Vec of values.
    */
  def apply[CX: ST: ORD, @spec(Int, Long, Double) T: ST](
      values: (CX, Vec[T])*
  ): Frame[Int, CX, T] = {
    val asIdxSeq = values.map(_._2).toIndexedSeq
    val idx = Index(values.map(_._1).toArray)
    asIdxSeq.length match {
      case 0 => empty[Int, CX, T]
      case _ => Frame(asIdxSeq, IndexIntRange(asIdxSeq(0).length), idx)
    }
  }

  // --------------------------------
  // Construct using sequence of series

  // dummy type, extra implicit parameter allows us to disambiguate the following
  // overloaded apply method
  private type ID[T] = T => T

  /**
    * Factory method to create a Frame from a sequence of Series. The row labels
    * of the result are the outer join of the indexes of the series provided.
    */
  def apply[RX: ST: ORD, T: ST: ID](
      values: Series[RX, T]*
  ): Frame[RX, Int, T] = {
    val asIdxSeq = values.toIndexedSeq
    asIdxSeq.length match {
      case 0 => empty[RX, Int, T]
      case 1 =>
        Frame(asIdxSeq.map(_.values), asIdxSeq(0).index, IndexIntRange(1))
      case _ => {
        val init = Frame(
          IndexedSeq(asIdxSeq(0).values),
          asIdxSeq(0).index,
          Index(Array(0))
        )
        val temp = asIdxSeq.tail.foldLeft(init)(_.addCol(_, OuterJoin))
        Frame(temp.values, temp.rowIx, IndexIntRange(temp.numCols))
      }
    }
  }

  /**
    * Factory method to create a Frame from a sequence of series, also specifying
    * the column index to use. The row labels of the result are the outer join of
    * the indexes of the series provided.
    */
  def apply[RX: ST: ORD, CX: ST: ORD, T: ST](
      values: Seq[Series[RX, T]],
      colIx: Index[CX]
  ): Frame[RX, CX, T] = {
    val asIdxSeq = values.toIndexedSeq
    asIdxSeq.length match {
      case 0 => empty[RX, CX, T]
      case 1 => Frame(asIdxSeq.map(_.values), asIdxSeq(0).index, colIx)
      case _ => {
        val init = Frame(Seq(asIdxSeq(0).values), asIdxSeq(0).index, Index(0))
        val temp = values.tail.foldLeft(init)(_.addCol(_, OuterJoin))
        Frame(temp.values, temp.rowIx, colIx)
      }
    }
  }

  /**
    * Factory method to create a Frame from a sequence of tuples, where the
    * first element of the tuple is a column label, and the second a series
    * of values. The row labels of the result are the outer join of the
    * indexes of the series provided.
    */
  def apply[RX: ST: ORD, CX: ST: ORD, T: ST](
      values: (CX, Series[RX, T])*
  ): Frame[RX, CX, T] = {
    val asIdxSeq = values.map(_._2).toIndexedSeq
    val idx = Index(values.map(_._1).toArray)
    asIdxSeq.length match {
      case 0 => empty[RX, CX, T]
      case 1 => Frame(asIdxSeq.map(_.values), asIdxSeq(0).index, idx)
      case _ => {
        val init =
          Frame(Seq(asIdxSeq(0).values), asIdxSeq(0).index, Index(Array(0)))
        val temp = asIdxSeq.tail.foldLeft(init)(_.addCol(_, OuterJoin))
        Frame(temp.values, temp.rowIx, idx)
      }
    }
  }

  // --------------------------------
  // Construct using matrix

  /**
    * Build a Frame from a provided Mat
    */
  def apply[T: ST](values: Mat[T]): Frame[Int, Int, T] =
    apply(
      values,
      new IndexIntRange(values.numRows),
      new IndexIntRange(values.numCols)
    )

  /**
    * Build a Frame from a provided Mat, row index, and col index
    */
  def apply[RX: ST: ORD, CX: ST: ORD, T: ST](
      mat: Mat[T],
      rowIx: Index[RX],
      colIx: Index[CX]
  ): Frame[RX, CX, T] =
    if (mat.length == 0)
      empty[RX, CX, T]
    else {
      new Frame[RX, CX, T](
        MatCols(mat.cols(): _*),
        rowIx,
        colIx,
        Some(mat)
      )
    }

  def table[RX: ST: ORD, CX: ST: ORD](
      values: Vec[(RX, CX)]
  ): Frame[RX, CX, Int] = {
    val map = scala.collection.mutable.AnyRefMap[(RX, CX), Int]()
    values.toArray.foreach { v =>
      map.get(v) match {
        case None    => map.update(v, 1)
        case Some(c) => map.update(v, c + 1)
      }
    }
    map
      .map { case ((rx, cx), count) => (rx, cx, count) }
      .toSeq
      .toFrame
      .sortedRIx
      .sortedCIx
  }
}

/**
  * Convenience constructors for a Frame[RX, CX, Any] that accept arbitrarily-typed Vectors
  * and Series as constructor parameters, leaving their internal representations unchanged.
  */
object Panel {

  /**
    * Factory method to create an empty Frame whose columns have type Any
    * @tparam RX Type of row keys
    * @tparam CX Type of col keys
    */
  def empty[RX: ST: ORD, CX: ST: ORD]: Frame[RX, CX, Any] =
    new Frame[RX, CX, Any](
      MatCols.empty,
      Index.empty[RX],
      Index.empty[CX],
      None
    )

  // --------------------------------
  // Construct using sequence of vectors

  /**
    * Factory method to create a Frame from a sequence of Vec objects
    */
  def apply(values: Vec[_]*): Frame[Int, Int, Any] =
    if (values.isEmpty) empty[Int, Int]
    else {
      val asIdxSeq = values.toIndexedSeq
      apply(
        asIdxSeq,
        IndexIntRange(asIdxSeq(0).length),
        IndexIntRange(asIdxSeq.length)
      )
    }

  /**
    * Factory method to create a Frame from a sequence of Vec objects,
    * a row index, and a column index.
    */
  def apply[RX: ST: ORD, CX: ST: ORD](
      values: Seq[Vec[_]],
      rowIx: Index[RX],
      colIx: Index[CX]
  ): Frame[RX, CX, Any] = {
    val anySeq = values.toIndexedSeq
    if (values.isEmpty)
      empty[RX, CX]
    else
      Frame(toSeqVec(anySeq), rowIx, colIx)
  }

  /**
    * Factory method to create a Frame from a sequence of Vec objects
    * and a column index.
    */
  def apply[CX: ST: ORD](
      values: Seq[Vec[_]],
      colIx: Index[CX]
  ): Frame[Int, CX, Any] =
    if (values.isEmpty) empty[Int, CX]
    else {
      val asIdxSeq = values.toIndexedSeq
      apply(asIdxSeq, IndexIntRange(asIdxSeq(0).length), colIx)
    }

  private def toSeqVec(anySeq: Seq[Vec[_]]): IndexedSeq[Vec[Any]] =
    anySeq.toIndexedSeq.asInstanceOf[IndexedSeq[Vec[Any]]]

  /**
    * Factory method to create a Frame from tuples whose first element is
    * the column label and the second is a Vec of values.
    */
  def apply[CX: ST: ORD, T: ST](values: (CX, Vec[_])*): Frame[Int, CX, Any] = {
    val asIdxSeq = values.map(_._2).toIndexedSeq
    val idx = Index(values.map(_._1).toArray)
    asIdxSeq.length match {
      case 0 => empty[Int, CX]
      case _ =>
        Frame(toSeqVec(asIdxSeq), IndexIntRange(asIdxSeq(0).length), idx)
    }
  }

  // --------------------------------
  // Construct using sequence of series

  private def toSeqSeries[RX](anySeq: Seq[Series[RX, _]]) =
    anySeq.toIndexedSeq.asInstanceOf[IndexedSeq[Series[RX, Any]]]

  /**
    * Factory method to create a Frame from a sequence of Series. The row labels
    * of the result are the outer join of the indexes of the series provided.
    */
  def apply[RX: ST: ORD](values: Series[RX, _]*): Frame[RX, Int, Any] = {
    val asIdxSeq = toSeqSeries(values)
    asIdxSeq.length match {
      case 0 => empty[RX, Int]
      case 1 =>
        Frame(asIdxSeq.map(_.values), asIdxSeq(0).index, IndexIntRange(1))
      case _ => {
        val init =
          Frame(Seq(asIdxSeq(0).values), asIdxSeq(0).index, Index(Array(0)))
        val temp = asIdxSeq.tail.foldLeft(init)(_.addCol(_, OuterJoin))
        Frame(temp.values, temp.rowIx, IndexIntRange(temp.numCols))
      }
    }
  }

  /**
    * Factory method to create a Frame from a sequence of series, also specifying
    * the column index to use. The row labels of the result are the outer join of
    * the indexes of the series provided.
    */
  def apply[RX: ST: ORD, CX: ST: ORD](
      values: Seq[Series[RX, _]],
      colIx: Index[CX]
  ): Frame[RX, CX, Any] = {
    val asIdxSeq = toSeqSeries(values)
    asIdxSeq.length match {
      case 0 => empty[RX, CX]
      case 1 => Frame(asIdxSeq.map(_.values), asIdxSeq(0).index, colIx)
      case _ => {
        val init = Frame(Seq(asIdxSeq(0).values), asIdxSeq(0).index, Index(0))
        val temp = asIdxSeq.tail.foldLeft(init)(_.addCol(_, OuterJoin))
        Frame(temp.values, temp.rowIx, colIx)
      }
    }
  }

  /**
    * Factory method to create a Frame from a sequence of tuples, where the
    * first element of the tuple is a column label, and the second a series
    * of values. The row labels of the result are the outer join of the
    * indexes of the series provided.
    */
  def apply[RX: ST: ORD, CX: ST: ORD](
      values: (CX, Series[RX, _])*
  ): Frame[RX, CX, Any] = {
    val asIdxSeq = toSeqSeries(values.map(_._2))
    val idx = Index(values.map(_._1).toArray)
    asIdxSeq.length match {
      case 0 => empty[RX, CX]
      case 1 => Frame(asIdxSeq.map(_.values), asIdxSeq(0).index, idx)
      case _ => {
        val init =
          Frame(Seq(asIdxSeq(0).values), asIdxSeq(0).index, Index(Array(0)))
        val temp = asIdxSeq.tail.foldLeft(init)(_.addCol(_, OuterJoin))
        Frame(temp.values, temp.rowIx, idx)
      }
    }
  }

}
