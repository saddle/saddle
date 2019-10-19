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
package org.saddle.index

import org.saddle.Index

/**
  * We often need to "reindex" one array/vec/series/frame so as to produce a new
  * one.
  *
  * First, note that an array A of values of type T is actually a mapping defined
  * as
  *
  * {{{
  *                 f
  *   [0, A.length) => {T}
  * }}}
  *
  * That is, each integer index of the array yields a value of type T when it is
  * de-referenced.
  *
  * ReIndexer has two fields, lTake and rTake, which are array-based maps defined
  * for some integers M and N as
  *
  * {{{
  *          g
  *   [0, N) => [-1, M)
  * }}}
  *
  * In other words, lTake and rTake are arrays of length N whose entries are
  * integers between -1 and M-1. We call these "indexers".
  *
  * Given a reindexer, g, a re-indexing operation on array A yields a new array
  * B of length N which represents the following mapping:
  *
  * {{{
  *           B
  *   [0, N)  =>  {T}
  *
  *   where B(i) == f(g(i)) if g(i) != -1
  *              == NA      if g(i) == -1
  *
  * }}}
  *
  *
  * For this to work, the maximum value M in the co-domain of g must be <= A.length.
  * We also augment the mapping f to send -1 to the corresponding NA value of type T.
  *
  * For example, suppose we have Index(0,1,2,4) and Index(0,1,2,3). Performing
  * a full outer join would yield the following ReIndexer:
  *
  *   - lTake = {0, 1, 2, -1, 3},
  *   - rTake = {0, 1, 2, 3, -1},
  *   - index = Index(0, 1, 2, 3, 4).
  *
  * These indexers are then amenable to using with org.saddle.array.take to
  * select elements out of an indexed data structure.
  *
  * A performance optimization is to make lTake and rTake of type Option[Array],
  * where we make a value of None behave as if it were an array of [0, N). So, the
  * "taking" code knows to take the original values. You will therefore see code
  * that looks like:
  *
  * {{{
  *   val v = Vec(...)
  *   val ixer = ReIndexer(...)
  *   ixer.lTake.map(x => v.take(x)) getOrElse v
  * }}}
  */
trait ReIndexer[T] {

  /**
    * Offsets into left index corresponding to new index
    *
    */
  def lTake: Option[Array[Int]]

  /**
    * Offsets into right index corresponding to new index
    *
    */
  def rTake: Option[Array[Int]]

  /**
    * The new index
    */
  def index: Index[T]

  /**
    * Return ReIndexer with lTake and rTake swapped
    */
  def swap: ReIndexer[T] = ReIndexer(rTake, lTake, index)
}

object ReIndexer {

  /**
    * Factory to produce a new [[org.saddle.index.ReIndexer]] instance
    * @param lft The left indexer
    * @param rgt The right indexer
    * @param ix The joint index
    * @tparam T The type of the index
    */
  def apply[T](lft: Option[Array[Int]], rgt: Option[Array[Int]], ix: Index[T]) =
    new ReIndexer[T] {
      def lTake = lft
      def rTake = rgt
      def index = ix
    }
}
