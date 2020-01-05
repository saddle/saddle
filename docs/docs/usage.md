---
title: 'Usage'
weight: 2
---

The following documentation is taken from the original Saddle documentation at [http://saddle.github.io/doc/quickstart.html](http://saddle.github.io/doc/quickstart.html), and fed into [mdoc](https://scalameta.org/mdoc/).

### 1D vector: Vec[T]

Factories:
```scala mdoc
import org.saddle._
Vec(1, 2, 3)  
Vec(1 to 3 : _*)    
Vec(Array(1,2,3))
Vec.empty[Double] 

vec.ones(2)
vec.zeros(3)
vec.rand(20)
```

Operations:
```scala mdoc:nest
import org.saddle.ops.BinOps._

Vec(1,2,3) + Vec(4,5,6)
Vec(1,2,3) * Vec(4,5,6)
Vec(1,2,3) ** Vec(4,5,6)
```
Note above, that you have to import that `BinOp` instances. 

An alternative set of instances are also available which inline the innermost in Mat and Vec operations:
```scala mdoc:reset
// The macros package contains instances where the innermost loop is inlined.
import org.saddle._
import org.saddle.macros.BinOps._ 
Vec(1,2,3) + Vec(4,5,6)
Vec(1,2,3) * Vec(4,5,6)
Vec(1,2,3) ** Vec(4,5,6)
 
```

Slicing:
```scala mdoc:reset
import org.saddle._
import org.saddle.ops.BinOps._
Vec(1,2,3).at(2)
Vec(1,2,3).raw(2)  
Vec(1,2,3).apply(0,2)
Vec(1,2,3).apply(1 -> *)
Vec(1,2,3).apply(* -> 1)
```

### 1D vector with index: Series[K,V]
A Series combines a Vec with an Index that provides an ordered key-value mapping. We’ll talk more about the details of Index later. 

The key type of a must have a natural ordering (ie, an Ordering of that type within the implicit scope). However, the Series maintains the order in which its data was supplied unless ordered othewise.

Let’s look at a few constructions:

```scala mdoc
// we already know we can convert a Vec
Series(Vec(32, 12, 9))

// we can pass a pair of tuples
Series("a" -> 1, "b" -> 2, "c" -> 3)

// any series of tuples will work, eg:
Series(List("a" -> 1, "b" -> 2, "c" -> 3) : _*)

// can pass data and index separately:
Series(Vec(1,2,3), Index("a", "b", "c"))

// you can create an empty Series like so:
Series.empty[String, Int]

// supplied order is maintained:
Series(Vec(1,2,3), Index("c", "b", "a"))

// unlike map, multiple keys are entirely fine:
Series(Vec(1,2,3,4), Index("c", "b", "a", "b"))
```

With construction out of the way, let’s look at a few ways we can get data out of a Series.

```scala mdoc
 val q = Series(Vec(1,3,2,4), Index("c", "b", "a", "b"))

// get the values or index
 q.values
 q.index

// extract value by numerical offset
 q.at(2)

 q.at(2,3,1)

// or extract key
 q.keyAt(2)

 q.keyAt(2,3,1)

// sort by index ordering
 q.sortedIx

// sort by value ordering
 q.sorted

// extract elements matching the index
 q("b")

 q("a", "b")

// notice ordering subtleties:
 q("b", "a")

// get first or last values
 q.first
 q.last

// or key
 q.firstKey
 q.lastKey

// "reindex" to a new index:
 q.reindex(Index("a","c","d"))

// or just by a sequence of keys:
 q.reindex("a","c","d")

// notice that 'slicing' ignores unknown keys:
 q("a", "d")
```

```scala mdoc:crash
// we cannot reindex with "b", because it isn't unique.
// (the problem is, which "b" would we choose?)
 q.reindex("a", "b")
```

```scala mdoc
// we can "reset" the index to integer labels
 q.resetIndex

// or to a new index altogether
 q.setIndex(Index("w", "x", "y", "z"))

// to 'slice', we need a sorted index; slice is inclusive by default
 val s = q.sortedIx
 s.sliceBy("b", "c")

// syntactic sugar is provided:
 s.sliceBy("b" -> "c")
 s.sliceBy(* -> "b")

// where slice is by offset, exclusive by default, and the
// index doesn't have to be sorted:
 q.slice(0,2)

// there are head/tail methods:
 q.head(2)
 q.tail(2)
```

Aside from extracting values, there are many fun ways to compute with Series. Try the following:

```scala mdoc
q.mapValues(_ + 1)
q.mapIndex(_ + "x")
q.shift(1)
q.filter(_ > 2)
q.filterIx(_ != "b")
q.filterAt { case loc => loc != 1 && loc != 3 }
q.find(_ == 2)
q.findKey { case x => x == 2 || x == 3 }
q.findOneKey { case x => x == 2 || x == 3 }
q.minKey
q.contains("a")
q.scanLeft(0) { case (acc, v) => acc + v }
q.reversed

val ma = q.mask(q.values > 2)
ma.hasNA
ma.dropNA

q.rolling(2, _.minKey)
q.splitAt(2)
q.sortedIx.splitBy("b")
```

We can of course convert to a Vec or a Seq if we need to. The Series.toSeq method yields a sequence of key/value tuples.

```scala mdoc
 q.toVec
 q.toSeq
```

We can also group by key in order to transform or combine the groupings, which themselves are Series. For example:

```scala mdoc
q.groupBy.combine(_.sum)

q.groupBy.transform(s => s - s.mean)
```
You can also group by another index, or by a transformation of the current index, by passing an argument into groupBy. See the Saddle API for more info.

The expressive nature of working with Series becomes apparent when you need to align data:

```scala mdoc
val a = Series(Vec(1,4,2,3), Index("a","b","c","d"))
val b = Series(Vec(5,2,1,8,7), Index("b","c","d","e","f"))

a + b
```
You see that the indexes have been aligned prior to operation being performed. Because there is a missing observation in each label of a, e, and f, the summation is not done and instead an NA value is inserted into the result.

Generally, a full-outer join is performed. So, for instance:
```scala mdoc
val c = Series(Vec(1,4,2), Index("a","b","b"))
val d = Series(Vec(5,2,1), Index("b","b","d"))

c + d
```
Most basic math and boolean operations are supported between two Series, as well as between a Series and a scalar value.

We mentioned joins. Let’s look at a few join operations; the result is a Frame, which we will touch on a bit later. These are similar in nature to SQL joins.

```scala mdoc
 a.join(b, how=index.LeftJoin)

 a.join(b, how=index.RightJoin)

 a.join(b, how=index.InnerJoin)

 a.join(b, how=index.OuterJoin)
``` 

### Matrix: Mat[T]

A `Mat[T]` represents a matrix of values. Internally it is stored as a single contiguous array in row-major order.

The row-major layout is compatible with EJML, a linear algebra library running on the JVM. 
If you want to use a BLAS, you can take a look in `saddle-linalg`.

Let’s start off with construction:

```scala mdoc
import org.saddle._
 Mat(2,2, Array(1,2,3,4))

// all same:
 Mat(Array(Array(1,3), Array(2,4)))
 Mat(Vec(1,3), Vec(2,4))
 Mat(Array(Vec(1,3), Vec(2,4)))

// identity matrix:
 mat.ident(2)

// empty matrix:
 Mat.empty[Double]

// zeros:
 Mat[Int](2, 2)
```

Again, sometimes we want to create instances filled with random observations. As to Vec, we can do the following:

```scala mdoc
 mat.rand(2,2)       
 mat.randp(2,2)      // random positive doubles
 mat.randn(2,2)      // random normally distributed doubles
 mat.randn2(2,2,3,12) // random normally distributed with mean=3, stdev=12
``` 
There are a few other factory methods available:

```scala mdoc
 mat.ones(2,2)
 mat.zeros(2,2)
 mat.diag(Vec(1,2))
``` 
Let’s look at some basic operations with Mat. As with Vec, you may perform calculations on two Mat instances, or on a Mat and a scalar value.

```scala mdoc
// element-wise multiplication
 Mat(2,2,Array(1,2,3,4)) * Mat(2,2,Array(4,1,2,3))
```

Matrix multiplication and matrix-vector multiplication needs `saddle-linalg`, which depends on netlib-java. These only work on `Mat[Double]` and `Vec[Double]`. 

```scala mdoc
import org.saddle.linalg._
// matrix multiplication
 Mat(2,2,Array(1d,2d,3d,4d)) mm Mat(2,2,Array(4d,1d,2d,3d))


// matrix-vector multiplication
 Mat(2,2,Array(1d,2d,3d,4d)) mv Vec(2d,1d)

// as expected
 Mat(2,2,Array(1,2,3,4)) * 2
 Mat(2,2,Array(1,2,3,4)) + 2
 Mat(2,2,Array(1,2,3,4)) << 2
// etc...

// transpose
 Mat(2,2,Array(1,2,3,4)).T

// properties of Mat
val m = Mat(2,2,Array(1,2,3,4))
 m.numRows
 m.numCols
 m.isSquare
 m.isEmpty
``` 
There are a few ways to extract values from a Mat.
```scala mdoc
 m.at(0,1)

// be careful with this one!
 m.raw(0,1)

 m.takeRows(0)

 m.withoutRows(0)

 m.takeCols(0)

 m.col(0)
 m.row(0)
 m.rows()
 m.cols()
``` 
Some other interesting methods on Mat:

```scala mdoc
 val m2 = Mat(2,2,Array(1,2,na.to[Int],4))

 m2.rowsWithNA
 m2.dropRowsWithNA
 m2.reshape(1,4)
 mat.rand(2,2).roundTo(2)
``` 

### Homogeneous table with row and column index (data frame) : Frame[RX,CX,T]

A Frame combines a Mat with a row index and a column index which provides a way to index into the Mat. 

A Frame is represented internally as a sequence of column Vec instances all sharing the same row index.

Let’s look at some ways to instantiated a Frame:

```scala mdoc
 val v = Vec(1, 2)                              // given the following
 val u = Vec(3, 4)
 val s2 = Series("a" -> 1, "b" -> 2)
 val t = Series("b" -> 3, "c" -> 4)

 Frame(v, u)                                    // two-column frame

 Frame("x" -> v, "y" -> u)                      // with column index

 Frame(s2, t)                                    // aligned along rows

 Frame("x" -> s2, "y" -> t)                      // with column index

 Frame(Seq(s2, t), Index("x", "y"))              // explicit column index

 Frame(Seq(v, u), Index(0, 1), Index("x", "y")) // row & col indexes specified explicitly

 Frame(Seq(v, u), Index("a", "b"))              // col index specified
```

You’ll notice that if an index is not provided, a default int index is set where the index ranges between 0 and the length of the data.

If you want to set or reset the index, these methods are your friends:
```scala mdoc
 val f = Frame("x" -> s2, "y" -> t)

 f.setRowIndex(org.saddle.index.IndexIntRange(f.numRows))
 f.setColIndex(Index("p", "q"))
 f.resetRowIndex
 f.resetColIndex
```
(Note: frame f will carry through the next examples.)

You also have the following index transformation tools at hand:
```scala mdoc
f.mapRowIndex { case rx => rx+1 }
f.mapColIndex { case cx => cx+2 }
```
Let’s next look at how to extract data from the Frame.
```scala mdoc
 f.rowAt(2)    // extract row at offset 2, as Series
 f.rowAt(1,2)  // extract frame of rows 1 & 2
 f.rowAt(1->2) // extract frame of rows 1 & 2

 f.colAt(1)    // extract col at offset 1, as Series
 f.colAt(0,1)  // extract frame of cols 1 & 2
 f.colAt(0->1) // extract frame of cols 1 & 2
``` 
rowAt and colAt are used under the hood for the at extractor:
```scala mdoc
 f.at(1,1)              // Scalar value
 f.at(Array(1,2), 0)    // extract rows 1,2 of column 0
```
If you want more control over slicing, you can use these methods:

```scala mdoc
 f.colSlice(0,1)        // frame slice consisting of column 0
 f.rowSlice(0,3,2)      // row slice from 0 until 3, striding by 2
``` 
Of course, this is an bi-indexed data structure, so we can use its indexes to select out data using keys:
```scala mdoc
 f.row("a")             // row series 'a', with all columns
 f.col("x")             // col series 'x', with all rows
 f.row("a", "c")        // select two rows
 f.row("a"->"b")        // slice two rows (index must be sorted)
 f.row(Seq("a", "c"):_*)   // another way to select
``` 
A more explict way to slice with keys is as follows, and you can specify whether the right bound is inclusive or exclusive. Again, to slice, the index keys must be ordered.

```scala mdoc
 f.rowSliceBy("a", "b", inclusive=false)
 f.colSliceBy("x", "x", inclusive=true)
``` 
The row and col methods are used under the hood for the apply method:

```scala mdoc
 f("a", "x")             // extract a one-element frame by keys
 f("a"->"b", "x")        // two-row, one-column frame
 f(Vec("a", "c").toArray, "x")   // same as above, but extracting, not slicing
``` 
The methods of extracting multiple rows shown above can of course be done on columns as well.

You can also split up the Frame by key or index:

```scala mdoc
 f.colSplitAt(1)          // split into two frames at column 1
 f.colSplitBy("y")

 f.rowSplitAt(1)
 f.rowSplitBy("b")
``` 
You extract some number of rows or columns:
```scala mdoc
 f.head(2)                // operates on rows
 f.tail(2)
 f.headCol(1)             // operates on cols
 f.tailCol(1)
``` 
Or the first & last of some key (which is helpful when you’ve got a multi-key index):
```scala mdoc
 f.first("b")              // first row indexed by "b" key
 f.last("b")               // last row indexed by "b" key
 f.firstCol("x")
 f.lastCol("x")
``` 
There are a few other methods of extracting data:

```scala mdoc
import org.saddle.linalg._
 f.filter { case s => s.toVec.map(_.toDouble).mean2 > 2.0 }  // any column whose series satisfies predicate
 f.filterIx { case x => x == "x" }    // col where index matches key "x"
 f.where(Vec(false, true))            // extract second column
``` 
There are analogous methods to operate on rows rather then columns:
```scala
rfilter
rfilterIx
rwhere
```
etc... in general, methods operate on a column-wise basis, whereas the r-counterpart does so on a row-wise basis.

You can drop cols (rows) containing any NA values:
```scala mdoc
 f.dropNA
 f.rdropNA
``` 
Let’s take a look at some operations we can do with Frames. We can do all the normal binary math operations with Frames, with either a scalar value or with another Frame. When two frames are involved, they are reindexed along both axes to match the outer join of their indices, but any missing observation in either will carry through the calculations.

```scala mdoc
 f + 1
 f * f
 val g = Frame("y"->Series("b"->5, "d"->10))
 f + g                      // one non-NA entry, ("b", "y", 8)
``` 
You can effectively supply your own binary frame operation using joinMap, which lets you control the join style on rows and columns:
```scala mdoc
 f.joinMap(g, rhow=index.LeftJoin, chow=index.LeftJoin) { case (x, y) => x + y }
```
If you want simply to align one frame to another without performing an operation, use the following method:
```scala mdoc
 val (fNew, gNew) = f.align(g, rhow=index.LeftJoin, chow=index.OuterJoin)
```
If you want to treat a Frame as a matrix to use in linear algebraic fashion, call the `toMat` method.

We can sort a frame in various ways:

```scala mdoc
 f.sortedRIx                // sorted by row index
 f.sortedCIx                // sorted by col index
 f.sortedRows(0,1)          // sort rows by (primary) col 0 and (secondary) col 1
 f.sortedCols(1,0)          // sort cols by (primary) row 1 and (secondary) row 0
``` 
We can also sort by an ordering provided by the result of a function acting on rows or cols:

```scala mdoc
 f.sortedRowsBy { case r => r.at(0) }   // sort rows by first element of row
 f.sortedColsBy { case c => c.at(0) }   // sort cols by first element of col
``` 
There are several mapping functions:

```scala mdoc
 f.mapValues { case t => t + 1 }        // add one to each element of frame
 import org.saddle.linalg._
 f.mapVec { case v => v.map(_.toDouble).demeaned }      // map over each col vec of the frame
 f.reduce { case s => s.toVec.map(_.toDouble).mean2 }          // collapse each col series to a single value
 f.transform { case s => s.reversed }   // transform each series; outerjoin results
``` 
We can mask out values:
```scal mdoc
 f.mask(_ > 2)                          // mask out values > 2
 f.mask(Vec(false, true, true))         // mask out rows 1 & 2 (keep row 0)
``` 
Columns (rows) containing only NA values can be dropped as follows:
```scala mdoc
 f.mask(Vec(true, false, false)).rsqueeze   // drop rows containing NA values
 f.rmask(Vec(false, true)).squeeze          // takes "x" column
``` 
We can groupBy in order to combine or transform:
```scala mdoc
import org.saddle.linalg._
 f.groupBy(_ == "a").combine(_.count)       // # obs in each column that have/not row key "a"
 f.groupBy(_ == "a").transform(_.map(_.toDouble).demeaned)  // contrived, but you get the idea hopefully!
```
We can join against another frame, or against a series. 
```scala mdoc
 f.rconcat(g, how=index.LeftJoin)              
 f.concat(g, how=index.LeftJoin)              
 f.cbind(g, how=index.LeftJoin)              
 f.rbind(g, how=index.LeftJoin)              
``` 
Btw, to join a Frame to a series, the call looks like this:
```scala mdoc
 s.joinF(g, how=index.LeftJoin)
``` 
