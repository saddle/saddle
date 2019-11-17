package org.saddle

import org.saddle.scalar.ScalarTag
import org.scalacheck._
import scala.reflect.ClassTag

package object framework {
  /* Implicit generators for factories needing Double and DateTime generation. */
  implicit def double = boundedDouble
  implicit def long = Gen.choose(Long.MinValue, Long.MaxValue)
  implicit def int = Gen.choose(Int.MinValue, Int.MaxValue)
  implicit def bool = Gen.oneOf(true, false)

  /** Yield an arbitrary Double List of size 100 */
  implicit def arbList: Arbitrary[List[Double]] =
    Arbitrary(Gen.listOfN(100, boundedDouble))

  /** Yield an arbitrary 100 x 100 Matrix. */
  implicit def arbMatrix[S: ClassTag: Gen]: Arbitrary[Mat[S]] =
    arbMatrixOfN[S](100, 100)

  /** Yield an arbitrary Vector of 100 values. */
  implicit def arbVector[S: ClassTag: Gen]: Arbitrary[Vec[S]] =
    arbVectorOfN[S](100)

  /** Yield an arbitrary Series of 100 values. */
  implicit def arbSeries[X: Gen: Ordering: ClassTag, S: ClassTag: Gen]
      : Arbitrary[Series[X, S]] = arbSeriesOfN[X, S](100)

  /** Yield an arbitrary 10 x 100 Frame. */
  implicit def arbFrame[X: Gen: Ordering: ClassTag, S: ClassTag: Gen]
      : Arbitrary[Frame[X, Int, S]] = arbFrameOfN[X, S](10, 100)

  /** A generator for doubles with bounds at .5 the size of the min/max valyes. */
  def boundedDouble: Gen[Double] = Gen.choose(-1000000d, 1000000d)

  /** A generator for longs between 0 and the current time in milliseconds. */
  def longEpochToNow: Gen[Long] = Gen.choose(1L, System.currentTimeMillis)

  /** A generator for Vectors of an arbitrary size. */
  def genVectorOfN[S: Gen: ScalarTag](size: Int): Gen[Vec[S]] =
    Gen.listOfN(size, implicitly[Gen[S]]).map(l => Vec(l: _*))

  /** A generator for MatS of an arbitrary size. */
  def genMatrixOfN[S: Gen: ScalarTag](cols: Int, rows: Int): Gen[Mat[S]] =
    Gen.listOfN(cols, genVectorOfN[S](rows)).map(v => Mat(v: _*))

  /** A generator for Series of an arbitrary size. */
  def genSeriesOfN[X: Gen: Ordering: ScalarTag, S: Gen: ScalarTag](
      size: Int
  ): Gen[Series[X, S]] =
    for {
      dateTime <- Gen.listOfN(size, implicitly[Gen[X]])
      value <- Gen.listOfN(size, implicitly[Gen[S]])
    } yield Series(dateTime.zip(value): _*)

  /** A generator for FrameS of an arbitrary size. */
  def genFrameOfN[X: Gen: Ordering: ScalarTag, S: Gen: ScalarTag](
      dsize: Int,
      ssize: Int
  ): Gen[Frame[X, Int, S]] =
    Gen.listOfN(dsize, genSeriesOfN[X, S](ssize)).map(ss => Frame(ss: _*))

  /** Yield an arbitrary 100 x 100 Matrix. */
  def arbMatrixOfN[S: ClassTag: Gen](cols: Int, rows: Int): Arbitrary[Mat[S]] =
    Arbitrary(genMatrixOfN[S](cols, rows))

  /** Yield an arbitrary Vector of 100 values. */
  def arbVectorOfN[S: ClassTag: Gen](size: Int): Arbitrary[Vec[S]] =
    Arbitrary(genVectorOfN[S](size))

  /** Yield an arbitrary Series of 100 values. */
  def arbSeriesOfN[X: Gen: Ordering: ClassTag, S: ClassTag: Gen](
      size: Int
  ): Arbitrary[Series[X, S]] = Arbitrary(genSeriesOfN[X, S](size))

  /** Yield an arbitrary 100 x 100 Series. */
  def arbFrameOfN[X: Gen: Ordering: ClassTag, S: ClassTag: Gen](
      cols: Int,
      rows: Int
  ): Arbitrary[Frame[X, Int, S]] = Arbitrary(genFrameOfN[X, S](cols, rows))
}
