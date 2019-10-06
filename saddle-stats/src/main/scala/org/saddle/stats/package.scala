package org.saddle

import scala.language.implicitConversions

package object stats {
  // stats implicits

  /**
    * Enrich a Frame to provide statistical methods
    */
  implicit def frameToStats[RX, CX, T: ST: NUM](f: Frame[RX, CX, T]) =
    new FrameStats[RX, CX, T](f)

  // stats implicits

  type Vec2Stats[T] = Vec[T] => VecStats[T]
  type Vec2RollingStats[T] = Vec[T] => VecRollingStats[T]
  type Vec2ExpandingStats[T] = Vec[T] => VecExpandingStats[T]

  type Series2Stats[T] = Series[_, T] => VecStats[T]

  /**
    * Enrich Series with basic stats
    * @param s Series[_, T]
    */
  implicit def seriesToStats[T: Vec2Stats](s: Series[_, T]): VecStats[T] =
    implicitly[Vec2Stats[T]].apply(s.values)

  /**
    * Enrich Series with rolling stats
    * @param s Series[_, T]
    */
  implicit def seriesToRollingStats[X: ST: ORD, T: Vec2RollingStats: ST](
      s: Series[X, T]
  ): SeriesRollingStats[X, T] =
    SeriesRollingStats[X, T](s)

  /**
    * Enrich Series with expanding stats
    * @param s Series[_, T]
    */
  implicit def seriesToExpandingStats[X: ST: ORD, T: Vec2ExpandingStats: ST](
      s: Series[X, T]
  ): SeriesExpandingStats[X, T] =
    SeriesExpandingStats[X, T](s)

  implicit def vecToIntStats(s: Vec[Int]): VecStats[Int] = new IntStats(s)
  implicit def vecToLongStats(s: Vec[Long]): VecStats[Long] = new LongStats(s)
  implicit def vecToDoubleStats(s: Vec[Double]): VecStats[Double] =
    new DoubleStats(s)

  implicit def vecToIntExpStats(s: Vec[Int]): VecExpandingStats[Int] =
    new IntExpandingStats(s)
  implicit def vecToLongExpStats(s: Vec[Long]): VecExpandingStats[Long] =
    new LongExpandingStats(s)
  implicit def vecToDoubleExpStats(s: Vec[Double]): VecExpandingStats[Double] =
    new DoubleExpandingStats(s)

  implicit def vecToIntRollingStats(s: Vec[Int]): VecRollingStats[Int] =
    new VecRollingStats[Int](s)
  implicit def vecToLongRollingStats(s: Vec[Long]): VecRollingStats[Long] =
    new VecRollingStats[Long](s)
  implicit def vecToDoubleRollingStats(
      s: Vec[Double]
  ): VecRollingStats[Double] = new VecRollingStats[Double](s)
}
