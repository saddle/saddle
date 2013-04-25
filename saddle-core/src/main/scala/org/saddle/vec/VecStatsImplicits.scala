package org.saddle.vec

import org.saddle.stats._
import org.saddle.Vec

/**
 * Enrich Vec with stats
 */
trait VecStatsImplicits {
  type Vec2Stats[A] = Vec[A] => VecStats[A]

  implicit def vecToIntStats(s: Vec[Int]): VecStats[Int] = new IntStats(s)
  implicit def vecToLongStats(s: Vec[Long]): VecStats[Long] = new LongStats(s)
  implicit def vecToDoubleStats(s: Vec[Double]): VecStats[Double] = new DoubleStats(s)

  implicit def vecToIntExpStats(s: Vec[Int]): VecExpandingStats[Int] = new IntExpandingStats(s)
  implicit def vecToLongExpStats(s: Vec[Long]): VecExpandingStats[Long] = new LongExpandingStats(s)
  implicit def vecToDoubleExpStats(s: Vec[Double]): VecExpandingStats[Double] = new DoubleExpandingStats(s)

  implicit def vecToIntRollingStats(s: Vec[Int]): VecRollingStats[Int] = new VecRollingStats[Int](s)
  implicit def vecToLongRollingStats(s: Vec[Long]): VecRollingStats[Long] = new VecRollingStats[Long](s)
  implicit def vecToDoubleRollingStats(s: Vec[Double]): VecRollingStats[Double] = new VecRollingStats[Double](s)
}
