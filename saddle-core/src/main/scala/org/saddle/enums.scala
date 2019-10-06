package org.saddle

/**
  * Trait which specifies how to break a rank tie
  */
sealed trait RankTie

object RankTie {

  /**
    * Take the average of the ranks for all ties
    *
    * {{{
    *   Vec(3,6,6,4).rank(tie = stats.RankTie.Avg) == Vec[Double](1,3.5,3.5,2)
    * }}}
    */
  object Avg extends RankTie

  /**
    * Take the minimum rank for all ties
    *
    * {{{
    *   Vec(3,6,6,4).rank(tie = stats.RankTie.Min) == Vec[Double](1,3,3,2)
    * }}}
    */
  object Min extends RankTie

  /**
    * Take the maximum rank for all ties
    *
    * {{{
    *   Vec(3,6,6,4).rank(tie = stats.RankTie.Max) == Vec[Double](1,4,4,2)
    * }}}
    */
  object Max extends RankTie

  /**
    * Take the rank according to natural (input) order
    *
    * {{{
    *   Vec(3,6,6,4).rank(tie = stats.RankTie.Nat) == Vec[Double](1,3,4,2)
    * }}}
    */
  object Nat extends RankTie
}

/**
  * Trait which specifies what percentile method to use
  */
sealed trait PctMethod

object PctMethod {

  /**
    * Take percentile as MS Excel does
    */
  object Excel extends PctMethod

  /**
    * Take percentile according to [[http://www.itl.nist.gov/div898/handbook/prc/section2/prc252.htm NIST]]
    */
  object NIST extends PctMethod
}
