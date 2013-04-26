package org.saddle.vec

import org.saddle.Vec

/**
 * Enriches Boolean Vec with logic methods
 */
trait VecBoolEnricher {

  // some logic methods. scala.Function1 is not specialized on
  // Boolean input. not sure I care to work around this
  implicit def vecToBoolLogic(v: Vec[Boolean]) = new {
    /**
     * True if all elements are true
     */
    def all: Boolean  = -1 == v.findOne(_ == false)

    /**
     * True if some elements are true
     */
    def some: Boolean = -1 != v.findOne(_ == true)

    /**
     * True if no elements are true
     */
    def none: Boolean = !some

    /**
     * Number of elements which are true
     */
    def countT: Int = v.foldLeft(0)((a, b) => a + (if (b) 1 else 0))

    /**
     * Number of elements which are false
     */
    def countF: Int = v.foldLeft(0)((a, b) => a + (if (b) 0 else 1))
  }
}
