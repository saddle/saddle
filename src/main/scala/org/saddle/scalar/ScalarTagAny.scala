package org.saddle.scalar

import org.saddle._

/**
 * User: Adam Klein
 * Date: 3/6/13
 * Time: 5:55 PM
 */
private[saddle] object ScalarTagAny {
  def apply[T : CLM] = new ScalarTag[T] {
    def missing: T = null.asInstanceOf[T]
    def isMissing(v: T): Boolean = v == null
    def notMissing(v: T): Boolean = v != null

    def isTuple = false

    def classTag = implicitly[CLM[T]]

    def compare(x: T, y: T)(implicit ev: ORD[T]): Int =
      if (x == null && y == null) 0
      else if (x == null) -1
      else if (y == null) +1
      else ev.compare(x, y)

    def toDouble(t: T)(implicit ev: NUM[T]) = ev.toDouble(t)
    def isDouble = false

    def zero(implicit ev: NUM[T]) = ev.zero
    def one(implicit ev: NUM[T]) = ev.one
    def inf(implicit ev: NUM[T]) = sys.error("Infinities not supported")
    def negInf(implicit ev: NUM[T]) = sys.error("Infinities not supported")

    def show(v: T) = "%s" format (if (v == null) "NA" else v.toString)
  }
}
