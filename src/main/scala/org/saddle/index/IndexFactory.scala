package org.saddle.index

import org.saddle._
import org.joda.time.DateTime
import org.saddle.vec.VecLong
import org.saddle.time.IndexTime

/**
 * Interface for instantiating a Buffer
 */
trait IndexFactory[T] {
  def create(v: Vec[T]): Index[T]
}

object IndexFactory extends IndexFactoryImplicitsLowPriority {
  implicit object bFactory extends IndexFactory[Boolean] {
    def create(v: Vec[Boolean]) = new IndexBool(v)
  }

  implicit object iFactory extends IndexFactory[Int] {
    def create(v: Vec[Int]) = new IndexInt(v)
  }

  implicit object lFactory extends IndexFactory[Long] {
    def create(v: Vec[Long]) = new IndexLong(v)
  }

  implicit object dFactory extends IndexFactory[Double] {
    def create(v: Vec[Double]) = new IndexDouble(v)
  }

  implicit object tFactory extends IndexFactory[DateTime] {
    def create(v: Vec[DateTime]) = {
      val len = v.length
      val tmp = Array.ofDim[Long](len)
      var i = 0
      while (i < len) {
        tmp(i) = v.raw(i).getMillis
        i += 1
      }
      new IndexTime(new IndexLong(new VecLong(tmp)))
    }
  }
}

trait IndexFactoryImplicitsLowPriority {
  implicit def aFactory[T: ST: ORD] = new IndexFactory[T] {
    def create(v: Vec[T]) = new IndexAny[T](v)
  }
}