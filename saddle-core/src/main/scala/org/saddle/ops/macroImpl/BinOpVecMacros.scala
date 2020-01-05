package org.saddle.ops.macroImpl

import scala.reflect.macros.blackbox.Context

import scala.language.experimental.macros
import org.saddle.Vec
import org.saddle.ops._

object BinOpVecMacros {

  import BinOpMacros._

  def vecVecIP[A, B, OP]: BinOpInPlace[OP, Vec[A], Vec[B]] =
    macro vecVecIPImpl[A, B, OP]

  def vecVecIPImpl[
      A: cxt.WeakTypeTag,
      B: cxt.WeakTypeTag,
      OP: cxt.WeakTypeTag
  ](cxt: Context) = {
    import cxt.universe._
    val a = weakTypeOf[A]
    val b = weakTypeOf[B]
    val opT = weakTypeOf[OP]

    val (name1, name2, body) =
      inline[A, B, A](cxt)(createOperationBody[A, B, A, OP](cxt))

    val r = q"""
    new _root_.org.saddle.ops.BinOpInPlace[$opT, _root_.org.saddle.Vec[$a], _root_.org.saddle.Vec[$b]] {

      def apply(v1: _root_.org.saddle.Vec[$a], v2: _root_.org.saddle.Vec[$b]) = {
        require(v1.length == v2.length, "Vecs must have the same size!")
        val sz = v1.length
        var i = 0
        while (i < sz) {
          val $name1 = v1.raw(i)
          val $name2 = v2.raw(i)
          v1(i) = $body
          i += 1
        }
        ()
      }
  
    }
    """
    r
  }
  def vecVec[A, B, C, OP]: BinOp[OP, Vec[A], Vec[B], Vec[C]] =
    macro vecVecImpl[A, B, C, OP]

  def vecVecImpl[
      A: cxt.WeakTypeTag,
      B: cxt.WeakTypeTag,
      C: cxt.WeakTypeTag,
      OP: cxt.WeakTypeTag
  ](cxt: Context) = {
    import cxt.universe._
    val a = weakTypeOf[A]
    val b = weakTypeOf[B]
    val c = weakTypeOf[C]
    val opT = weakTypeOf[OP]

    val (name1, name2, body) =
      inline[A, B, C](cxt)(createOperationBody[A, B, C, OP](cxt))

    val r = q"""
      new _root_.org.saddle.ops.BinOp[$opT, _root_.org.saddle.Vec[$a], _root_.org.saddle.Vec[$b],_root_.org.saddle.Vec[$c]] {
  
        def apply(v1: _root_.org.saddle.Vec[$a], v2: _root_.org.saddle.Vec[$b]) = {
          require(v1.length == v2.length, "Vecs must have the same size!")
          val sz = v1.length
          val ar = new Array[$c](sz)
          var i = 0
          while (i < sz) {
            val $name1 = v1.raw(i)
            val $name2 = v2.raw(i)
            ar(i) = $body
            i += 1
          }
          _root_.org.saddle.Vec(ar)
        }
    
      }
      """
    r
  }
  def vecScalarElementwise[A, B, C, OP]: BinOp[OP, Vec[A], B, Vec[C]] =
    macro vecScalarElementwiseImpl[A, B, C, OP]

  def vecScalarElementwiseImpl[
      A: cxt.WeakTypeTag,
      B: cxt.WeakTypeTag,
      C: cxt.WeakTypeTag,
      OP: cxt.WeakTypeTag
  ](cxt: Context) = {
    import cxt.universe._
    val a = weakTypeOf[A]
    val b = weakTypeOf[B]
    val c = weakTypeOf[C]
    val opT = weakTypeOf[OP]

    val (name1, name2, body) =
      inline[A, B, C](cxt)(createOperationBody[A, B, C, OP](cxt))

    val r = q"""
    new _root_.org.saddle.ops.BinOp[$opT, _root_.org.saddle.Vec[$a], $b, _root_.org.saddle.Vec[$c]] {
    def apply(v1: _root_.org.saddle.Vec[$a], v2: $b) = {
      val sz = v1.length
      val ar = new Array[$c](sz)
      var i = 0
      while (i < sz) {
        val $name1 = v1.raw(i)
        val $name2 = v2
        ar(i) = $body
        i += 1
      }
      _root_.org.saddle.Vec(ar)
    }
  }
    """
    r
  }

  def vecScalarElementwiseIP[A, B, OP]: BinOpInPlace[OP, Vec[A], B] =
    macro vecScalarElementwiseIPImpl[A, B, OP]

  def vecScalarElementwiseIPImpl[
      A: cxt.WeakTypeTag,
      B: cxt.WeakTypeTag,
      OP: cxt.WeakTypeTag
  ](cxt: Context) = {
    import cxt.universe._
    val a = weakTypeOf[A]
    val b = weakTypeOf[B]
    val opT = weakTypeOf[OP]

    val (name1, name2, body) =
      inline[A, B, A](cxt)(createOperationBody[A, B, A, OP](cxt))

    val r = q"""
    new _root_.org.saddle.ops.BinOpInPlace[$opT, _root_.org.saddle.Vec[$a], $b] {
    def apply(v1: _root_.org.saddle.Vec[$a], v2: $b) = {
      val sz = v1.length
      var i = 0
      while (i < sz) {
        val $name1 = v1.raw(i)
        val $name2 = v2
        v1(i) = $body
        i += 1
      }
    }
  }
    """
    r
  }

}
