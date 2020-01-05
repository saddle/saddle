package org.saddle.ops.macroImpl

import scala.reflect.macros.blackbox.Context

import scala.language.experimental.macros
import org.saddle.Mat
import org.saddle.ops._

object BinOpMatMacros {

  import BinOpMacros._

  def matMatIP[A, B, OP]: BinOpInPlace[OP, Mat[A], Mat[B]] =
    macro matMatIPImpl[A, B, OP]

  def matMatIPImpl[
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
    new _root_.org.saddle.ops.BinOpInPlace[$opT, _root_.org.saddle.Mat[$a], _root_.org.saddle.Mat[$b]] {

      def apply(v1: _root_.org.saddle.Mat[$a], v2: _root_.org.saddle.Mat[$b]) =
        if (v1.numRows == v2.numRows && v1.numCols == v2.numCols) {
          val sz = v1.length
          var i = 0
          val v1a = v1.toArray
          val v2a = v2.toArray
          while (i < sz) {
            val $name1 = v1a(i)
            val $name2 = v2a(i)
            v1a(i) = $body
            i += 1
          }
          
        } else if ((v1.numRows == v2.numRows || v1.numRows == 1 || v2.numRows == 1) &&
                   (v1.numCols == v2.numCols || v1.numCols == 1 || v2.numCols == 1)) {
          //   Broadcasting
          val nR = _root_.scala.math.max(v1.numRows, v2.numRows)
          val nC = _root_.scala.math.max(v1.numCols, v2.numCols)
          val nR1 = v1.numRows
          val nR2 = v2.numRows
          val nC1 = v1.numCols
          val nC2 = v2.numCols
          val v1a = v1.toArray
          val v2a = v2.toArray
          val sz = nR * nC
          var i = 0
          while (i < sz) {
            val r = i / nC
            val c = i % nC
            val r1 = if (nR1 == 1) 0 else r
            val r2 = if (nR2 == 1) 0 else r
            val c1 = if (nC1 == 1) 0 else c
            val c2 = if (nC2 == 1) 0 else c
            val $name1 = v1a(r1 * nC1 + c1)
            val $name2 = v2a(r2 * nC2 + c2)
            v1a(i) = $body
            i += 1
          }
          
        } else throw new RuntimeException("Mats must have compatible size!")
  
    }
    """
    r
  }
  def matMat[A, B, C, OP]: BinOp[OP, Mat[A], Mat[B], Mat[C]] =
    macro matMatImpl[A, B, C, OP]

  def matMatImpl[
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
    new _root_.org.saddle.ops.BinOp[$opT, _root_.org.saddle.Mat[$a], _root_.org.saddle.Mat[$b], _root_.org.saddle.Mat[$c]] {

      def apply(v1: _root_.org.saddle.Mat[$a], v2: _root_.org.saddle.Mat[$b]) =
        if (v1.numRows == v2.numRows && v1.numCols == v2.numCols) {
          val sz = v1.length
          val ar = new Array[$c](sz)
          var i = 0
          val v1a = v1.toArray
          val v2a = v2.toArray
          while (i < sz) {
            val $name1 = v1a(i)
            val $name2 = v2a(i)
            ar(i) = $body
            i += 1
          }
          _root_.org.saddle.Mat(v1.numRows, v1.numCols, ar)
        } else if ((v1.numRows == v2.numRows || v1.numRows == 1 || v2.numRows == 1) &&
                   (v1.numCols == v2.numCols || v1.numCols == 1 || v2.numCols == 1)) {
          //   Broadcasting
          val nR = _root_.scala.math.max(v1.numRows, v2.numRows)
          val nC = _root_.scala.math.max(v1.numCols, v2.numCols)
          val nR1 = v1.numRows
          val nR2 = v2.numRows
          val nC1 = v1.numCols
          val nC2 = v2.numCols
          val v1a = v1.toArray
          val v2a = v2.toArray
          val sz = nR * nC
          val ar = new Array[$c](sz)
          var i = 0
          while (i < sz) {
            val r = i / nC
            val c = i % nC
            val r1 = if (nR1 == 1) 0 else r
            val r2 = if (nR2 == 1) 0 else r
            val c1 = if (nC1 == 1) 0 else c
            val c2 = if (nC2 == 1) 0 else c
            val $name1 = v1a(r1 * nC1 + c1)
            val $name2 = v2a(r2 * nC2 + c2)
            ar(i) = $body
            i += 1
          }
          _root_.org.saddle.Mat(nR, nC, ar)
        } else throw new RuntimeException("Mats must have compatible size!")
  
    }
    """
    r
  }
  def matScalarElementwise[A, B, C, OP]: BinOp[OP, Mat[A], B, Mat[C]] =
    macro matScalarElementwiseImpl[A, B, C, OP]

  def matScalarElementwiseImpl[
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
    new _root_.org.saddle.ops.BinOp[$opT, _root_.org.saddle.Mat[$a], $b, _root_.org.saddle.Mat[$c]] {
    def apply(v1: _root_.org.saddle.Mat[$a], v2: $b) = {
      val sz = v1.length
      val v1a = v1.toArray
      val ar = new Array[$c](sz)
      var i = 0
      while (i < sz) {
        val $name1 = v1a(i)
        val $name2 = v2
        ar(i) = $body
        i += 1
      }
      _root_.org.saddle.Mat(v1.numRows, v1.numCols, ar)
    }
  }
    """
    r
  }

  def matScalarElementwiseIP[A, B, OP]: BinOpInPlace[OP, Mat[A], B] =
    macro matScalarElementwiseIPImpl[A, B, OP]

  def matScalarElementwiseIPImpl[
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
    new _root_.org.saddle.ops.BinOpInPlace[$opT, _root_.org.saddle.Mat[$a], $b] {
    def apply(v1: _root_.org.saddle.Mat[$a], v2: $b) = {
      val sz = v1.length
      val v1a = v1.toArray
      var i = 0
      while (i < sz) {
        val $name1 = v1a(i)
        val $name2 = v2
        v1a(i) = $body
        i += 1
      }
    }
  }
    """
    r
  }

}
