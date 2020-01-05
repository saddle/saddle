package org.saddle.ops.macroImpl

import scala.reflect.macros.blackbox.Context
import org.saddle.ops._
object BinOpMacros {

  def createOperationBody[
      A: cxt.WeakTypeTag,
      B: cxt.WeakTypeTag,
      C: cxt.WeakTypeTag,
      OP: cxt.WeakTypeTag
  ](
      cxt: Context
  ): cxt.Expr[(A, B) => C] = {
    import cxt.universe._
    val a = weakTypeOf[A]
    val b = weakTypeOf[B]
    val c = weakTypeOf[C]
    val opT = weakTypeOf[OP]
    val scalarTagA = a match {
      case t if t =:= weakTypeOf[Double]  => q"org.saddle.scalar.ScalarTagDouble"
      case t if t =:= weakTypeOf[Int]     => q"org.saddle.scalar.ScalarTagInt"
      case t if t =:= weakTypeOf[Long]    => q"org.saddle.scalar.ScalarTagLong"
      case t if t =:= weakTypeOf[Boolean] => q"org.saddle.scalar.ScalarTagBool"
    }
    val scalarTagB = b match {
      case t if t =:= weakTypeOf[Double]  => q"org.saddle.scalar.ScalarTagDouble"
      case t if t =:= weakTypeOf[Int]     => q"org.saddle.scalar.ScalarTagInt"
      case t if t =:= weakTypeOf[Long]    => q"org.saddle.scalar.ScalarTagLong"
      case t if t =:= weakTypeOf[Boolean] => q"org.saddle.scalar.ScalarTagBool"
    }
    val scalarTagC = c match {
      case t if t =:= weakTypeOf[Double]  => q"org.saddle.scalar.ScalarTagDouble"
      case t if t =:= weakTypeOf[Int]     => q"org.saddle.scalar.ScalarTagInt"
      case t if t =:= weakTypeOf[Long]    => q"org.saddle.scalar.ScalarTagLong"
      case t if t =:= weakTypeOf[Boolean] => q"org.saddle.scalar.ScalarTagBool"
    }

    val bothOperandsAreDouble = (a =:= weakTypeOf[Double] && b =:= weakTypeOf[
      Double
    ])

    val tree = opT match {
      // operations on Doubles yielding Doubles do not check for missingness
      case t if t =:= weakTypeOf[Power] && bothOperandsAreDouble =>
        q"""(a:$a,b:$b) => {
        math.pow(a, b)
      }"""
      case t if t =:= weakTypeOf[Divide] && bothOperandsAreDouble =>
        q"""(a:$a,b:$b) => {
         a / b
      }"""
      case t if t =:= weakTypeOf[Multiply] && bothOperandsAreDouble =>
        q"""(a:$a,b:$b) => {
         a * b
      }"""
      case t if t =:= weakTypeOf[Add] && bothOperandsAreDouble =>
        q"""(a:$a,b:$b) => {
         a + b
      }"""
      case t if t =:= weakTypeOf[Subtract] && bothOperandsAreDouble =>
        q"""(a:$a,b:$b) => {
         a - b
      }"""
      case t if t =:= weakTypeOf[Mod] && bothOperandsAreDouble =>
        q"""(a:$a,b:$b) => {
         a % b
      }"""
      case t if t =:= weakTypeOf[GtOp] && bothOperandsAreDouble =>
        q"""(a:$a,b:$b) => {
        a > b
      }"""
      case t if t =:= weakTypeOf[LtOp] && bothOperandsAreDouble =>
        q"""(a:$a,b:$b) => {
        a < b
      }"""
      case t if t =:= weakTypeOf[EqOp] && bothOperandsAreDouble =>
        q"""(a:$a,b:$b) => {
        a == b
      }"""
      case t if t =:= weakTypeOf[NeqOp] && bothOperandsAreDouble =>
        q"""(a:$a,b:$b) => {
         a != b
      }"""
      case t if t =:= weakTypeOf[GteOp] && bothOperandsAreDouble =>
        q"""(a:$a,b:$b) => {
        a >= b
      }"""
      case t if t =:= weakTypeOf[LteOp] && bothOperandsAreDouble =>
        q"""(a:$a,b:$b) => {
        a <= b
      }"""

      // power on Long/Int is converted back to Long/Int
      case t if t =:= weakTypeOf[Power] && c =:= weakTypeOf[Long] =>
        q"""(a:$a,b:$b) => {
        val stA = $scalarTagA
        val stB = $scalarTagB
        val stC = $scalarTagC
        if (stA.isMissing(a) || stB.isMissing(b)) stC.missing
        else math.pow(a, b).toLong
      }"""
      case t if t =:= weakTypeOf[Power] && c =:= weakTypeOf[Int] =>
        q"""(a:$a,b:$b) => {
        val stA = $scalarTagA
        val stB = $scalarTagB
        val stC = $scalarTagC
        if (stA.isMissing(a) || stB.isMissing(b)) stC.missing
        else math.pow(a, b).toInt
      }"""

      // all other operations follow
      case t if t =:= weakTypeOf[Power]    => q"""(a:$a,b:$b) => {
        val stA = $scalarTagA
        val stB = $scalarTagB
        val stC = $scalarTagC
        if (stA.isMissing(a) || stB.isMissing(b)) stC.missing
        else math.pow(a, b)
      }"""
      case t if t =:= weakTypeOf[Divide]   => q"""(a:$a,b:$b) => {
        val stA = $scalarTagA
        val stB = $scalarTagB
        val stC = $scalarTagC
        if (stA.isMissing(a) || stB.isMissing(b)) stC.missing
        else a / b
      }"""
      case t if t =:= weakTypeOf[Multiply] => q"""(a:$a,b:$b) => {
        val stA = $scalarTagA
        val stB = $scalarTagB
        val stC = $scalarTagC
        if (stA.isMissing(a) || stB.isMissing(b)) stC.missing
        else a * b
      }"""
      case t if t =:= weakTypeOf[Add]      => q"""(a:$a,b:$b) => {
        val stA = $scalarTagA
        val stB = $scalarTagB
        val stC = $scalarTagC
        if (stA.isMissing(a) || stB.isMissing(b)) stC.missing
        else a + b
      }"""
      case t if t =:= weakTypeOf[Subtract] => q"""(a:$a,b:$b) => {
        val stA = $scalarTagA
        val stB = $scalarTagB
        val stC = $scalarTagC
        if (stA.isMissing(a) || stB.isMissing(b)) stC.missing
        else a - b
      }"""
      case t if t =:= weakTypeOf[Mod]      => q"""(a:$a,b:$b) => {
        val stA = $scalarTagA
        val stB = $scalarTagB
        val stC = $scalarTagC
        if (stA.isMissing(a) || stB.isMissing(b)) stC.missing
        else a % b
      }"""
      case t if t =:= weakTypeOf[BitAnd]   => q"""(a:$a,b:$b) => {
        val stA = $scalarTagA
        val stB = $scalarTagB
        val stC = $scalarTagC
        if (stA.isMissing(a) || stB.isMissing(b)) stC.missing
        else a & b
      }"""
      case t if t =:= weakTypeOf[BitOr]    => q"""(a:$a,b:$b) => {
        val stA = $scalarTagA
        val stB = $scalarTagB
        val stC = $scalarTagC
        if (stA.isMissing(a) || stB.isMissing(b)) stC.missing
        else a | b
      }"""
      case t if t =:= weakTypeOf[BitXor]   => q"""(a:$a,b:$b) => {
        val stA = $scalarTagA
        val stB = $scalarTagB
        val stC = $scalarTagC
        if (stA.isMissing(a) || stB.isMissing(b)) stC.missing
        else a ^ b
      }"""
      case t if t =:= weakTypeOf[BitShl]   => q"""(a:$a,b:$b) => {
        val stA = $scalarTagA
        val stB = $scalarTagB
        val stC = $scalarTagC
        if (stA.isMissing(a) || stB.isMissing(b)) stC.missing
        else a << b
      }"""
      case t if t =:= weakTypeOf[BitShr]   => q"""(a:$a,b:$b) => {
        val stA = $scalarTagA
        val stB = $scalarTagB
        val stC = $scalarTagC
        if (stA.isMissing(a) || stB.isMissing(b)) stC.missing
        else a >> b
      }"""
      case t if t =:= weakTypeOf[BitUShr]  => q"""(a:$a,b:$b) => {
        val stA = $scalarTagA
        val stB = $scalarTagB
        val stC = $scalarTagC
        if (stA.isMissing(a) || stB.isMissing(b)) stC.missing
        else a >>> b
      }"""
      case t if t =:= weakTypeOf[AndOp]    => q"""(a:$a,b:$b) => {
        a && b
      }"""
      case t if t =:= weakTypeOf[OrOp]     => q"""(a:$a,b:$b) => {
        a || b
      }"""
      case t if t =:= weakTypeOf[XorOp]    => q"""(a:$a,b:$b) => {
        a && b || !a && !b
      }"""
      case t if t =:= weakTypeOf[GtOp]     => q"""(a:$a,b:$b) => {
        val stA = $scalarTagA
        val stB = $scalarTagB
        val stC = $scalarTagC
        if (stA.isMissing(a) || stB.isMissing(b)) stC.missing
        else a > b
      }"""
      case t if t =:= weakTypeOf[LtOp]     => q"""(a:$a,b:$b) => {
        val stA = $scalarTagA
        val stB = $scalarTagB
        val stC = $scalarTagC
        if (stA.isMissing(a) || stB.isMissing(b)) stC.missing
        else a < b
      }"""
      case t if t =:= weakTypeOf[EqOp]     => q"""(a:$a,b:$b) => {
        val stA = $scalarTagA
        val stB = $scalarTagB
        val stC = $scalarTagC
        if (stA.isMissing(a) || stB.isMissing(b)) stC.missing
        else a == b
      }"""
      case t if t =:= weakTypeOf[NeqOp]    => q"""(a:$a,b:$b) => {
        val stA = $scalarTagA
        val stB = $scalarTagB
        val stC = $scalarTagC
        if (stA.isMissing(a) || stB.isMissing(b)) stC.missing
        else a != b
      }"""
      case t if t =:= weakTypeOf[GteOp]    => q"""(a:$a,b:$b) => {
        val stA = $scalarTagA
        val stB = $scalarTagB
        val stC = $scalarTagC
        if (stA.isMissing(a) || stB.isMissing(b)) stC.missing
        else a >= b
      }"""
      case t if t =:= weakTypeOf[LteOp]    => q"""(a:$a,b:$b) => {
        val stA = $scalarTagA
        val stB = $scalarTagB
        val stC = $scalarTagC
        if (stA.isMissing(a) || stB.isMissing(b)) stC.missing
        else a <= b
      }"""
      case _ =>
        cxt.abort(cxt.enclosingPosition, s"Unimplemented operator: $opT")
    }
    cxt.Expr((tree))
  }

  def inline[A: cxt.WeakTypeTag, B: cxt.WeakTypeTag, C: cxt.WeakTypeTag](
      cxt: Context
  )(op: cxt.Expr[(A, B) => C]) = {
    import cxt.universe._

    val (params, opBody) = op.tree.duplicate match {
      case Function(params, body) =>
        (params, body)
      case _ =>
        cxt.abort(
          op.tree.pos,
          "Expected function literal."
        )
    }

    class InlineBody(
        param1: TermName,
        param2: TermName,
        nparam1: TermName,
        nparam2: TermName
    ) extends Transformer {
      override def transform(tree: Tree): Tree = {
        tree match {
          case Ident(x) if x == param1 =>
            q"""$nparam1"""
          case Ident(x) if x == param2 =>
            q"""$nparam2"""
          case _ => super.transform(tree)
        }
      }
    }

    val fresh1 = cxt.freshName(TermName("t"))
    val fresh2 = cxt.freshName(TermName("t"))
    val rewriter =
      new InlineBody(params.head.name, params(1).name, fresh1, fresh2)

    val body = cxt.untypecheck(rewriter.transform(opBody))

    (fresh1, fresh2, body)
  }

}
