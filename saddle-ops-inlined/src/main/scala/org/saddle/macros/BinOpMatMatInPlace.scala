package org.saddle.macros

import org.saddle.ops._
import org.saddle.ops.macroImpl.BinOpMatMacros.{matMatIP => make}

trait BinOpMatMatInPlace {

  // basic operations
  implicit val matMatIP_DD_Add = make[Double, Double, Add]
  implicit val matMatIP_DL_Add = make[Double, Long, Add]
  implicit val matMatIP_DI_Add = make[Double, Int, Add]
  implicit val matMatIP_LL_Add = make[Long, Long, Add]
  implicit val matMatIP_LI_Add = make[Long, Int, Add]
  implicit val matMatIP_II_Add = make[Int, Int, Add]

  implicit val matMatIP_DD_Power = make[Double, Double, Power]
  implicit val matMatIP_DL_Power = make[Double, Long, Power]
  implicit val matMatIP_DI_Power = make[Double, Int, Power]
  implicit val matMatIP_LL_Power = make[Long, Long, Power]
  implicit val matMatIP_LI_Power = make[Long, Int, Power]
  implicit val matMatIP_II_Power = make[Int, Int, Power]

  implicit val matMatIP_DD_Sub = make[Double, Double, Subtract]
  implicit val matMatIP_DL_Sub = make[Double, Long, Subtract]
  implicit val matMatIP_DI_Sub = make[Double, Int, Subtract]
  implicit val matMatIP_LL_Sub = make[Long, Long, Subtract]
  implicit val matMatIP_LI_Sub = make[Long, Int, Subtract]
  implicit val matMatIP_II_Sub = make[Int, Int, Subtract]

  implicit val matMatIP_DD_Mult = make[Double, Double, Multiply]
  implicit val matMatIP_DL_Mult = make[Double, Long, Multiply]
  implicit val matMatIP_DI_Mult = make[Double, Int, Multiply]
  implicit val matMatIP_LL_Mult = make[Long, Long, Multiply]
  implicit val matMatIP_LI_Mult = make[Long, Int, Multiply]
  implicit val matMatIP_II_Mult = make[Int, Int, Multiply]

  implicit val matMatIP_DD_Div = make[Double, Double, Divide]
  implicit val matMatIP_DL_Div = make[Double, Long, Divide]
  implicit val matMatIP_DI_Div = make[Double, Int, Divide]
  implicit val matMatIP_LL_Div = make[Long, Long, Divide]
  implicit val matMatIP_LI_Div = make[Long, Int, Divide]
  implicit val matMatIP_II_Div = make[Int, Int, Divide]

  implicit val matMatIP_DD_Mod = make[Double, Double, Mod]
  implicit val matMatIP_DL_Mod = make[Double, Long, Mod]
  implicit val matMatIP_DI_Mod = make[Double, Int, Mod]
  implicit val matMatIP_LL_Mod = make[Long, Long, Mod]
  implicit val matMatIP_LI_Mod = make[Long, Int, Mod]
  implicit val matMatIP_II_Mod = make[Int, Int, Mod]

  // bitwise

  implicit val matMatIP_LL_BitAnd = make[Long, Long, BitAnd]
  implicit val matMatIP_LI_BitAnd = make[Long, Int, BitAnd]
  implicit val matMatIP_II_BitAnd = make[Int, Int, BitAnd]

  implicit val matMatIP_LL_BitOr = make[Long, Long, BitOr]
  implicit val matMatIP_LI_BitOr = make[Long, Int, BitOr]
  implicit val matMatIP_II_BitOr = make[Int, Int, BitOr]

  implicit val matMatIP_LL_BitXor = make[Long, Long, BitXor]
  implicit val matMatIP_LI_BitXor = make[Long, Int, BitXor]
  implicit val matMatIP_II_BitXor = make[Int, Int, BitXor]

  implicit val matMatIP_LL_BitShl = make[Long, Long, BitShl]
  implicit val matMatIP_LI_BitShl = make[Long, Int, BitShl]
  implicit val matMatIP_II_BitShl = make[Int, Int, BitShl]

  implicit val matMatIP_LL_BitShr = make[Long, Long, BitShr]
  implicit val matMatIP_LI_BitShr = make[Long, Int, BitShr]
  implicit val matMatIP_II_BitShr = make[Int, Int, BitShr]

  implicit val matMatIP_LL_BitUshr = make[Long, Long, BitUShr]
  implicit val matMatIP_LI_BitUshr = make[Long, Int, BitUShr]
  implicit val matMatIP_II_BitUshr = make[Int, Int, BitUShr]

  // comparison

  implicit val matMatIP_BB_GT = make[Boolean, Boolean, GtOp]

  implicit val matMatIP_BB_GTE = make[Boolean, Boolean, GteOp]

  implicit val matMatIP_BB_LT = make[Boolean, Boolean, LtOp]

  implicit val matMatIP_BB_LTE = make[Boolean, Boolean, LteOp]

  implicit val matMatIP_BB_NEQ = make[Boolean, Boolean, NeqOp]

  implicit val matMatIP_BB_EQ = make[Boolean, Boolean, EqOp]

  // Boolean
  implicit val matMatIP_BB_And = make[Boolean, Boolean, AndOp]
  implicit val matMatIP_BB_Or = make[Boolean, Boolean, OrOp]
  implicit val matMatIP_BB_Xor = make[Boolean, Boolean, XorOp]

}
