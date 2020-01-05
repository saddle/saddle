package org.saddle.macros

import org.saddle.ops._
import org.saddle.ops.macroImpl.BinOpVecMacros.{vecVecIP => make}

trait BinOpVecVecInPlace {

  // basic operations
  implicit val vecVecIP_DD_Add = make[Double, Double, Add]
  implicit val vecVecIP_DL_Add = make[Double, Long, Add]
  implicit val vecVecIP_DI_Add = make[Double, Int, Add]
  implicit val vecVecIP_LL_Add = make[Long, Long, Add]
  implicit val vecVecIP_LI_Add = make[Long, Int, Add]
  implicit val vecVecIP_II_Add = make[Int, Int, Add]

  implicit val vecVecIP_DD_Power = make[Double, Double, Power]
  implicit val vecVecIP_DL_Power = make[Double, Long, Power]
  implicit val vecVecIP_DI_Power = make[Double, Int, Power]
  implicit val vecVecIP_LL_Power = make[Long, Long, Power]
  implicit val vecVecIP_LI_Power = make[Long, Int, Power]
  implicit val vecVecIP_II_Power = make[Int, Int, Power]

  implicit val vecVecIP_DD_Sub = make[Double, Double, Subtract]
  implicit val vecVecIP_DL_Sub = make[Double, Long, Subtract]
  implicit val vecVecIP_DI_Sub = make[Double, Int, Subtract]
  implicit val vecVecIP_LL_Sub = make[Long, Long, Subtract]
  implicit val vecVecIP_LI_Sub = make[Long, Int, Subtract]
  implicit val vecVecIP_II_Sub = make[Int, Int, Subtract]

  implicit val vecVecIP_DD_Mult = make[Double, Double, Multiply]
  implicit val vecVecIP_DL_Mult = make[Double, Long, Multiply]
  implicit val vecVecIP_DI_Mult = make[Double, Int, Multiply]
  implicit val vecVecIP_LL_Mult = make[Long, Long, Multiply]
  implicit val vecVecIP_LI_Mult = make[Long, Int, Multiply]
  implicit val vecVecIP_II_Mult = make[Int, Int, Multiply]

  implicit val vecVecIP_DD_Div = make[Double, Double, Divide]
  implicit val vecVecIP_DL_Div = make[Double, Long, Divide]
  implicit val vecVecIP_DI_Div = make[Double, Int, Divide]
  implicit val vecVecIP_LL_Div = make[Long, Long, Divide]
  implicit val vecVecIP_LI_Div = make[Long, Int, Divide]
  implicit val vecVecIP_II_Div = make[Int, Int, Divide]

  implicit val vecVecIP_DD_Mod = make[Double, Double, Mod]
  implicit val vecVecIP_DL_Mod = make[Double, Long, Mod]
  implicit val vecVecIP_DI_Mod = make[Double, Int, Mod]
  implicit val vecVecIP_LL_Mod = make[Long, Long, Mod]
  implicit val vecVecIP_LI_Mod = make[Long, Int, Mod]
  implicit val vecVecIP_II_Mod = make[Int, Int, Mod]

  // bitwise

  implicit val vecVecIP_LL_BitAnd = make[Long, Long, BitAnd]
  implicit val vecVecIP_LI_BitAnd = make[Long, Int, BitAnd]
  implicit val vecVecIP_II_BitAnd = make[Int, Int, BitAnd]

  implicit val vecVecIP_LL_BitOr = make[Long, Long, BitOr]
  implicit val vecVecIP_LI_BitOr = make[Long, Int, BitOr]
  implicit val vecVecIP_II_BitOr = make[Int, Int, BitOr]

  implicit val vecVecIP_LL_BitXor = make[Long, Long, BitXor]
  implicit val vecVecIP_LI_BitXor = make[Long, Int, BitXor]
  implicit val vecVecIP_II_BitXor = make[Int, Int, BitXor]

  implicit val vecVecIP_LL_BitShl = make[Long, Long, BitShl]
  implicit val vecVecIP_LI_BitShl = make[Long, Int, BitShl]
  implicit val vecVecIP_II_BitShl = make[Int, Int, BitShl]

  implicit val vecVecIP_LL_BitShr = make[Long, Long, BitShr]
  implicit val vecVecIP_LI_BitShr = make[Long, Int, BitShr]
  implicit val vecVecIP_II_BitShr = make[Int, Int, BitShr]

  implicit val vecVecIP_LL_BitUshr = make[Long, Long, BitUShr]
  implicit val vecVecIP_LI_BitUshr = make[Long, Int, BitUShr]
  implicit val vecVecIP_II_BitUshr = make[Int, Int, BitUShr]

  // comparison

  implicit val vecVecIP_BB_GT = make[Boolean, Boolean, GtOp]

  implicit val vecVecIP_BB_GTE = make[Boolean, Boolean, GteOp]

  implicit val vecVecIP_BB_LT = make[Boolean, Boolean, LtOp]

  implicit val vecVecIP_BB_LTE = make[Boolean, Boolean, LteOp]

  implicit val vecVecIP_BB_NEQ = make[Boolean, Boolean, NeqOp]

  implicit val vecVecIP_BB_EQ = make[Boolean, Boolean, EqOp]

  // Boolean
  implicit val vecVec_BB_And = make[Boolean, Boolean, AndOp]
  implicit val vecVec_BB_Or = make[Boolean, Boolean, OrOp]
  implicit val vecVec_BB_Xor = make[Boolean, Boolean, XorOp]

}
