package org.saddle.macros

import org.saddle.ops._
import org.saddle.ops.macroImpl.BinOpVecMacros.{vecVec => make}

trait BinOpVecVecCopy {

  // basic operations
  implicit val vecVec_Copy_DD_Add = make[Double, Double, Double, Add]
  implicit val vecVec_Copy_LD_Add = make[Long, Double, Double, Add]
  implicit val vecVec_Copy_DL_Add = make[Double, Long, Double, Add]
  implicit val vecVec_Copy_DI_Add = make[Double, Int, Double, Add]
  implicit val vecVec_Copy_ID_Add = make[Int, Double, Double, Add]
  implicit val vecVec_Copy_LL_Add = make[Long, Long, Long, Add]
  implicit val vecVec_Copy_LI_Add = make[Long, Int, Long, Add]
  implicit val vecVec_Copy_IL_Add = make[Int, Long, Long, Add]
  implicit val vecVec_Copy_II_Add = make[Int, Int, Int, Add]

  implicit val vecVec_Copy_DD_Power = make[Double, Double, Double, Power]
  implicit val vecVec_Copy_LD_Power = make[Long, Double, Double, Power]
  implicit val vecVec_Copy_DL_Power = make[Double, Long, Double, Power]
  implicit val vecVec_Copy_DI_Power = make[Double, Int, Double, Power]
  implicit val vecVec_Copy_ID_Power = make[Int, Double, Double, Power]
  implicit val vecVec_Copy_LL_Power = make[Long, Long, Long, Power]
  implicit val vecVec_Copy_LI_Power = make[Long, Int, Long, Power]
  implicit val vecVec_Copy_IL_Power = make[Int, Long, Long, Power]
  implicit val vecVec_Copy_II_Power = make[Int, Int, Int, Power]

  implicit val vecVec_Copy_DD_Sub = make[Double, Double, Double, Subtract]
  implicit val vecVec_Copy_LD_Sub = make[Long, Double, Double, Subtract]
  implicit val vecVec_Copy_DL_Sub = make[Double, Long, Double, Subtract]
  implicit val vecVec_Copy_DI_Sub = make[Double, Int, Double, Subtract]
  implicit val vecVec_Copy_ID_Sub = make[Int, Double, Double, Subtract]
  implicit val vecVec_Copy_LL_Sub = make[Long, Long, Long, Subtract]
  implicit val vecVec_Copy_LI_Sub = make[Long, Int, Long, Subtract]
  implicit val vecVec_Copy_IL_Sub = make[Int, Long, Long, Subtract]
  implicit val vecVec_Copy_II_Sub = make[Int, Int, Int, Subtract]

  implicit val vecVec_Copy_DD_Mult = make[Double, Double, Double, Multiply]
  implicit val vecVec_Copy_LD_Mult = make[Long, Double, Double, Multiply]
  implicit val vecVec_Copy_DL_Mult = make[Double, Long, Double, Multiply]
  implicit val vecVec_Copy_DI_Mult = make[Double, Int, Double, Multiply]
  implicit val vecVec_Copy_ID_Mult = make[Int, Double, Double, Multiply]
  implicit val vecVec_Copy_LL_Mult = make[Long, Long, Long, Multiply]
  implicit val vecVec_Copy_LI_Mult = make[Long, Int, Long, Multiply]
  implicit val vecVec_Copy_IL_Mult = make[Int, Long, Long, Multiply]
  implicit val vecVec_Copy_II_Mult = make[Int, Int, Int, Multiply]

  implicit val vecVec_Copy_DD_Div = make[Double, Double, Double, Divide]
  implicit val vecVec_Copy_LD_Div = make[Long, Double, Double, Divide]
  implicit val vecVec_Copy_DL_Div = make[Double, Long, Double, Divide]
  implicit val vecVec_Copy_DI_Div = make[Double, Int, Double, Divide]
  implicit val vecVec_Copy_ID_Div = make[Int, Double, Double, Divide]
  implicit val vecVec_Copy_LL_Div = make[Long, Long, Long, Divide]
  implicit val vecVec_Copy_LI_Div = make[Long, Int, Long, Divide]
  implicit val vecVec_Copy_IL_Div = make[Int, Long, Long, Divide]
  implicit val vecVec_Copy_II_Div = make[Int, Int, Int, Divide]

  implicit val vecVec_Copy_DD_Mod = make[Double, Double, Double, Mod]
  implicit val vecVec_Copy_LD_Mod = make[Long, Double, Double, Mod]
  implicit val vecVec_Copy_DL_Mod = make[Double, Long, Double, Mod]
  implicit val vecVec_Copy_DI_Mod = make[Double, Int, Double, Mod]
  implicit val vecVec_Copy_ID_Mod = make[Int, Double, Double, Mod]
  implicit val vecVec_Copy_LL_Mod = make[Long, Long, Long, Mod]
  implicit val vecVec_Copy_LI_Mod = make[Long, Int, Long, Mod]
  implicit val vecVec_Copy_IL_Mod = make[Int, Long, Long, Mod]
  implicit val vecVec_Copy_II_Mod = make[Int, Int, Int, Mod]

  // bitwise

  implicit val vecVec_Copy_LL_BitAnd = make[Long, Long, Long, BitAnd]
  implicit val vecVec_Copy_LI_BitAnd = make[Long, Int, Long, BitAnd]
  implicit val vecVec_Copy_II_BitAnd = make[Int, Int, Int, BitAnd]

  implicit val vecVec_Copy_LL_BitOr = make[Long, Long, Long, BitOr]
  implicit val vecVec_Copy_LI_BitOr = make[Long, Int, Long, BitOr]
  implicit val vecVec_Copy_II_BitOr = make[Int, Int, Int, BitOr]

  implicit val vecVec_Copy_LL_BitXor = make[Long, Long, Long, BitXor]
  implicit val vecVec_Copy_LI_BitXor = make[Long, Int, Long, BitXor]
  implicit val vecVec_Copy_II_BitXor = make[Int, Int, Int, BitXor]

  implicit val vecVec_Copy_LL_BitShl = make[Long, Long, Long, BitShl]
  implicit val vecVec_Copy_LI_BitShl = make[Long, Int, Long, BitShl]
  implicit val vecVec_Copy_II_BitShl = make[Int, Int, Int, BitShl]

  implicit val vecVec_Copy_LL_BitShr = make[Long, Long, Long, BitShr]
  implicit val vecVec_Copy_LI_BitShr = make[Long, Int, Long, BitShr]
  implicit val vecVec_Copy_II_BitShr = make[Int, Int, Int, BitShr]

  implicit val vecVec_Copy_LL_BitUshr = make[Long, Long, Long, BitUShr]
  implicit val vecVec_Copy_LI_BitUshr = make[Long, Int, Long, BitUShr]
  implicit val vecVec_Copy_II_BitUshr = make[Int, Int, Int, BitUShr]

  // Boolean
  implicit val vecVec_Copy_BB_And = make[Boolean, Boolean, Boolean, AndOp]
  implicit val vecVec_Copy_BB_Or = make[Boolean, Boolean, Boolean, OrOp]
  implicit val vecVec_Copy_BB_Xor = make[Boolean, Boolean, Boolean, XorOp]
}
