package org.saddle.macros

import org.saddle.ops._
import org.saddle.ops.macroImpl.BinOpVecMacros.{vecScalarElementwise => make}

trait BinOpVecCopy {

  // basic operations
  implicit val vecSclr_Copy_DD_Add = make[Double, Double, Double, Add]
  implicit val vecSclr_Copy_DL_Add = make[Double, Long, Double, Add]
  implicit val vecSclr_Copy_DI_Add = make[Double, Int, Double, Add]
  implicit val vecSclr_Copy_LL_Add = make[Long, Long, Long, Add]
  implicit val vecSclr_Copy_LD_Add = make[Long, Double, Double, Add]
  implicit val vecSclr_Copy_LI_Add = make[Long, Int, Long, Add]
  implicit val vecSclr_Copy_IL_Add = make[Int, Long, Long, Add]
  implicit val vecSclr_Copy_II_Add = make[Int, Int, Int, Add]
  implicit val vecSclr_Copy_ID_Add = make[Int, Double, Double, Add]

  implicit val vecSclr_Copy_DD_Power = make[Double, Double, Double, Power]
  implicit val vecSclr_Copy_DL_Power = make[Double, Long, Double, Power]
  implicit val vecSclr_Copy_DI_Power = make[Double, Int, Double, Power]
  implicit val vecSclr_Copy_LL_Power = make[Long, Long, Long, Power]
  implicit val vecSclr_Copy_LD_Power = make[Long, Double, Double, Power]
  implicit val vecSclr_Copy_LI_Power = make[Long, Int, Long, Power]
  implicit val vecSclr_Copy_IL_Power = make[Int, Long, Long, Power]
  implicit val vecSclr_Copy_II_Power = make[Int, Int, Int, Power]
  implicit val vecSclr_Copy_ID_Power = make[Int, Double, Double, Power]

  implicit val vecSclr_Copy_DD_Sub = make[Double, Double, Double, Subtract]
  implicit val vecSclr_Copy_DL_Sub = make[Double, Long, Double, Subtract]
  implicit val vecSclr_Copy_DI_Sub = make[Double, Int, Double, Subtract]
  implicit val vecSclr_Copy_LL_Sub = make[Long, Long, Long, Subtract]
  implicit val vecSclr_Copy_LD_Sub = make[Long, Double, Double, Subtract]
  implicit val vecSclr_Copy_LI_Sub = make[Long, Int, Long, Subtract]
  implicit val vecSclr_Copy_IL_Sub = make[Int, Long, Long, Subtract]
  implicit val vecSclr_Copy_II_Sub = make[Int, Int, Int, Subtract]
  implicit val vecSclr_Copy_ID_Sub = make[Int, Double, Double, Subtract]

  implicit val vecSclr_Copy_DD_Mult = make[Double, Double, Double, Multiply]
  implicit val vecSclr_Copy_DL_Mult = make[Double, Long, Double, Multiply]
  implicit val vecSclr_Copy_DI_Mult = make[Double, Int, Double, Multiply]
  implicit val vecSclr_Copy_LL_Mult = make[Long, Long, Long, Multiply]
  implicit val vecSclr_Copy_LD_Mult = make[Long, Double, Double, Multiply]
  implicit val vecSclr_Copy_LI_Mult = make[Long, Int, Long, Multiply]
  implicit val vecSclr_Copy_IL_Mult = make[Int, Long, Long, Multiply]
  implicit val vecSclr_Copy_II_Mult = make[Int, Int, Int, Multiply]
  implicit val vecSclr_Copy_ID_Mult = make[Int, Double, Double, Multiply]

  implicit val vecSclr_Copy_DD_Div = make[Double, Double, Double, Divide]
  implicit val vecSclr_Copy_DL_Div = make[Double, Long, Double, Divide]
  implicit val vecSclr_Copy_DI_Div = make[Double, Int, Double, Divide]
  implicit val vecSclr_Copy_LL_Div = make[Long, Long, Long, Divide]
  implicit val vecSclr_Copy_LD_Div = make[Long, Double, Double, Divide]
  implicit val vecSclr_Copy_LI_Div = make[Long, Int, Long, Divide]
  implicit val vecSclr_Copy_IL_Div = make[Int, Long, Long, Divide]
  implicit val vecSclr_Copy_II_Div = make[Int, Int, Int, Divide]
  implicit val vecSclr_Copy_ID_Div = make[Int, Double, Double, Divide]

  implicit val vecSclr_Copy_DD_Mod = make[Double, Double, Double, Mod]
  implicit val vecSclr_Copy_DL_Mod = make[Double, Long, Double, Mod]
  implicit val vecSclr_Copy_DI_Mod = make[Double, Int, Double, Mod]
  implicit val vecSclr_Copy_LL_Mod = make[Long, Long, Long, Mod]
  implicit val vecSclr_Copy_LD_Mod = make[Long, Double, Double, Mod]
  implicit val vecSclr_Copy_LI_Mod = make[Long, Int, Long, Mod]
  implicit val vecSclr_Copy_IL_Mod = make[Int, Long, Long, Mod]
  implicit val vecSclr_Copy_II_Mod = make[Int, Int, Int, Mod]
  implicit val vecSclr_Copy_ID_Mod = make[Int, Double, Double, Mod]

  // bitwise

  implicit val vecSclr_Copy_LL_BitAnd = make[Long, Long, Long, BitAnd]
  implicit val vecSclr_Copy_LI_BitAnd = make[Long, Int, Long, BitAnd]
  implicit val vecSclr_Copy_II_BitAnd = make[Int, Int, Int, BitAnd]

  implicit val vecSclr_Copy_LL_BitOr = make[Long, Long, Long, BitOr]
  implicit val vecSclr_Copy_LI_BitOr = make[Long, Int, Long, BitOr]
  implicit val vecSclr_Copy_II_BitOr = make[Int, Int, Int, BitOr]

  implicit val vecSclr_Copy_LL_BitXor = make[Long, Long, Long, BitXor]
  implicit val vecSclr_Copy_LI_BitXor = make[Long, Int, Long, BitXor]
  implicit val vecSclr_Copy_II_BitXor = make[Int, Int, Int, BitXor]

  implicit val vecSclr_Copy_LL_BitShl = make[Long, Long, Long, BitShl]
  implicit val vecSclr_Copy_LI_BitShl = make[Long, Int, Long, BitShl]
  implicit val vecSclr_Copy_II_BitShl = make[Int, Int, Int, BitShl]

  implicit val vecSclr_Copy_LL_BitShr = make[Long, Long, Long, BitShr]
  implicit val vecSclr_Copy_LI_BitShr = make[Long, Int, Long, BitShr]
  implicit val vecSclr_Copy_II_BitShr = make[Int, Int, Int, BitShr]

  implicit val vecSclr_Copy_LL_BitUshr = make[Long, Long, Long, BitUShr]
  implicit val vecSclr_Copy_LI_BitUshr = make[Long, Int, Long, BitUShr]
  implicit val vecSclr_Copy_II_BitUshr = make[Int, Int, Int, BitUShr]

  // comparison

  implicit val vecSclr_Copy_DD_GT = make[Double, Double, Boolean, GtOp]
  implicit val vecSclr_Copy_DL_GT = make[Double, Long, Boolean, GtOp]
  implicit val vecSclr_Copy_DI_GT = make[Double, Int, Boolean, GtOp]
  implicit val vecSclr_Copy_LD_GT = make[Long, Double, Boolean, GtOp]
  implicit val vecSclr_Copy_LL_GT = make[Long, Long, Boolean, GtOp]
  implicit val vecSclr_Copy_LI_GT = make[Long, Int, Boolean, GtOp]
  implicit val vecSclr_Copy_ID_GT = make[Int, Double, Boolean, GtOp]
  implicit val vecSclr_Copy_II_GT = make[Int, Int, Boolean, GtOp]
  implicit val vecSclr_Copy_BB_GT = make[Boolean, Boolean, Boolean, GtOp]

  implicit val vecSclr_Copy_DD_GTE = make[Double, Double, Boolean, GteOp]
  implicit val vecSclr_Copy_DL_GTE = make[Double, Long, Boolean, GteOp]
  implicit val vecSclr_Copy_DI_GTE = make[Double, Int, Boolean, GteOp]
  implicit val vecSclr_Copy_LD_GTE = make[Long, Double, Boolean, GteOp]
  implicit val vecSclr_Copy_LL_GTE = make[Long, Long, Boolean, GteOp]
  implicit val vecSclr_Copy_LI_GTE = make[Long, Int, Boolean, GteOp]
  implicit val vecSclr_Copy_ID_GTE = make[Int, Double, Boolean, GteOp]
  implicit val vecSclr_Copy_II_GTE = make[Int, Int, Boolean, GteOp]
  implicit val vecSclr_Copy_BB_GTE = make[Boolean, Boolean, Boolean, GteOp]

  implicit val vecSclr_Copy_DD_LT = make[Double, Double, Boolean, LtOp]
  implicit val vecSclr_Copy_DL_LT = make[Double, Long, Boolean, LtOp]
  implicit val vecSclr_Copy_DI_LT = make[Double, Int, Boolean, LtOp]
  implicit val vecSclr_Copy_LD_LT = make[Long, Double, Boolean, LtOp]
  implicit val vecSclr_Copy_LL_LT = make[Long, Long, Boolean, LtOp]
  implicit val vecSclr_Copy_LI_LT = make[Long, Int, Boolean, LtOp]
  implicit val vecSclr_Copy_ID_LT = make[Int, Double, Boolean, LtOp]
  implicit val vecSclr_Copy_II_LT = make[Int, Int, Boolean, LtOp]
  implicit val vecSclr_Copy_BB_LT = make[Boolean, Boolean, Boolean, LtOp]

  implicit val vecSclr_Copy_DD_LTE = make[Double, Double, Boolean, LteOp]
  implicit val vecSclr_Copy_DL_LTE = make[Double, Long, Boolean, LteOp]
  implicit val vecSclr_Copy_DI_LTE = make[Double, Int, Boolean, LteOp]
  implicit val vecSclr_Copy_LD_LTE = make[Long, Double, Boolean, LteOp]
  implicit val vecSclr_Copy_LL_LTE = make[Long, Long, Boolean, LteOp]
  implicit val vecSclr_Copy_LI_LTE = make[Long, Int, Boolean, LteOp]
  implicit val vecSclr_Copy_ID_LTE = make[Int, Double, Boolean, LteOp]
  implicit val vecSclr_Copy_II_LTE = make[Int, Int, Boolean, LteOp]
  implicit val vecSclr_Copy_BB_LTE = make[Boolean, Boolean, Boolean, LteOp]

  implicit val vecSclr_Copy_DD_NEQ = make[Double, Double, Boolean, NeqOp]
  implicit val vecSclr_Copy_DL_NEQ = make[Double, Long, Boolean, NeqOp]
  implicit val vecSclr_Copy_DI_NEQ = make[Double, Int, Boolean, NeqOp]
  implicit val vecSclr_Copy_LD_NEQ = make[Long, Double, Boolean, NeqOp]
  implicit val vecSclr_Copy_LL_NEQ = make[Long, Long, Boolean, NeqOp]
  implicit val vecSclr_Copy_LI_NEQ = make[Long, Int, Boolean, NeqOp]
  implicit val vecSclr_Copy_ID_NEQ = make[Int, Double, Boolean, NeqOp]
  implicit val vecSclr_Copy_II_NEQ = make[Int, Int, Boolean, NeqOp]
  implicit val vecSclr_Copy_BB_NEQ = make[Boolean, Boolean, Boolean, NeqOp]

  implicit val vecSclr_Copy_DD_EQ = make[Double, Double, Boolean, EqOp]
  implicit val vecSclr_Copy_DL_EQ = make[Double, Long, Boolean, EqOp]
  implicit val vecSclr_Copy_DI_EQ = make[Double, Int, Boolean, EqOp]
  implicit val vecSclr_Copy_LD_EQ = make[Long, Double, Boolean, EqOp]
  implicit val vecSclr_Copy_LL_EQ = make[Long, Long, Boolean, EqOp]
  implicit val vecSclr_Copy_LI_EQ = make[Long, Int, Boolean, EqOp]
  implicit val vecSclr_Copy_ID_EQ = make[Int, Double, Boolean, EqOp]
  implicit val vecSclr_Copy_II_EQ = make[Int, Int, Boolean, EqOp]
  implicit val vecSclr_Copy_BB_EQ = make[Boolean, Boolean, Boolean, EqOp]

  // Boolean
  implicit val vecSclr_Copy_BB_And = make[Boolean, Boolean, Boolean, AndOp]
  implicit val vecSclr_Copy_BB_Or = make[Boolean, Boolean, Boolean, OrOp]
  implicit val vecSclr_Copy_BB_Xor = make[Boolean, Boolean, Boolean, XorOp]

}
