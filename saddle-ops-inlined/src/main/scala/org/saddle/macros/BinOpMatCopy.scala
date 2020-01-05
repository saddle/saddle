package org.saddle.macros

import org.saddle.ops._
import org.saddle.ops.macroImpl.BinOpMatMacros.{matScalarElementwise => MatSclr}

trait BinOpMatCopy {

  // basic operations
  implicit val matSclr_Copy_DD_Add = MatSclr[Double, Double, Double, Add]
  implicit val matSclr_Copy_DL_Add = MatSclr[Double, Long, Double, Add]
  implicit val matSclr_Copy_DI_Add = MatSclr[Double, Int, Double, Add]
  implicit val matSclr_Copy_LL_Add = MatSclr[Long, Long, Long, Add]
  implicit val matSclr_Copy_LD_Add = MatSclr[Long, Double, Double, Add]
  implicit val matSclr_Copy_LI_Add = MatSclr[Long, Int, Long, Add]
  implicit val matSclr_Copy_IL_Add = MatSclr[Int, Long, Long, Add]
  implicit val matSclr_Copy_II_Add = MatSclr[Int, Int, Int, Add]
  implicit val matSclr_Copy_ID_Add = MatSclr[Int, Double, Double, Add]

  implicit val matSclr_Copy_DD_Power = MatSclr[Double, Double, Double, Power]
  implicit val matSclr_Copy_DL_Power = MatSclr[Double, Long, Double, Power]
  implicit val matSclr_Copy_DI_Power = MatSclr[Double, Int, Double, Power]
  implicit val matSclr_Copy_LL_Power = MatSclr[Long, Long, Long, Power]
  implicit val matSclr_Copy_LD_Power = MatSclr[Long, Double, Double, Power]
  implicit val matSclr_Copy_LI_Power = MatSclr[Long, Int, Long, Power]
  implicit val matSclr_Copy_IL_Power = MatSclr[Int, Long, Long, Power]
  implicit val matSclr_Copy_II_Power = MatSclr[Int, Int, Int, Power]
  implicit val matSclr_Copy_ID_Power = MatSclr[Int, Double, Double, Power]

  implicit val matSclr_Copy_DD_Sub = MatSclr[Double, Double, Double, Subtract]
  implicit val matSclr_Copy_DL_Sub = MatSclr[Double, Long, Double, Subtract]
  implicit val matSclr_Copy_DI_Sub = MatSclr[Double, Int, Double, Subtract]
  implicit val matSclr_Copy_LL_Sub = MatSclr[Long, Long, Long, Subtract]
  implicit val matSclr_Copy_LD_Sub = MatSclr[Long, Double, Double, Subtract]
  implicit val matSclr_Copy_LI_Sub = MatSclr[Long, Int, Long, Subtract]
  implicit val matSclr_Copy_IL_Sub = MatSclr[Int, Long, Long, Subtract]
  implicit val matSclr_Copy_II_Sub = MatSclr[Int, Int, Int, Subtract]
  implicit val matSclr_Copy_ID_Sub = MatSclr[Int, Double, Double, Subtract]

  implicit val matSclr_Copy_DD_Mult = MatSclr[Double, Double, Double, Multiply]
  implicit val matSclr_Copy_DL_Mult = MatSclr[Double, Long, Double, Multiply]
  implicit val matSclr_Copy_DI_Mult = MatSclr[Double, Int, Double, Multiply]
  implicit val matSclr_Copy_LL_Mult = MatSclr[Long, Long, Long, Multiply]
  implicit val matSclr_Copy_LD_Mult = MatSclr[Long, Double, Double, Multiply]
  implicit val matSclr_Copy_LI_Mult = MatSclr[Long, Int, Long, Multiply]
  implicit val matSclr_Copy_IL_Mult = MatSclr[Int, Long, Long, Multiply]
  implicit val matSclr_Copy_II_Mult = MatSclr[Int, Int, Int, Multiply]
  implicit val matSclr_Copy_ID_Mult = MatSclr[Int, Double, Double, Multiply]

  implicit val matSclr_Copy_DD_Div = MatSclr[Double, Double, Double, Divide]
  implicit val matSclr_Copy_DL_Div = MatSclr[Double, Long, Double, Divide]
  implicit val matSclr_Copy_DI_Div = MatSclr[Double, Int, Double, Divide]
  implicit val matSclr_Copy_LL_Div = MatSclr[Long, Long, Long, Divide]
  implicit val matSclr_Copy_LD_Div = MatSclr[Long, Double, Double, Divide]
  implicit val matSclr_Copy_LI_Div = MatSclr[Long, Int, Long, Divide]
  implicit val matSclr_Copy_IL_Div = MatSclr[Int, Long, Long, Divide]
  implicit val matSclr_Copy_II_Div = MatSclr[Int, Int, Int, Divide]
  implicit val matSclr_Copy_ID_Div = MatSclr[Int, Double, Double, Divide]

  implicit val matSclr_Copy_DD_Mod = MatSclr[Double, Double, Double, Mod]
  implicit val matSclr_Copy_DL_Mod = MatSclr[Double, Long, Double, Mod]
  implicit val matSclr_Copy_DI_Mod = MatSclr[Double, Int, Double, Mod]
  implicit val matSclr_Copy_LL_Mod = MatSclr[Long, Long, Long, Mod]
  implicit val matSclr_Copy_LD_Mod = MatSclr[Long, Double, Double, Mod]
  implicit val matSclr_Copy_LI_Mod = MatSclr[Long, Int, Long, Mod]
  implicit val matSclr_Copy_IL_Mod = MatSclr[Int, Long, Long, Mod]
  implicit val matSclr_Copy_II_Mod = MatSclr[Int, Int, Int, Mod]
  implicit val matSclr_Copy_ID_Mod = MatSclr[Int, Double, Double, Mod]

  // bitwise

  implicit val matSclr_Copy_LL_BitAnd = MatSclr[Long, Long, Long, BitAnd]
  implicit val matSclr_Copy_LI_BitAnd = MatSclr[Long, Int, Long, BitAnd]
  implicit val matSclr_Copy_II_BitAnd = MatSclr[Int, Int, Int, BitAnd]

  implicit val matSclr_Copy_LL_BitOr = MatSclr[Long, Long, Long, BitOr]
  implicit val matSclr_Copy_LI_BitOr = MatSclr[Long, Int, Long, BitOr]
  implicit val matSclr_Copy_II_BitOr = MatSclr[Int, Int, Int, BitOr]

  implicit val matSclr_Copy_LL_BitXor = MatSclr[Long, Long, Long, BitXor]
  implicit val matSclr_Copy_LI_BitXor = MatSclr[Long, Int, Long, BitXor]
  implicit val matSclr_Copy_II_BitXor = MatSclr[Int, Int, Int, BitXor]

  implicit val matSclr_Copy_LL_BitShl = MatSclr[Long, Long, Long, BitShl]
  implicit val matSclr_Copy_LI_BitShl = MatSclr[Long, Int, Long, BitShl]
  implicit val matSclr_Copy_II_BitShl = MatSclr[Int, Int, Int, BitShl]

  implicit val matSclr_Copy_LL_BitShr = MatSclr[Long, Long, Long, BitShr]
  implicit val matSclr_Copy_LI_BitShr = MatSclr[Long, Int, Long, BitShr]
  implicit val matSclr_Copy_II_BitShr = MatSclr[Int, Int, Int, BitShr]

  implicit val matSclr_Copy_LL_BitUshr = MatSclr[Long, Long, Long, BitUShr]
  implicit val matSclr_Copy_LI_BitUshr = MatSclr[Long, Int, Long, BitUShr]
  implicit val matSclr_Copy_II_BitUshr = MatSclr[Int, Int, Int, BitUShr]

  // comparison

  implicit val matSclr_Copy_DD_GT = MatSclr[Double, Double, Boolean, GtOp]
  implicit val matSclr_Copy_DL_GT = MatSclr[Double, Long, Boolean, GtOp]
  implicit val matSclr_Copy_DI_GT = MatSclr[Double, Int, Boolean, GtOp]
  implicit val matSclr_Copy_LD_GT = MatSclr[Long, Double, Boolean, GtOp]
  implicit val matSclr_Copy_LL_GT = MatSclr[Long, Long, Boolean, GtOp]
  implicit val matSclr_Copy_LI_GT = MatSclr[Long, Int, Boolean, GtOp]
  implicit val matSclr_Copy_ID_GT = MatSclr[Int, Double, Boolean, GtOp]
  implicit val matSclr_Copy_II_GT = MatSclr[Int, Int, Boolean, GtOp]
  implicit val matSclr_Copy_BB_GT = MatSclr[Boolean, Boolean, Boolean, GtOp]

  implicit val matSclr_Copy_DD_GTE = MatSclr[Double, Double, Boolean, GteOp]
  implicit val matSclr_Copy_DL_GTE = MatSclr[Double, Long, Boolean, GteOp]
  implicit val matSclr_Copy_DI_GTE = MatSclr[Double, Int, Boolean, GteOp]
  implicit val matSclr_Copy_LD_GTE = MatSclr[Long, Double, Boolean, GteOp]
  implicit val matSclr_Copy_LL_GTE = MatSclr[Long, Long, Boolean, GteOp]
  implicit val matSclr_Copy_LI_GTE = MatSclr[Long, Int, Boolean, GteOp]
  implicit val matSclr_Copy_ID_GTE = MatSclr[Int, Double, Boolean, GteOp]
  implicit val matSclr_Copy_II_GTE = MatSclr[Int, Int, Boolean, GteOp]
  implicit val matSclr_Copy_BB_GTE = MatSclr[Boolean, Boolean, Boolean, GteOp]

  implicit val matSclr_Copy_DD_LT = MatSclr[Double, Double, Boolean, LtOp]
  implicit val matSclr_Copy_DL_LT = MatSclr[Double, Long, Boolean, LtOp]
  implicit val matSclr_Copy_DI_LT = MatSclr[Double, Int, Boolean, LtOp]
  implicit val matSclr_Copy_LD_LT = MatSclr[Long, Double, Boolean, LtOp]
  implicit val matSclr_Copy_LL_LT = MatSclr[Long, Long, Boolean, LtOp]
  implicit val matSclr_Copy_LI_LT = MatSclr[Long, Int, Boolean, LtOp]
  implicit val matSclr_Copy_ID_LT = MatSclr[Int, Double, Boolean, LtOp]
  implicit val matSclr_Copy_II_LT = MatSclr[Int, Int, Boolean, LtOp]
  implicit val matSclr_Copy_BB_LT = MatSclr[Boolean, Boolean, Boolean, LtOp]

  implicit val matSclr_Copy_DD_LTE = MatSclr[Double, Double, Boolean, LteOp]
  implicit val matSclr_Copy_DL_LTE = MatSclr[Double, Long, Boolean, LteOp]
  implicit val matSclr_Copy_DI_LTE = MatSclr[Double, Int, Boolean, LteOp]
  implicit val matSclr_Copy_LD_LTE = MatSclr[Long, Double, Boolean, LteOp]
  implicit val matSclr_Copy_LL_LTE = MatSclr[Long, Long, Boolean, LteOp]
  implicit val matSclr_Copy_LI_LTE = MatSclr[Long, Int, Boolean, LteOp]
  implicit val matSclr_Copy_ID_LTE = MatSclr[Int, Double, Boolean, LteOp]
  implicit val matSclr_Copy_II_LTE = MatSclr[Int, Int, Boolean, LteOp]
  implicit val matSclr_Copy_BB_LTE = MatSclr[Boolean, Boolean, Boolean, LteOp]

  implicit val matSclr_Copy_DD_NEQ = MatSclr[Double, Double, Boolean, NeqOp]
  implicit val matSclr_Copy_DL_NEQ = MatSclr[Double, Long, Boolean, NeqOp]
  implicit val matSclr_Copy_DI_NEQ = MatSclr[Double, Int, Boolean, NeqOp]
  implicit val matSclr_Copy_LD_NEQ = MatSclr[Long, Double, Boolean, NeqOp]
  implicit val matSclr_Copy_LL_NEQ = MatSclr[Long, Long, Boolean, NeqOp]
  implicit val matSclr_Copy_LI_NEQ = MatSclr[Long, Int, Boolean, NeqOp]
  implicit val matSclr_Copy_ID_NEQ = MatSclr[Int, Double, Boolean, NeqOp]
  implicit val matSclr_Copy_II_NEQ = MatSclr[Int, Int, Boolean, NeqOp]
  implicit val matSclr_Copy_BB_NEQ = MatSclr[Boolean, Boolean, Boolean, NeqOp]

  implicit val matSclr_Copy_DD_EQ = MatSclr[Double, Double, Boolean, EqOp]
  implicit val matSclr_Copy_DL_EQ = MatSclr[Double, Long, Boolean, EqOp]
  implicit val matSclr_Copy_DI_EQ = MatSclr[Double, Int, Boolean, EqOp]
  implicit val matSclr_Copy_LD_EQ = MatSclr[Long, Double, Boolean, EqOp]
  implicit val matSclr_Copy_LL_EQ = MatSclr[Long, Long, Boolean, EqOp]
  implicit val matSclr_Copy_LI_EQ = MatSclr[Long, Int, Boolean, EqOp]
  implicit val matSclr_Copy_ID_EQ = MatSclr[Int, Double, Boolean, EqOp]
  implicit val matSclr_Copy_II_EQ = MatSclr[Int, Int, Boolean, EqOp]
  implicit val matSclr_Copy_BB_EQ = MatSclr[Boolean, Boolean, Boolean, EqOp]

  // Boolean
  implicit val matSclr_Copy_BB_And = MatSclr[Boolean, Boolean, Boolean, AndOp]
  implicit val matSclr_Copy_BB_Or = MatSclr[Boolean, Boolean, Boolean, OrOp]
  implicit val matSclr_Copy_BB_Xor = MatSclr[Boolean, Boolean, Boolean, XorOp]

}
