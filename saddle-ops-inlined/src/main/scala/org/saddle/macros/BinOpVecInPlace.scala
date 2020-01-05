package org.saddle.macros

import org.saddle.ops._
import org.saddle.ops.macroImpl.BinOpVecMacros.{
  vecScalarElementwiseIP => VecSclrIP
}

trait BinOpVecInPlace {

  // basic operations
  implicit val vecSclr_DD_Add = VecSclrIP[Double, Double, Add]
  implicit val vecSclr_DL_Add = VecSclrIP[Double, Long, Add]
  implicit val vecSclr_DI_Add = VecSclrIP[Double, Int, Add]
  implicit val vecSclr_LL_Add = VecSclrIP[Long, Long, Add]
  implicit val vecSclr_LI_Add = VecSclrIP[Long, Int, Add]
  implicit val vecSclr_II_Add = VecSclrIP[Int, Int, Add]

  implicit val vecSclr_DD_Power = VecSclrIP[Double, Double, Power]
  implicit val vecSclr_DL_Power = VecSclrIP[Double, Long, Power]
  implicit val vecSclr_DI_Power = VecSclrIP[Double, Int, Power]
  implicit val vecSclr_LL_Power = VecSclrIP[Long, Long, Power]
  implicit val vecSclr_LI_Power = VecSclrIP[Long, Int, Power]
  implicit val vecSclr_II_Power = VecSclrIP[Int, Int, Power]

  implicit val vecSclr_DD_Sub = VecSclrIP[Double, Double, Subtract]
  implicit val vecSclr_DL_Sub = VecSclrIP[Double, Long, Subtract]
  implicit val vecSclr_DI_Sub = VecSclrIP[Double, Int, Subtract]
  implicit val vecSclr_LL_Sub = VecSclrIP[Long, Long, Subtract]
  implicit val vecSclr_LI_Sub = VecSclrIP[Long, Int, Subtract]
  implicit val vecSclr_II_Sub = VecSclrIP[Int, Int, Subtract]

  implicit val vecSclr_DD_Mult = VecSclrIP[Double, Double, Multiply]
  implicit val vecSclr_DL_Mult = VecSclrIP[Double, Long, Multiply]
  implicit val vecSclr_DI_Mult = VecSclrIP[Double, Int, Multiply]
  implicit val vecSclr_LL_Mult = VecSclrIP[Long, Long, Multiply]
  implicit val vecSclr_LI_Mult = VecSclrIP[Long, Int, Multiply]
  implicit val vecSclr_II_Mult = VecSclrIP[Int, Int, Multiply]

  implicit val vecSclr_DD_Div = VecSclrIP[Double, Double, Divide]
  implicit val vecSclr_DL_Div = VecSclrIP[Double, Long, Divide]
  implicit val vecSclr_DI_Div = VecSclrIP[Double, Int, Divide]
  implicit val vecSclr_LL_Div = VecSclrIP[Long, Long, Divide]
  implicit val vecSclr_LI_Div = VecSclrIP[Long, Int, Divide]
  implicit val vecSclr_II_Div = VecSclrIP[Int, Int, Divide]

  implicit val vecSclr_DD_Mod = VecSclrIP[Double, Double, Mod]
  implicit val vecSclr_DL_Mod = VecSclrIP[Double, Long, Mod]
  implicit val vecSclr_DI_Mod = VecSclrIP[Double, Int, Mod]
  implicit val vecSclr_LL_Mod = VecSclrIP[Long, Long, Mod]
  implicit val vecSclr_LI_Mod = VecSclrIP[Long, Int, Mod]
  implicit val vecSclr_II_Mod = VecSclrIP[Int, Int, Mod]

  // bitwise

  implicit val vecSclr_LL_BitAnd = VecSclrIP[Long, Long, BitAnd]
  implicit val vecSclr_LI_BitAnd = VecSclrIP[Long, Int, BitAnd]
  implicit val vecSclr_II_BitAnd = VecSclrIP[Int, Int, BitAnd]

  implicit val vecSclr_LL_BitOr = VecSclrIP[Long, Long, BitOr]
  implicit val vecSclr_LI_BitOr = VecSclrIP[Long, Int, BitOr]
  implicit val vecSclr_II_BitOr = VecSclrIP[Int, Int, BitOr]

  implicit val vecSclr_LL_BitXor = VecSclrIP[Long, Long, BitXor]
  implicit val vecSclr_LI_BitXor = VecSclrIP[Long, Int, BitXor]
  implicit val vecSclr_II_BitXor = VecSclrIP[Int, Int, BitXor]

  implicit val vecSclr_LL_BitShl = VecSclrIP[Long, Long, BitShl]
  implicit val vecSclr_LI_BitShl = VecSclrIP[Long, Int, BitShl]
  implicit val vecSclr_II_BitShl = VecSclrIP[Int, Int, BitShl]

  implicit val vecSclr_LL_BitShr = VecSclrIP[Long, Long, BitShr]
  implicit val vecSclr_LI_BitShr = VecSclrIP[Long, Int, BitShr]
  implicit val vecSclr_II_BitShr = VecSclrIP[Int, Int, BitShr]

  implicit val vecSclr_LL_BitUshr = VecSclrIP[Long, Long, BitUShr]
  implicit val vecSclr_LI_BitUshr = VecSclrIP[Long, Int, BitUShr]
  implicit val vecSclr_II_BitUshr = VecSclrIP[Int, Int, BitUShr]

  // comparison

  implicit val vecSclr_BB_GT = VecSclrIP[Boolean, Boolean, GtOp]

  implicit val vecSclr_BB_GTE = VecSclrIP[Boolean, Boolean, GteOp]

  implicit val vecSclr_BB_LT = VecSclrIP[Boolean, Boolean, LtOp]

  implicit val vecSclr_BB_LTE = VecSclrIP[Boolean, Boolean, LteOp]

  implicit val vecSclr_BB_NEQ = VecSclrIP[Boolean, Boolean, NeqOp]

  implicit val vecSclr_BB_EQ = VecSclrIP[Boolean, Boolean, EqOp]

  // Boolean
  implicit val vecSclr_BB_And = VecSclrIP[Boolean, Boolean, AndOp]
  implicit val vecSclr_BB_Or = VecSclrIP[Boolean, Boolean, OrOp]
  implicit val vecSclr_BB_Xor = VecSclrIP[Boolean, Boolean, XorOp]

}
