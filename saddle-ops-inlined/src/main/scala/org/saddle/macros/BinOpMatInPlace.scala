package org.saddle.macros

import org.saddle.ops._
import org.saddle.ops.macroImpl.BinOpMatMacros.{
  matScalarElementwiseIP => MatSclrIP
}

trait BinOpMatInPlace {

  // basic operations
  implicit val matSclr_DD_Add = MatSclrIP[Double, Double, Add]
  implicit val matSclr_DL_Add = MatSclrIP[Double, Long, Add]
  implicit val matSclr_DI_Add = MatSclrIP[Double, Int, Add]
  implicit val matSclr_LL_Add = MatSclrIP[Long, Long, Add]
  implicit val matSclr_LI_Add = MatSclrIP[Long, Int, Add]
  implicit val matSclr_II_Add = MatSclrIP[Int, Int, Add]

  implicit val matSclr_DD_Power = MatSclrIP[Double, Double, Power]
  implicit val matSclr_DL_Power = MatSclrIP[Double, Long, Power]
  implicit val matSclr_DI_Power = MatSclrIP[Double, Int, Power]
  implicit val matSclr_LL_Power = MatSclrIP[Long, Long, Power]
  implicit val matSclr_LI_Power = MatSclrIP[Long, Int, Power]
  implicit val matSclr_II_Power = MatSclrIP[Int, Int, Power]

  implicit val matSclr_DD_Sub = MatSclrIP[Double, Double, Subtract]
  implicit val matSclr_DL_Sub = MatSclrIP[Double, Long, Subtract]
  implicit val matSclr_DI_Sub = MatSclrIP[Double, Int, Subtract]
  implicit val matSclr_LL_Sub = MatSclrIP[Long, Long, Subtract]
  implicit val matSclr_LI_Sub = MatSclrIP[Long, Int, Subtract]
  implicit val matSclr_II_Sub = MatSclrIP[Int, Int, Subtract]

  implicit val matSclr_DD_Mult = MatSclrIP[Double, Double, Multiply]
  implicit val matSclr_DL_Mult = MatSclrIP[Double, Long, Multiply]
  implicit val matSclr_DI_Mult = MatSclrIP[Double, Int, Multiply]
  implicit val matSclr_LL_Mult = MatSclrIP[Long, Long, Multiply]
  implicit val matSclr_LI_Mult = MatSclrIP[Long, Int, Multiply]
  implicit val matSclr_II_Mult = MatSclrIP[Int, Int, Multiply]

  implicit val matSclr_DD_Div = MatSclrIP[Double, Double, Divide]
  implicit val matSclr_DL_Div = MatSclrIP[Double, Long, Divide]
  implicit val matSclr_DI_Div = MatSclrIP[Double, Int, Divide]
  implicit val matSclr_LL_Div = MatSclrIP[Long, Long, Divide]
  implicit val matSclr_LI_Div = MatSclrIP[Long, Int, Divide]
  implicit val matSclr_II_Div = MatSclrIP[Int, Int, Divide]

  implicit val matSclr_DD_Mod = MatSclrIP[Double, Double, Mod]
  implicit val matSclr_DL_Mod = MatSclrIP[Double, Long, Mod]
  implicit val matSclr_DI_Mod = MatSclrIP[Double, Int, Mod]
  implicit val matSclr_LL_Mod = MatSclrIP[Long, Long, Mod]
  implicit val matSclr_LI_Mod = MatSclrIP[Long, Int, Mod]
  implicit val matSclr_II_Mod = MatSclrIP[Int, Int, Mod]

  // bitwise

  implicit val matSclr_LL_BitAnd = MatSclrIP[Long, Long, BitAnd]
  implicit val matSclr_LI_BitAnd = MatSclrIP[Long, Int, BitAnd]
  implicit val matSclr_II_BitAnd = MatSclrIP[Int, Int, BitAnd]

  implicit val matSclr_LL_BitOr = MatSclrIP[Long, Long, BitOr]
  implicit val matSclr_LI_BitOr = MatSclrIP[Long, Int, BitOr]
  implicit val matSclr_II_BitOr = MatSclrIP[Int, Int, BitOr]

  implicit val matSclr_LL_BitXor = MatSclrIP[Long, Long, BitXor]
  implicit val matSclr_LI_BitXor = MatSclrIP[Long, Int, BitXor]
  implicit val matSclr_II_BitXor = MatSclrIP[Int, Int, BitXor]

  implicit val matSclr_LL_BitShl = MatSclrIP[Long, Long, BitShl]
  implicit val matSclr_LI_BitShl = MatSclrIP[Long, Int, BitShl]
  implicit val matSclr_II_BitShl = MatSclrIP[Int, Int, BitShl]

  implicit val matSclr_LL_BitShr = MatSclrIP[Long, Long, BitShr]
  implicit val matSclr_LI_BitShr = MatSclrIP[Long, Int, BitShr]
  implicit val matSclr_II_BitShr = MatSclrIP[Int, Int, BitShr]

  implicit val matSclr_LL_BitUshr = MatSclrIP[Long, Long, BitUShr]
  implicit val matSclr_LI_BitUshr = MatSclrIP[Long, Int, BitUShr]
  implicit val matSclr_II_BitUshr = MatSclrIP[Int, Int, BitUShr]

  // comparison

  implicit val matSclr_BB_GT = MatSclrIP[Boolean, Boolean, GtOp]

  implicit val matSclr_BB_GTE = MatSclrIP[Boolean, Boolean, GteOp]

  implicit val matSclr_BB_LT = MatSclrIP[Boolean, Boolean, LtOp]

  implicit val matSclr_BB_LTE = MatSclrIP[Boolean, Boolean, LteOp]

  implicit val matSclr_BB_NEQ = MatSclrIP[Boolean, Boolean, NeqOp]

  implicit val matSclr_BB_EQ = MatSclrIP[Boolean, Boolean, EqOp]

  // Bool
  implicit val matSclr_BB_And = MatSclrIP[Boolean, Boolean, AndOp]
  implicit val matSclr_BB_Or = MatSclrIP[Boolean, Boolean, OrOp]
  implicit val matSclr_BB_Xor = MatSclrIP[Boolean, Boolean, XorOp]

}
