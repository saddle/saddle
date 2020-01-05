package org.saddle.macros

import org.saddle.ops._
import org.saddle.ops.macroImpl.BinOpMatMacros.{matMat => MatMat}

trait BinOpMatMatCopy {

  // basic operations
  implicit val matMat_Copy_DD_Add = MatMat[Double, Double, Double, Add]
  implicit val matMat_Copy_LD_Add = MatMat[Long, Double, Double, Add]
  implicit val matMat_Copy_DL_Add = MatMat[Double, Long, Double, Add]
  implicit val matMat_Copy_DI_Add = MatMat[Double, Int, Double, Add]
  implicit val matMat_Copy_ID_Add = MatMat[Int, Double, Double, Add]
  implicit val matMat_Copy_LL_Add = MatMat[Long, Long, Long, Add]
  implicit val matMat_Copy_LI_Add = MatMat[Long, Int, Long, Add]
  implicit val matMat_Copy_IL_Add = MatMat[Int, Long, Long, Add]
  implicit val matMat_Copy_II_Add = MatMat[Int, Int, Int, Add]

  implicit val matMat_Copy_DD_Power = MatMat[Double, Double, Double, Power]
  implicit val matMat_Copy_LD_Power = MatMat[Long, Double, Double, Power]
  implicit val matMat_Copy_DL_Power = MatMat[Double, Long, Double, Power]
  implicit val matMat_Copy_DI_Power = MatMat[Double, Int, Double, Power]
  implicit val matMat_Copy_ID_Power = MatMat[Int, Double, Double, Power]
  implicit val matMat_Copy_LL_Power = MatMat[Long, Long, Long, Power]
  implicit val matMat_Copy_LI_Power = MatMat[Long, Int, Long, Power]
  implicit val matMat_Copy_IL_Power = MatMat[Int, Long, Long, Power]
  implicit val matMat_Copy_II_Power = MatMat[Int, Int, Int, Power]

  implicit val matMat_Copy_DD_Sub = MatMat[Double, Double, Double, Subtract]
  implicit val matMat_Copy_LD_Sub = MatMat[Long, Double, Double, Subtract]
  implicit val matMat_Copy_DL_Sub = MatMat[Double, Long, Double, Subtract]
  implicit val matMat_Copy_DI_Sub = MatMat[Double, Int, Double, Subtract]
  implicit val matMat_Copy_ID_Sub = MatMat[Int, Double, Double, Subtract]
  implicit val matMat_Copy_LL_Sub = MatMat[Long, Long, Long, Subtract]
  implicit val matMat_Copy_LI_Sub = MatMat[Long, Int, Long, Subtract]
  implicit val matMat_Copy_IL_Sub = MatMat[Int, Long, Long, Subtract]
  implicit val matMat_Copy_II_Sub = MatMat[Int, Int, Int, Subtract]

  implicit val matMat_Copy_DD_Mult = MatMat[Double, Double, Double, Multiply]
  implicit val matMat_Copy_LD_Mult = MatMat[Long, Double, Double, Multiply]
  implicit val matMat_Copy_DL_Mult = MatMat[Double, Long, Double, Multiply]
  implicit val matMat_Copy_DI_Mult = MatMat[Double, Int, Double, Multiply]
  implicit val matMat_Copy_ID_Mult = MatMat[Int, Double, Double, Multiply]
  implicit val matMat_Copy_LL_Mult = MatMat[Long, Long, Long, Multiply]
  implicit val matMat_Copy_LI_Mult = MatMat[Long, Int, Long, Multiply]
  implicit val matMat_Copy_IL_Mult = MatMat[Int, Long, Long, Multiply]
  implicit val matMat_Copy_II_Mult = MatMat[Int, Int, Int, Multiply]

  implicit val matMat_Copy_DD_Div = MatMat[Double, Double, Double, Divide]
  implicit val matMat_Copy_LD_Div = MatMat[Long, Double, Double, Divide]
  implicit val matMat_Copy_DL_Div = MatMat[Double, Long, Double, Divide]
  implicit val matMat_Copy_DI_Div = MatMat[Double, Int, Double, Divide]
  implicit val matMat_Copy_ID_Div = MatMat[Int, Double, Double, Divide]
  implicit val matMat_Copy_LL_Div = MatMat[Long, Long, Long, Divide]
  implicit val matMat_Copy_LI_Div = MatMat[Long, Int, Long, Divide]
  implicit val matMat_Copy_IL_Div = MatMat[Int, Long, Long, Divide]
  implicit val matMat_Copy_II_Div = MatMat[Int, Int, Int, Divide]

  implicit val matMat_Copy_DD_Mod = MatMat[Double, Double, Double, Mod]
  implicit val matMat_Copy_LD_Mod = MatMat[Long, Double, Double, Mod]
  implicit val matMat_Copy_DL_Mod = MatMat[Double, Long, Double, Mod]
  implicit val matMat_Copy_DI_Mod = MatMat[Double, Int, Double, Mod]
  implicit val matMat_Copy_ID_Mod = MatMat[Int, Double, Double, Mod]
  implicit val matMat_Copy_LL_Mod = MatMat[Long, Long, Long, Mod]
  implicit val matMat_Copy_LI_Mod = MatMat[Long, Int, Long, Mod]
  implicit val matMat_Copy_IL_Mod = MatMat[Int, Long, Long, Mod]
  implicit val matMat_Copy_II_Mod = MatMat[Int, Int, Int, Mod]

  // bitwise

  implicit val matMat_Copy_LL_BitAnd = MatMat[Long, Long, Long, BitAnd]
  implicit val matMat_Copy_LI_BitAnd = MatMat[Long, Int, Long, BitAnd]
  implicit val matMat_Copy_II_BitAnd = MatMat[Int, Int, Int, BitAnd]

  implicit val matMat_Copy_LL_BitOr = MatMat[Long, Long, Long, BitOr]
  implicit val matMat_Copy_LI_BitOr = MatMat[Long, Int, Long, BitOr]
  implicit val matMat_Copy_II_BitOr = MatMat[Int, Int, Int, BitOr]

  implicit val matMat_Copy_LL_BitXor = MatMat[Long, Long, Long, BitXor]
  implicit val matMat_Copy_LI_BitXor = MatMat[Long, Int, Long, BitXor]
  implicit val matMat_Copy_II_BitXor = MatMat[Int, Int, Int, BitXor]

  implicit val matMat_Copy_LL_BitShl = MatMat[Long, Long, Long, BitShl]
  implicit val matMat_Copy_LI_BitShl = MatMat[Long, Int, Long, BitShl]
  implicit val matMat_Copy_II_BitShl = MatMat[Int, Int, Int, BitShl]

  implicit val matMat_Copy_LL_BitShr = MatMat[Long, Long, Long, BitShr]
  implicit val matMat_Copy_LI_BitShr = MatMat[Long, Int, Long, BitShr]
  implicit val matMat_Copy_II_BitShr = MatMat[Int, Int, Int, BitShr]

  implicit val matMat_Copy_LL_BitUshr = MatMat[Long, Long, Long, BitUShr]
  implicit val matMat_Copy_LI_BitUshr = MatMat[Long, Int, Long, BitUShr]
  implicit val matMat_Copy_II_BitUshr = MatMat[Int, Int, Int, BitUShr]

  // Boolean
  implicit val matMat_Copy_BB_And = MatMat[Boolean, Boolean, Boolean, AndOp]
  implicit val matMat_Copy_BB_Or = MatMat[Boolean, Boolean, Boolean, OrOp]
  implicit val matMat_Copy_BB_Xor = MatMat[Boolean, Boolean, Boolean, XorOp]
}
