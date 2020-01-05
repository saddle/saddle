package org.saddle.macros

object BinOps
    extends BinOpMatInPlace
    with BinOpMatCopy
    with BinOpMatMatInPlace
    with BinOpMatMatCopy
    with BinOpVecInPlace
    with BinOpVecVecInPlace
    with BinOpVecCopy
    with BinOpVecVecCopy
    with org.saddle.ops.BinOpFrame
    with org.saddle.ops.BinOpSeries
