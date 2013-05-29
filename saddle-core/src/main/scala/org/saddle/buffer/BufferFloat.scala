package org.saddle.buffer

import it.unimi.dsi.fastutil.floats.FloatArrays
import org.saddle.Buffer

class BufferFloat(sz: Int = Buffer.INIT_CAPACITY) extends Buffer[Float] {

  var list = Array.ofDim[Float](sz)
  var count = 0
  var remain = sz

  def apply(loc: Int) = list(loc)

  def add(i: Float) {
    if (remain == 0) {
      remain = list.length
      list = FloatArrays.setLength(list, remain * 2)
    }

    list(count) = i
    count += 1
    remain -= 1
  }

  def toArray: Array[Float] = FloatArrays.copy(list, 0, count)
}

object BufferFloat {
  def apply(sz: Int) = new BufferFloat(sz)
  def apply() = new BufferFloat()
}
