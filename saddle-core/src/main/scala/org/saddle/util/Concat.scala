/**
  * Copyright (c) 2013 Saddle Development Team
  *
  * Licensed under the Apache License, Version 2.0 (the "License");
  * you may not use this file except in compliance with the License.
  * You may obtain a copy of the License at
  *
  *     http://www.apache.org/licenses/LICENSE-2.0
  *
  * Unless required by applicable law or agreed to in writing, software
  * distributed under the License is distributed on an "AS IS" BASIS,
  * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  * See the License for the specific language governing permissions and
  * limitations under the License.
 **/
package org.saddle.util

import scala.{specialized => spec}
import org.saddle.{ST, array}
import org.saddle.scalar.{
  ScalarTagByte,
  ScalarTagShort,
  ScalarTagChar,
  ScalarTagInt,
  ScalarTagLong,
  ScalarTagFloat,
  ScalarTagDouble,
  ScalarTagAny
}

/**
  * Machinery for concatenating arrays of differing types, with NA-handling
  */
private[saddle] trait LowPriorityConcatImplicits {
  implicit def waa[T <: Any] =
    new Concat.Promoter[T, T, T](Concat.id, Concat.id)
}

/**
  * Provides a way to append two arrays of possibly different types together by
  * intelligently promoting primitive types where possible.
  *
  * Key method is Concat.append(array1, array2)
  */
object Concat extends LowPriorityConcatImplicits {
  val sy = ScalarTagByte
  val sc = ScalarTagChar
  val ss = ScalarTagShort
  val si = ScalarTagInt
  val sl = ScalarTagLong
  val sf = ScalarTagFloat
  val sd = ScalarTagDouble
  val sr = new ScalarTagAny[AnyRef]

  /**
    * Existence of an instance of this class yields a way to promote instances of the first
    * two types to an instance of the third type. Concrete instances are values held within
    * the Concat object.
    *
    * @param promoteA A function to take A to C
    * @param promoteB A functino to take B to C
    * @tparam A The first type to promote
    * @tparam B The second type to promote
    * @tparam C The joint promotion type
    */
  case class Promoter[
      @spec(Boolean, Byte, Int, Long, Double) -A,
      @spec(Boolean, Byte, Int, Long, Double) -B,
      @spec(Boolean, Byte, Int, Long, Double) +C
  ](promoteA: A => C, promoteB: B => C)

  import scala.language.implicitConversions
  implicit def id[T](a: T): T = a

  // boolean promoting

  implicit val promoteBY: Boolean => Byte = if (_) 1 else 0
  implicit val promoteBC: Boolean => Char = if (_) 1 else 0
  implicit val promoteBS: Boolean => Short = if (_) 1 else 0
  implicit val promoteBI: Boolean => Int = if (_) 1 else 0
  implicit val promoteBL: Boolean => Long = if (_) 1L else 0L
  implicit val promoteBF: Boolean => Float = if (_) 1f else 0f
  implicit val promoteBD: Boolean => Double = if (_) 1d else 0d
  implicit val promoteBA: Boolean => AnyRef = Boolean.box _

  implicit object wbb extends Promoter[Boolean, Boolean, Boolean](id, id)

  implicit object wyb extends Promoter[Boolean, Byte, Byte](promoteBY, id)
  implicit object wby extends Promoter[Byte, Boolean, Byte](id, promoteBY)

  implicit object wcb extends Promoter[Boolean, Char, Char](promoteBC, id)
  implicit object wbc extends Promoter[Char, Boolean, Char](id, promoteBC)

  implicit object wsb extends Promoter[Boolean, Short, Short](promoteBS, id)
  implicit object wbs extends Promoter[Short, Boolean, Short](id, promoteBS)

  implicit object wib extends Promoter[Boolean, Int, Int](promoteBI, id)
  implicit object wbi extends Promoter[Int, Boolean, Int](id, promoteBI)

  implicit object wlb extends Promoter[Boolean, Long, Long](promoteBL, id)
  implicit object wbl extends Promoter[Long, Boolean, Long](id, promoteBL)

  implicit object wfb extends Promoter[Boolean, Float, Float](promoteBF, id)
  implicit object wbf extends Promoter[Float, Boolean, Float](id, promoteBF)

  implicit object wdb extends Promoter[Boolean, Double, Double](promoteBD, id)
  implicit object wbd extends Promoter[Double, Boolean, Double](id, promoteBD)

  implicit object wrb extends Promoter[Boolean, AnyRef, AnyRef](promoteBA, id)
  implicit object wbr extends Promoter[AnyRef, Boolean, AnyRef](id, promoteBA)

  // byte promoting

  implicit val promoteYC: Byte => Char = (y: Byte) =>
    if (sy.isMissing(y)) sc.missing else y.toChar
  implicit val promoteYS: Byte => Short = (y: Byte) =>
    if (sy.isMissing(y)) ss.missing else y.toShort
  implicit val promoteYI: Byte => Int = (y: Byte) =>
    if (sy.isMissing(y)) si.missing else y.toInt
  implicit val promoteYL: Byte => Long = (y: Byte) =>
    if (sy.isMissing(y)) sl.missing else y.toLong
  implicit val promoteYF: Byte => Float = (y: Byte) =>
    if (sy.isMissing(y)) sf.missing else y.toFloat
  implicit val promoteYD: Byte => Double = (y: Byte) =>
    if (sy.isMissing(y)) sd.missing else y.toDouble
  implicit val promoteYR: Byte => AnyRef = (y: Byte) =>
    if (sy.isMissing(y)) sr.missing else Byte.box(y)

  implicit object wyy extends Promoter[Byte, Byte, Byte](id, id)

  implicit object wcy extends Promoter[Byte, Char, Char](promoteYC, id)
  implicit object wyc extends Promoter[Char, Byte, Char](id, promoteYC)

  implicit object wsy extends Promoter[Byte, Short, Short](promoteYS, id)
  implicit object wys extends Promoter[Short, Byte, Short](id, promoteYS)

  implicit object wiy extends Promoter[Byte, Int, Int](promoteYI, id)
  implicit object wyi extends Promoter[Int, Byte, Int](id, promoteYI)

  implicit object wly extends Promoter[Byte, Long, Long](promoteYL, id)
  implicit object wyl extends Promoter[Long, Byte, Long](id, promoteYL)

  implicit object wfy extends Promoter[Byte, Float, Float](promoteYF, id)
  implicit object wyf extends Promoter[Float, Byte, Float](id, promoteYF)

  implicit object wdy extends Promoter[Byte, Double, Double](promoteYD, id)
  implicit object wyd extends Promoter[Double, Byte, Double](id, promoteYD)

  implicit object wry extends Promoter[Byte, AnyRef, AnyRef](promoteYR, id)
  implicit object wyr extends Promoter[AnyRef, Byte, AnyRef](id, promoteYR)

  // char promoting

  implicit val promoteCS: Char => Short = (y: Char) =>
    if (sc.isMissing(y)) ss.missing else y.toShort
  implicit val promoteCI: Char => Int = (y: Char) =>
    if (sc.isMissing(y)) si.missing else y.toInt
  implicit val promoteCL: Char => Long = (y: Char) =>
    if (sc.isMissing(y)) sl.missing else y.toLong
  implicit val promoteCF: Char => Float = (y: Char) =>
    if (sc.isMissing(y)) sf.missing else y.toFloat
  implicit val promoteCD: Char => Double = (y: Char) =>
    if (sc.isMissing(y)) sd.missing else y.toDouble
  implicit val promoteCR: Char => AnyRef = (y: Char) =>
    if (sc.isMissing(y)) sr.missing else Char.box(y)

  implicit object wcc extends Promoter[Char, Char, Char](id, id)

  implicit object wsc extends Promoter[Char, Short, Short](promoteCS, id)
  implicit object wcs extends Promoter[Short, Char, Short](id, promoteCS)

  implicit object wic extends Promoter[Char, Int, Int](promoteCI, id)
  implicit object wci extends Promoter[Int, Char, Int](id, promoteCI)

  implicit object wlc extends Promoter[Char, Long, Long](promoteCL, id)
  implicit object wcl extends Promoter[Long, Char, Long](id, promoteCL)

  implicit object wfc extends Promoter[Char, Float, Float](promoteCF, id)
  implicit object wcf extends Promoter[Float, Char, Float](id, promoteCF)

  implicit object wdc extends Promoter[Char, Double, Double](promoteCD, id)
  implicit object wcd extends Promoter[Double, Char, Double](id, promoteCD)

  implicit object wrc extends Promoter[Char, AnyRef, AnyRef](promoteCR, id)
  implicit object wcr extends Promoter[AnyRef, Char, AnyRef](id, promoteCR)

  // short promoting

  implicit val promoteSI: Short => Int = (y: Short) =>
    if (ss.isMissing(y)) si.missing else y.toInt
  implicit val promoteSL: Short => Long = (y: Short) =>
    if (ss.isMissing(y)) sl.missing else y.toLong
  implicit val promoteSF: Short => Float = (y: Short) =>
    if (ss.isMissing(y)) sf.missing else y.toFloat
  implicit val promoteSD: Short => Double = (y: Short) =>
    if (ss.isMissing(y)) sd.missing else y.toDouble
  implicit val promoteSR: Short => AnyRef = (y: Short) =>
    if (ss.isMissing(y)) sr.missing else Short.box(y)

  implicit object wss extends Promoter[Short, Short, Short](id, id)

  implicit object wis extends Promoter[Short, Int, Int](promoteSI, id)
  implicit object wsi extends Promoter[Int, Short, Int](id, promoteSI)

  implicit object wls extends Promoter[Short, Long, Long](promoteSL, id)
  implicit object wsl extends Promoter[Long, Short, Long](id, promoteSL)

  implicit object wfs extends Promoter[Short, Float, Float](promoteSF, id)
  implicit object wsf extends Promoter[Float, Short, Float](id, promoteSF)

  implicit object wds extends Promoter[Short, Double, Double](promoteSD, id)
  implicit object wsd extends Promoter[Double, Short, Double](id, promoteSD)

  implicit object wrs extends Promoter[Short, AnyRef, AnyRef](promoteSR, id)
  implicit object wsr extends Promoter[AnyRef, Short, AnyRef](id, promoteSR)

  // int promoting

  implicit val promoteIL: Int => Long = (i: Int) =>
    if (si.isMissing(i)) sl.missing else i.toLong
  implicit val promoteIF: Int => Float = (i: Int) =>
    if (si.isMissing(i)) sf.missing else i.toFloat
  implicit val promoteID: Int => Double = (i: Int) =>
    if (si.isMissing(i)) sd.missing else i.toDouble
  implicit val promoteIR: Int => AnyRef = (i: Int) =>
    if (si.isMissing(i)) sr.missing else Int.box(i)

  implicit object wii extends Promoter[Int, Int, Int](id, id)

  implicit object wfi extends Promoter[Int, Float, Float](promoteIF, id)
  implicit object wif extends Promoter[Float, Int, Float](id, promoteIF)

  implicit object wli extends Promoter[Int, Long, Long](promoteIL, id)
  implicit object wil extends Promoter[Long, Int, Long](id, promoteIL)

  implicit object wdi extends Promoter[Int, Double, Double](promoteID, id)
  implicit object wid extends Promoter[Double, Int, Double](id, promoteID)

  implicit object wri extends Promoter[Int, AnyRef, AnyRef](promoteIR, id)
  implicit object wir extends Promoter[AnyRef, Int, AnyRef](id, promoteIR)

  // float promoting

  implicit val promoteFD: Float => Double = (i: Float) =>
    if (sf.isMissing(i)) sd.missing else i.toDouble
  implicit val promoteFR: Float => AnyRef = (i: Float) =>
    if (sf.isMissing(i)) sr.missing else Float.box(i)

  implicit object wff extends Promoter[Float, Float, Float](id, id)

  implicit object wdf extends Promoter[Float, Double, Double](promoteFD, id)
  implicit object wfd extends Promoter[Double, Float, Double](id, promoteFD)

  implicit object wrf extends Promoter[Float, AnyRef, AnyRef](promoteFR, id)
  implicit object wfr extends Promoter[AnyRef, Float, AnyRef](id, promoteFR)

  // long promoting

  implicit val promoteLD: Long => Double = (l: Long) =>
    if (sl.isMissing(l)) sd.missing else l.toDouble
  implicit val promoteLR: Long => AnyRef = (l: Long) =>
    if (sl.isMissing(l)) sr.missing else Long.box(l)

  implicit object wll extends Promoter[Long, Long, Long](id, id)
  implicit object wdl extends Promoter[Long, Double, Double](promoteLD, id)
  implicit object wld extends Promoter[Double, Long, Double](id, promoteLD)
  implicit object wrl extends Promoter[Long, AnyRef, AnyRef](promoteLR, id)
  implicit object wlr extends Promoter[AnyRef, Long, AnyRef](id, promoteLR)

  // double promoting

  implicit val promoteDR: Double => AnyRef = (d: Double) =>
    if (sd.isMissing(d)) sr.missing else Double.box(d)

  implicit object wdd extends Promoter[Double, Double, Double](id, id)
  implicit object wrd extends Promoter[Double, AnyRef, AnyRef](promoteDR, id)
  implicit object wdr extends Promoter[AnyRef, Double, AnyRef](id, promoteDR)

  // AnyRef promotes to itself

  implicit def wrr[T <: AnyRef] = new Promoter[T, T, T](id, id)

  /**
    * Append two arrays of possibly different types together by intelligently
    * promoting primitive types where possible.
    *
    * @param a1 First array
    * @param a2 Second array
    * @param wd Evidence of instance of Promoter for involved types
    * @param mc Evidence of ST for result type
    * @tparam A First array type
    * @tparam B Second array type
    * @tparam C Result array type
    */
  def append[
      @spec(Boolean, Byte, Int, Long, Double) A,
      @spec(Boolean, Byte, Int, Long, Double) B,
      @spec(Boolean, Byte, Int, Long, Double) C
  ](
      a1: Array[A],
      a2: Array[B]
  )(implicit wd: Promoter[A, B, C], mc: ST[C]): Array[C] = {
    val result = array.empty[C](a1.length + a2.length)
    var i = 0
    while (i < a1.length) {
      result(i) = wd.promoteA(a1(i))
      i += 1
    }
    var j = 0
    while (j < a2.length) {
      result(i) = wd.promoteB(a2(j))
      i += 1
      j += 1
    }
    result
  }
}
