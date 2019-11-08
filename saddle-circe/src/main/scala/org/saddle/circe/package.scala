package org.saddle

import _root_.io.circe.{Encoder, Decoder}

package object circe {

  implicit def indexEncoder[T: Encoder]: Encoder[Index[T]] =
    Encoder.encodeSeq[T].contramap(_.toSeq)
  implicit def indexDecoder[T: Decoder: ST: ORD]: Decoder[Index[T]] =
    Decoder.decodeSeq[T].map(v => Index(v: _*))

  implicit def vecEncoder[T: Encoder]: Encoder[Vec[T]] =
    Encoder.encodeSeq[T].contramap(_.toSeq)
  implicit def vecDecoder[T: Decoder: ST]: Decoder[Vec[T]] =
    Decoder.decodeSeq[T].map(v => Vec(v: _*))

  implicit def matEncoder[T: Encoder]: Encoder[Mat[T]] =
    implicitly[Encoder[(Int, Int, Vec[T])]]
      .contramap(m => (m.numRows, m.numCols, m.toVec))
  implicit def matDecoder[T: Decoder: ST]: Decoder[Mat[T]] =
    implicitly[Decoder[(Int, Int, Vec[T])]]
      .map(v => Mat(v._1, v._2, v._3.toArray))

  implicit def seriesEncoder[T: Encoder, I: Encoder]: Encoder[Series[I, T]] =
    implicitly[Encoder[(Index[I], Vec[T])]].contramap(v => (v.index, v.values))

  implicit def seriesDecoder[T: Decoder: ST, I: Decoder: ST: ORD]
      : Decoder[Series[I, T]] =
    implicitly[Decoder[(Index[I], Vec[T])]].map(v => Series(v._1, v._2))

  implicit def frameEncoder[T: Encoder, RX: Encoder, CX: Encoder]
      : Encoder[Frame[RX, CX, T]] =
    implicitly[Encoder[(Index[RX], Index[CX], Seq[Vec[T]])]]
      .contramap(v => (v.rowIx, v.colIx, v.values))

  implicit def frameDecoder[
      T: Decoder: ST,
      RX: Decoder: ST: ORD,
      CX: Decoder: ST: ORD
  ]: Decoder[Frame[RX, CX, T]] =
    implicitly[Decoder[(Index[RX], Index[CX], Seq[Vec[T]])]]
      .map(v => Frame(v._3, v._1, v._2))

}
