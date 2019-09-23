package org.saddle

package object linalg extends OpImpl {
  import scala.language.implicitConversions
  implicit def pimp(m: Mat[Double]): MatPimp = new MatPimp(m)
  implicit def pimp(m: Vec[Double]): VecPimp = new VecPimp(m)
}
