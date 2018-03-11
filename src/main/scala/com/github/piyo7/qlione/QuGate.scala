package com.github.piyo7.qlione

import com.github.piyo7.qlione.Complex._
import com.github.piyo7.qlione.Util.RichInt
import com.github.piyo7.qlione._OptNat._

class QuGate[A <: _Nat] private(val matrix: QuMatrix[A, A]) {
  override def equals(that: Any): Boolean = that match {
    case that: QuGate[A] => this.matrix equalsWithoutGlobalPhase that.matrix
    case _ => false
  }

  def t: QuGate[A] = {
    QuGate(matrix.t)
  }

  def *(that: QuBits[A]): QuBits[A] = {
    QuBits(matrix * that.matrix)
  }

  def *(that: QuGate[A]): QuGate[A] = {
    QuGate(matrix * that.matrix)
  }

  def x[B <: _Nat](that: QuGate[B])(implicit pAB: _plus[A, B]): QuGate[pAB.Out] = {
    QuGate(matrix x that.matrix)
  }

  def |>(that: QuGate[A]): QuGate[A] = that * this

  override def toString: String = matrix.toString
}

object QuGate {
  def apply[A <: _Nat](matrix: QuMatrix[A, A]): QuGate[A] = {
    implicit val vA: _value[A] = matrix.vA

    val expectedConstantI = matrix * matrix.t
    assert(expectedConstantI.map.keys.forall(ab => ab._1 == ab._2), matrix.toString)

    val expectedConstant = expectedConstantI.map(0, 0)
    assert(expectedConstantI == (expectedConstant * QuMatrix((0 until vA().get.pow2).map(a => (a, a) -> 1.r).toMap)), matrix.toString)

    new QuGate(matrix / math.sqrt(expectedConstant.abs))
  }

  def apply[A <: _Nat](seqSeq: Seq[Seq[Complex]])(implicit vA: _value[A]): QuGate[A] = {
    val map = (for {
      (seq, a) <- seqSeq.zipWithIndex
      (v, b) <- seq.zipWithIndex
    } yield (a, b) -> v).toMap
    apply(QuMatrix[A, A](map))
  }

  private type C2 = (Complex, Complex)

  def apply(cs0: C2, cs1: C2): QuGate[_1] =
    apply(Seq(cs0, cs1).map(_.productIterator.toSeq.map(_.asInstanceOf[Complex])))

  private type C4 = (Complex, Complex, Complex, Complex)

  def apply(cs0: C4, cs1: C4, cs2: C4, cs3: C4): QuGate[_2] =
    apply(Seq(cs0, cs1, cs2, cs3).map(_.productIterator.toSeq.map(_.asInstanceOf[Complex])))

  private type C8 = (Complex, Complex, Complex, Complex, Complex, Complex, Complex, Complex)

  def apply(cs0: C8, cs1: C8, cs2: C8, cs3: C8, cs4: C8, cs5: C8, cs6: C8, cs7: C8): QuGate[_2] =
    QuGate(Seq(cs0, cs1, cs2, cs3, cs4, cs5, cs6, cs7).map(_.productIterator.toSeq.map(_.asInstanceOf[Complex])))

  val I: QuGate[_1] = QuGate(
    (1, 0),
    (0, 1))

  def I[A <: _Nat](implicit vA: _value[A]): QuGate[A] = {
    QuGate(QuMatrix((0 until vA().get.pow2).map(a => (a, a) -> 1.r).toMap))
  }

  val X: QuGate[_1] = QuGate(
    (0, 1),
    (1, 0))

  val Y: QuGate[_1] = QuGate(
    (0, -1.i),
    (1.i, 0))

  val Z: QuGate[_1] = QuGate(
    (1, 0),
    (0, -1))

  def Rx(radian: Double): QuGate[_1] = QuGate(
    (math.cos(radian / 2), -math.sin(radian / 2).i),
    (-math.sin(radian / 2).i, math.cos(radian / 2)))

  def Ry(radian: Double): QuGate[_1] = QuGate(
    (math.cos(radian / 2), -math.sin(radian / 2)),
    (math.sin(radian / 2), math.cos(radian / 2)))

  def Rz(radian: Double): QuGate[_1] = QuGate(
    (1, 0),
    (0, radian.i.exp))

  val H: QuGate[_1] = QuGate(
    (1, 1),
    (1, -1))

  val SWAP: QuGate[_2] = QuGate(
    (1, 0, 0, 0),
    (0, 0, 1, 0),
    (0, 1, 0, 0),
    (0, 0, 0, 1))

  def C[A <: _Nat](gate: QuGate[A])(implicit p1A: _plus[_1, A]): QuGate[p1A.Out] = {
    implicit val vA: _value[A] = gate.matrix.vA
    implicit val vp1A: _value[p1A.Out] = p1A.vOut

    QuGate((QuMatrix[_1, _1](Map((0, 0) -> 1)) x I[A].matrix) + (QuMatrix[_1, _1](Map((1, 1) -> 1)) x gate.matrix))
  }

  implicit class QuGateC[A <: _Nat](val gate: QuGate[A]) {
    def C(implicit pA1: _plus[A, _1]): QuGate[pA1.Out] = {
      implicit val vA: _value[A] = gate.matrix.vA
      implicit val vpA1: _value[pA1.Out] = pA1.vOut

      QuGate((I[A].matrix x QuMatrix[_1, _1](Map((0, 0) -> 1))) + (gate.matrix x QuMatrix[_1, _1](Map((1, 1) -> 1))))
    }
  }

}
