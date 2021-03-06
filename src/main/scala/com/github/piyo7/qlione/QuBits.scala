package com.github.piyo7.qlione

import java.util.Random

import com.github.piyo7.qlione.Util.RichInt
import com.github.piyo7.qlione._OptNat._

class QuBits[A <: _Nat] private(val matrix: QuMatrix[A, _0]) {
  override def equals(that: Any): Boolean = that match {
    case that: QuBits[A] => this.matrix equalsWithoutGlobalPhase that.matrix
    case _ => false
  }

  def x[B <: _Nat](that: QuBits[B])(implicit c: _plus[A, B]): QuBits[c.Out] = {
    QuBits(matrix x that.matrix)
  }

  def reverse: QuBits[A] = {
    implicit val vA: _value[A] = matrix.vA

    QuBits(QuMatrix[A, _0](matrix.map.map { case ((a, 0), v) => ((a.reverseBits(vA().get), 0), v) }))
  }

  def measure[M <: _Nat](implicit pA: _pre[A], vM: _value[M], lMA: M _lt A, random: Random): QuBits.Measured[A, pA.Out, M] = {
    implicit val vA: _value[A] = matrix.vA
    implicit val vpA: _value[pA.Out] = pA.vOut

    val grouped = matrix.map.groupBy { case ((a, _), _) => a.bit(vM().get) }
    if (grouped.getOrElse(0, Map()).map(_._2.abs2).sum <= random.nextDouble()) {
      new QuBits.Measured(false, grouped.getOrElse(0, Map()))
    } else {
      new QuBits.Measured(true, grouped.getOrElse(1, Map()))
    }
  }

  def measureAll(implicit random: Random): Int = {
    val candidates = matrix.map.toSeq.map { case ((a, _), v) => (a, v.abs2) }.scanLeft((0, 0.0)) { case (l, r) => (r._1, l._2 + r._2) }.tail
    candidates.find(_._2 >= random.nextDouble()).map(_._1).getOrElse(candidates.last._1)
  }

  def getClassic: Option[Int] = {
    if (matrix.map.size == 1) {
      Some(matrix.map.map(_._1._1).head)
    } else None
  }

  def |>[B](f: QuBits[A] => B): B = f(this)

  def |>>[B](f: QuBits[A] => B): B = {
    println(this)
    |>(f)
  }

  def |>(gate: QuGate[A]): QuBits[A] = |>(gate * _)

  def |>>(gate: QuGate[A]): QuBits[A] = |>>(gate * _)

  override def toString: String = matrix.toString
}

object QuBits {

  class Measured[A <: _Nat, B <: _Nat, M <: _Nat] private[QuBits](val result: Boolean, map: Map[(Int, Int), Complex])(implicit vA: _value[A], vB: _value[B], vM: _value[M]) {
    def bits: QuBits[A] = QuBits(QuMatrix[A, _0](map))

    def unmeasured: QuBits[B] = QuBits(QuMatrix[B, _0](map.map { case ((a, b), v) => ((a.dropBit(vM().get), b), v) }))
  }

  def apply[A <: _Nat](matrix: QuMatrix[A, _0]): QuBits[A] = {
    val abs = math.sqrt(matrix.map.map(_._2.abs2).sum)
    assert(abs > 0, matrix.toString)
    new QuBits(matrix / abs)
  }

  def apply[A <: _Nat](seq: Seq[Complex])(implicit vA: _value[A]): QuBits[A] = {
    val map = seq.zipWithIndex.map { case (v, i) => (i, 0) -> v }.toMap
    apply(QuMatrix[A, _0](map))
  }

  def apply(c0: Complex, c1: Complex): QuBits[_1] =
    apply(Seq(c0, c1))

  def apply(c0: Complex, c1: Complex, c2: Complex, c3: Complex): QuBits[_2] =
    apply(Seq(c0, c1, c2, c3))

  def apply(c0: Complex, c1: Complex, c2: Complex, c3: Complex, c4: Complex, c5: Complex, c6: Complex, c7: Complex): QuBits[_3] =
    apply(Seq(c0, c1, c2, c3, c4, c5, c6, c7))

  val bit0: QuBits[_1] = apply(1, 0)

  val bit1: QuBits[_1] = apply(0, 1)
}
