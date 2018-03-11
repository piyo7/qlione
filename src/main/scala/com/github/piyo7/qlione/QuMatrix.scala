package com.github.piyo7.qlione

import com.github.piyo7.qlione.Complex._
import com.github.piyo7.qlione.Util.RichInt
import com.github.piyo7.qlione._OptNat._

class QuMatrix[A <: _OptNat, B <: _OptNat] private(val map: Map[(Int, Int), Complex])(implicit val vA: _value[A], val vB: _value[B]) {
  def bits[C <: _Nat](implicit eA: A =:= _none, eB: B =:= _0, vC: _value[C]): QuBits[C] = {
    QuBits(QuMatrix[C, _0](map))
  }

  def gate[C <: _Nat](implicit eA: A =:= _none, eB: B =:= _none, vC: _value[C]): QuGate[C] = {
    QuGate(QuMatrix[C, C](map))
  }

  def t: QuMatrix[B, A] = {
    QuMatrix(for {
      ((a, b), v) <- map
    } yield (b, a) -> v.conj)
  }

  def unary_-(): QuMatrix[A, B] = this * -1

  def *(x: Complex): QuMatrix[A, B] = QuMatrix(map.mapValues(_ * x))

  def /(x: Complex): QuMatrix[A, B] = QuMatrix(map.mapValues(_ / x))

  override def equals(that: Any): Boolean = that match {
    case that: QuMatrix[A, B] => this.vA() == that.vA() && this.vB() == that.vB() && (this - that).map.isEmpty
    case _  => false
  }

  def +(that: QuMatrix[A, B]): QuMatrix[A, B] = {
    QuMatrix((map.toSeq ++ that.map.toSeq).groupBy(_._1).mapValues(_.map(_._2).reduce(_ + _)))
  }

  def -(that: QuMatrix[A, B]): QuMatrix[A, B] = this + (-that)

  def *[C <: _OptNat](that: QuMatrix[B, C]): QuMatrix[A, C] = {
    implicit val vC: _value[C] = that.vB

    val thatRows = that.map.groupBy(_._1._1)
    QuMatrix((for {
      ((a, b), v1) <- map.toSeq
      thatRow <- thatRows.get(b).toSeq
      ((_, c), v2) <- thatRow.toSeq
    } yield (a, c) -> v1 * v2).groupBy(_._1).mapValues(_.map(_._2).reduce(_ + _)))
  }

  def x[C <: _Nat, D <: _Nat](that: QuMatrix[C, D])(implicit pAC: _plus[A, C], pBD: _plus[B, D]): QuMatrix[pAC.Out, pBD.Out] = {
    val vC: _value[C] = that.vA
    val vD: _value[D] = that.vB
    implicit val vE: _value[pAC.Out] = pAC.vOut
    implicit val vF: _value[pBD.Out] = pBD.vOut

    QuMatrix(for {
      ((a, b), v1) <- map
      ((c, d), v2) <- that.map
    } yield (a * vC().get.pow2 + c, b * vD().get.pow2 + d) -> v1 * v2)
  }

  override def toString: String = {
    val aBitsSize = vA().getOrElse(map.keys.map(_._1).reduceOption(_ max _).getOrElse(0).bitsSize)
    val bBitsSize = vB().getOrElse(map.keys.map(_._2).reduceOption(_ max _).getOrElse(0).bitsSize)

    (for {
      ((a, b), v) <- map.toSeq.sortBy(_._1)
    } yield {
      s"%+.${QuMatrix.epsilonScale}f %+.${QuMatrix.epsilonScale}f ".format(v.re, v.im) +
        (if (aBitsSize > 0) f"|${a.bitsString(aBitsSize)}>" else "") + (if (bBitsSize > 0) f"<${b.bitsString(bBitsSize)}|" else "")
    }).mkString(" ") + aBitsSize + bBitsSize + map
  }
}

object QuMatrix {
  val epsilonScale: Int = 4
  val epsilon: Double = math.pow(10, -epsilonScale)

  def apply[A <: _OptNat, B <: _OptNat](map: Map[(Int, Int), Complex])(implicit vA: _value[A], vB: _value[B]): QuMatrix[A, B] = {
    for (ai <- vA()) {
      assert(map.keys.map(_._1).reduceOption(_ min _).getOrElse(0) >= 0, map.toString)
      assert(map.keys.map(_._1).reduceOption(_ max _).getOrElse(0) < ai.pow2, map.toString)
    }

    for (bi <- vB()) {
      assert(map.keys.map(_._2).reduceOption(_ min _).getOrElse(0) >= 0, map.toString)
      assert(map.keys.map(_._2).reduceOption(_ max _).getOrElse(0) < bi.pow2, map.toString)
    }

    new QuMatrix(map.filter(_._2.abs >= epsilon))(vA, vB)
  }

  implicit class ComplexMulQuMatrix(val c: Complex) {
    def *[A <: _OptNat, B <: _OptNat](quMatrix: QuMatrix[A, B]): QuMatrix[A, B] = quMatrix * c
  }

}

case class |(i: Int) extends AnyVal {
  def > : QuMatrix[_none, _0] = QuMatrix(Map((i, 0) -> 1))
}

object | {
  def apply(digits: String): | = |(Integer.parseInt(digits, 2))
}

case class <(i: Int) extends AnyVal {
  def | : QuMatrix[_0, _none] = QuMatrix(Map((0, i) -> 1))
}

object < {
  def apply(digits: String): < = <(Integer.parseInt(digits, 2))
}
