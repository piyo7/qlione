package com.github.piyo7.qlione

import com.github.piyo7.qlione.Complex._
import com.github.piyo7.qlione.Util.√
import org.scalactic.{Equality, TolerantNumerics}
import org.scalatest.{FunSuite, Matchers}

import scala.math.{log, Pi}

class ComplexTest extends FunSuite with Matchers {
  implicit val doubleEq: Equality[Double] = TolerantNumerics.tolerantDoubleEquality(1e-4)

  implicit def complexEquality(implicit doubleEq: Equality[Double]): Equality[Complex] = {
    (a: Complex, b: Any) =>
      b match {
        case bc: Complex =>
          doubleEq.areEqual(a.re, bc.re) && doubleEq.areEqual(a.im, bc.im)
        case _ => false
      }
  }

  test("unary_-") {
    -(1 - 2.i) should equal (-1 + 2.i)
  }

  test("+") {
    ((1.2 + 3.4.i) + (-5.6 + 7.8.i)) should equal (-4.4 + 11.2.i)
  }

  test("-") {
    ((1.2 + 3.4.i) - (-5.6 + 7.8.i)) should equal (6.8 - 4.4.i)
  }

  test("*") {
    ((√(2) - √(2).i) * (1 + √(3).i)) should equal (√(6) + √(2) + √(6).i - √(2).i)
  }

  test("/") {
    ((√(2) - √(2).i) / (1 + √(3).i)) should equal ((- √(6) + √(2) - √(6).i - √(2).i) / 4)
  }

  test("conj") {
    (1 - 2.i).conj should equal (1 + 2.i)
  }

  test("abs2") {
    (3 + 4.i).abs2 should equal (25.0)
  }

  test("abs") {
    (3 + 4.i).abs should equal (5.0)
  }

  test("exp") {
    (log(8) + Pi.i / 6).exp should equal (4 * √(3) + 4.i)
  }

  test("rad") {
    (√(2) + √(2).i).rad should equal (Pi / 4)
  }

  test("deg") {
    (√(2) + √(2).i).deg should equal (45.0)
  }
}
