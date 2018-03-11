package com.github.piyo7.qlione

import com.github.piyo7.qlione.Complex._
import com.github.piyo7.qlione._OptNat._
import org.scalatest.{FunSuite, Matchers}

class QuMatrixTest extends FunSuite with Matchers {
  test("t") {
    QuMatrix[_1, _2](Map((0, 3) -> (1 - 5.i))).t shouldEqual QuMatrix[_2, _1](Map((3, 0) -> (1 + 5.i)))
  }

  test("unary_-") {
    -QuMatrix[_1, _2](Map((0, 3) -> (1 - 5.i))) shouldEqual QuMatrix[_1, _2](Map((0, 3) -> (-1 + 5.i)))
  }

  test("equals") {
    QuMatrix[_2, _3](Map((1, 4) -> (5 + 6.i))) shouldEqual QuMatrix[_2, _3](Map((1, 4) -> (5 + 6.i)))
    QuMatrix[_2, _3](Map((1, 4) -> (5 + 6.i))) shouldEqual QuMatrix[_2, _3](Map((1, 4) -> (5 + 6.i), (1, 1) -> QuMatrix.epsilon * 0.9))

    QuMatrix[_2, _3](Map((1, 4) -> (5 + 6.i))) shouldNot equal(QuMatrix[_3, _3](Map((1, 4) -> (5 + 6.i))))
    QuMatrix[_2, _3](Map((1, 4) -> (5 + 6.i))) shouldNot equal(QuMatrix[_2, _3](Map((1, 7) -> (5 + 6.i))))
    QuMatrix[_2, _3](Map((1, 4) -> (5 + 6.i))) shouldNot equal(QuMatrix[_2, _3](Map((1, 4) -> (7 + 6.i))))
    QuMatrix[_2, _3](Map((1, 4) -> (5 + 6.i))) shouldNot equal(QuMatrix[_2, _3](Map((1, 4) -> (5 + 6.i), (1, 1) -> QuMatrix.epsilon * 1.1)))
  }

  test("+") {
    QuMatrix[_1, _2](Map((0, 0) -> 1, (0, 3) -> 1)) +
      QuMatrix[_1, _2](Map((0, 0) -> 1.i, (1, 0) -> 1.i)) shouldEqual
      QuMatrix[_1, _2](Map((0, 0) -> (1 + 1.i), (0, 3) -> 1, (1, 0) -> 1.i))
  }

  test("-") {
    QuMatrix[_1, _2](Map((0, 0) -> 1, (0, 3) -> 1)) -
      QuMatrix[_1, _2](Map((0, 0) -> 1.i, (1, 0) -> 1.i)) shouldEqual
      QuMatrix[_1, _2](Map((0, 0) -> (1 - 1.i), (0, 3) -> 1, (1, 0) -> -1.i))
  }

  test("*") {
    QuMatrix[_1, _2](Map((0, 0) -> 1, (0, 2) -> 2, (1, 1) -> 3, (1, 3) -> 4)) *
      QuMatrix[_2, _none](Map((0, 1) -> -4, (2, 1) -> 2, (0, 9) -> 5, (1, 9) -> 6)) shouldEqual
      QuMatrix[_1, _none](Map((0, 9) -> 5, (1, 9) -> 18))

    QuMatrix[_none, _9](Map((0, 1) -> (2 + 3.i), (4, 5) -> (6 + 7.i))) * (1 + 1.i) shouldEqual
      QuMatrix[_none, _9](Map((0, 1) -> (-1 + 5.i), (4, 5) -> (-1 + 13.i)))
  }

  test("/") {
    QuMatrix[_none, _9](Map((0, 1) -> (2 + 3.i), (4, 5) -> (6 + 7.i))) / (1 + 1.i) shouldEqual
      QuMatrix[_none, _9](Map((0, 1) -> (2.5 + 0.5.i), (4, 5) -> (6.5 + 0.5.i)))
  }

  test("x") {
    QuMatrix[_1, _2](Map((0, 2) -> 1, (1, 3) -> 2.i)) x
      QuMatrix[_3, _4](Map((4, 5) -> 3.i, (7, 15) -> 4)) shouldEqual
      QuMatrix[_4, _6](Map((4, 37) -> 3.i, (7, 47) -> 4, (12, 53) -> -6, (15, 63) -> 8.i))
  }

  test("bra-ket notation") {
    // escape from shadowing by Matchers
    import com.github.piyo7.qlione.{< => \<}

    (\<(1).| * |(2).>) shouldEqual QuMatrix[_0, _0](Map.empty)
    (\<(3).| * |(3).>) shouldEqual QuMatrix[_0, _0](Map((0, 0) -> 1.r))
    (|(4).> * \<(5).|) shouldEqual QuMatrix[_none, _none](Map((4, 5) -> 1.r))
  }
}
