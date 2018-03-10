package com.github.piyo7.qlione

import com.github.piyo7.qlione._OptNat._
import org.scalatest.{FunSuite, Matchers}

class _OptNatTest extends FunSuite with Matchers {
  test("_value") {
    implicitly[_value[_none]].value should equal(None)
    implicitly[_value[_0]].value should equal(Some(0))
    implicitly[_value[_9]].value should equal(Some(9))
  }

  test("_lt") {
    assertCompiles("implicitly[_0 _lt _3]")
    assertCompiles("implicitly[_2 _lt _7]")

    assertDoesNotCompile("implicitly[_none _lt _5]")
    assertDoesNotCompile("implicitly[_1 _lt _none]")
    assertDoesNotCompile("implicitly[_0 _lt _0]")
    assertDoesNotCompile("implicitly[_7 _lt _0]")
    assertDoesNotCompile("implicitly[_2 _lt _2]")
    assertDoesNotCompile("implicitly[_9 _lt _2]")
  }

  test("_gt") {
    assertCompiles("implicitly[_3 _gt _0]")
    assertCompiles("implicitly[_7 _gt _2]")

    assertDoesNotCompile("implicitly[_none _gt _5]")
    assertDoesNotCompile("implicitly[_1 _gt _none]")
    assertDoesNotCompile("implicitly[_0 _gt _0]")
    assertDoesNotCompile("implicitly[_0 _gt _7]")
    assertDoesNotCompile("implicitly[_2 _gt _2]")
    assertDoesNotCompile("implicitly[_2 _gt _9]")
  }

  test("_pre") {
    implicitly[_pre[_1]].vOut() should equal(Some(0))
    implicitly[_pre[_7]].vOut() should equal(Some(6))
  }

  test("_plus") {
    implicitly[_plus[_1, _2]].vOut() should equal(Some(3))
    implicitly[_plus[_5, _0]].vOut() should equal(Some(5))
  }
}
