package com.github.piyo7.qlione

import com.github.piyo7.qlione.Complex._
import com.github.piyo7.qlione.QuGate._
import com.github.piyo7.qlione._OptNat._
import org.scalatest.{FunSuite, Matchers}

import scala.math.Pi

class QuGateTest extends FunSuite with Matchers {
  test("pauli gates") {
    X * X shouldEqual I
    Y * Y shouldEqual I
    Z * Z shouldEqual I

    X * Y shouldEqual Z
    Y * Z shouldEqual X
    Z * X shouldEqual Y

    Y * X shouldEqual Z
    Z * Y shouldEqual X
    X * Z shouldEqual Y
  }

  test("phase shift gates") {
    Rx(Pi) shouldEqual X
    Ry(Pi) shouldEqual Y
    Rz(Pi) shouldEqual Z

    Rx(Pi / 2) * (|(0).> - |(1).>) shouldEqual (|(0).> - |(1).>).bit
    Ry(Pi / 2) * (|(0).> - |(1).>) shouldEqual |(0).>.bit
    Rz(Pi / 2) * (|(0).> - |(1).>) shouldEqual (|(0).> - 1.i * |(1).>).bit
  }

  test("hadamard gate") {
    H * |(0).> shouldEqual (|(0).> + |(1).>).bit
    H * |(1).> shouldEqual (|(0).> - |(1).>).bit
  }

  test("swap gate") {
    SWAP * (|("00").> + 1.i * |("01").> - |("10").> - 1.i * |("11").>) shouldEqual
      (|("00").> - |("01").> + 1.i * |("10").> - 1.i * |("11").>).bits[_2]
  }

  test("controled gates") {
    C(X) * (|("00").> + 1.i * |("01").> - |("10").> - 1.i * |("11").>) shouldEqual
      (|("00").> + 1.i * |("01").> - 1.i * |("10").> - |("11").>).bits[_2]

    X.C * (|("00").> + 1.i * |("01").> - |("10").> - 1.i * |("11").>) shouldEqual
      (|("00").> - 1.i * |("01").> - |("10").> + 1.i * |("11").>).bits[_2]
  }

  test("quantum fourier transform circuits") {
    val result = (|("000").> + |("100").>).bits[_3] |>
      (H x I x I) |>
      (Rz(Pi / 2).C x I) |>
      (Rz(Pi / 4) x I).C |>
      (I x H x I) |>
      (I x Rz(Pi / 2).C) |>
      (I x I x H)

    result.reverse shouldEqual (|("000").> + |("010").> + |("100").> + |("110").>).bits[_3]
  }
}
