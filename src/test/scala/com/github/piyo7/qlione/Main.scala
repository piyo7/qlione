package com.github.piyo7.qlione

import java.util.Random

import com.github.piyo7.qlione.QuBits._
import com.github.piyo7.qlione.QuGate._
import com.github.piyo7.qlione._Nat._

import scala.math.Pi

object Main {
  def main(args: Array[String]) {
    implicit val random: Random = new Random(42)

    for (_ <- 0 to 10) {
      val result = (bit0 x bit1) |>>
        (H x H) |>>
        measure[_0, _2] |>>
        measure[_1, _2]
      println(result.getClassic)
    }

    val result: QuBits[_3] =
      (|("000").> + |("100").>).bits[_3] |>
        (H x I x I) |>
        (Rz(Pi / 2).C x I) |>
        (Rz(Pi / 4) x I).C |>
        (I x H x I) |>
        (I x Rz(Pi / 2).C) |>
        (I x I x H)

    println(result)
  }

}
