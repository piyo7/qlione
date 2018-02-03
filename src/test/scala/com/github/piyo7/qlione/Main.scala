package com.github.piyo7.qlione

import java.util.Random

import com.github.piyo7.qlione.Complex._
import com.github.piyo7.qlione.QuBits._
import com.github.piyo7.qlione.QuGate._
import com.github.piyo7.qlione._Nat._

import scala.annotation.tailrec
import scala.math.Pi

object Main {
  def main(args: Array[String]) {
    implicit val random: Random = new Random(42)

    for (_ <- 0 to 10) {
      val result = (bit0 x bit1) |>>
        (H x H) |>>
        (_.measure[_0].bits) |>>
        (_.measure[_1].bits) |>>
        (_.getClassic)

      println(result)
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

  @tailrec
  def activateRus(psi: QuBits[_1], phi: Double)(implicit random: Random): QuBits[_1] = {
    val m = (psi x bit0) |>
      (I x Ry(2 * phi)) |>
      (QuGate(-1.i * Y.matrix) x I) |>
      (I x Ry(2 * phi).t) |>
      (_.measure[_0])

    if (m.result) {
      activateRus(m.unmeasured |> Ry(Pi / 2), phi)
    } else {
      m.unmeasured
    }
  }
}
