package com.github.piyo7.qlione

import scala.annotation.tailrec

object Util {

  implicit class RichInt(val i: Int) extends AnyVal {
    def pow2: Int = math.pow(2, i).round.toInt

    def bitsSize: Int = bitsSize(0)

    @tailrec
    private def bitsSize(r: Int): Int = i match {
      case _ if i <= 0 => 0
      case 1 => r + 1
      case _ => (i / 2).bitsSize(r + 1)
    }

    def bitsString(size: Int): String = ("0" * size + i.toBinaryString).takeRight(size)
  }

}
