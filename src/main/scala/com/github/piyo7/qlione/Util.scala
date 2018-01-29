package com.github.piyo7.qlione

object Util {

  implicit class RichInt(val i: Int) extends AnyVal {
    def pow2: Int = math.pow(2, i).round.toInt

    def bitsSize: Int = i.toBinaryString.length

    def bitsString(size: Int): String = ("0" * size + i.toBinaryString).takeRight(size)
  }

}
