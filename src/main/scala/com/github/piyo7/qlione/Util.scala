package com.github.piyo7.qlione

object Util {
  def √(x: Double): Double = math.sqrt(x)

  implicit class RichInt(val i: Int) extends AnyVal {
    def pow2: Int = math.pow(2, i).round.toInt

    def bitsSize: Int = i.toBinaryString.length

    def bitsString(size: Int): String = ("0" * size + i.toBinaryString).takeRight(size)

    def bit(pos: Int): Int = {
      val str = i.toBinaryString
      str.lift(str.length - pos - 1).map(_.toString).map(Integer.parseInt).getOrElse(0)
    }

    def dropBit(pos: Int): Int = {
      val str = i.toBinaryString
      Integer.parseInt(str.take(str.length - pos - 1) + str.drop(str.length - pos), 2)
    }
  }

}
