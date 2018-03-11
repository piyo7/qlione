package com.github.piyo7.qlione

import scala.language.implicitConversions

case class Complex(re: Double, im: Double) {
  def unary_-(): Complex = Complex(-re, -im)

  def +(that: Complex): Complex = Complex(re + that.re, im + that.im)

  def -(that: Complex): Complex = Complex(re - that.re, im - that.im)

  def *(that: Complex): Complex = Complex(re * that.re - im * that.im, re * that.im + im * that.re)

  def /(that: Complex): Complex = this * Complex(that.re / that.abs2, -that.im / that.abs2)

  def conj: Complex = Complex(re, -im)

  def exp: Complex = math.exp(re) * Complex(math.cos(im), math.sin(im))

  def abs2: Double = re * re + im * im

  def abs: Double = math.sqrt(abs2)

  def rad: Double = math.atan2(im, re)

  def deg: Double = math.toDegrees(rad)
}

object Complex {
  implicit def intToComplex(i: Int): Complex = Complex(i, 0)

  implicit def doubleToComplex(d: Double): Complex = Complex(d, 0)

  implicit class DoubleToComplex(val d: Double) extends AnyVal {
    def r: Complex = Complex(d, 0)

    def i: Complex = Complex(0, d)
  }

}
