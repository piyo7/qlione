package com.github.piyo7.qlione

sealed trait _OptNat

object _OptNat {

  class _none extends _OptNat

  sealed trait _Nat extends _OptNat

  class _0 extends _Nat

  class _suc[A <: _Nat] extends _Nat

  class _value[A <: _OptNat] private(val value: Option[Int]) extends AnyVal {
    def apply(): Option[Int] = value
  }

  object _value {
    implicit val valueNone: _value[_none] = new _value(None)

    implicit val value0: _value[_0] = new _value(Some(0))

    implicit def valueSuc[A <: _Nat](implicit a: _value[A]): _value[_suc[A]] = new _value(a().map(_ + 1))
  }

  class _lt[A <: _OptNat, B <: _OptNat] private()

  object _lt {
    implicit def lt0[A <: _Nat]: _lt[_0, _suc[A]] = new _lt()

    implicit def ltSuc[A <: _Nat, B <: _Nat](implicit ab: _lt[A, B]): _lt[_suc[A], _suc[B]] = new _lt()
  }

  type _gt[A <: _OptNat, B <: _OptNat] = _lt[B, A]

  trait _pre[A <: _OptNat] {
    type Out <: _Nat
    val vOut: _value[Out]
  }

  object _pre {
    type Aux[A <: _Nat, B <: _Nat] =
      _pre[A] {
        type Out = B
      }

    implicit def preSuc[A <: _Nat](implicit vA: _value[A]): Aux[_suc[A], A] =
      new _pre[_suc[A]] {
        type Out = A
        val vOut: _value[A] = vA
      }
  }

  trait _plus[A <: _OptNat, B <: _OptNat] {
    type Out <: _Nat
    val vOut: _value[Out]
  }

  object _plus {
    type Aux[A <: _Nat, B <: _Nat, C <: _Nat] =
      _plus[A, B] {
        type Out = C
      }

    implicit def plus0[A <: _Nat](implicit vA: _value[A]): Aux[A, _0, A] =
      new _plus[A, _0] {
        type Out = A
        val vOut: _value[A] = vA
      }

    implicit def plusSuc[A <: _Nat, B <: _Nat, C <: _Nat](implicit pABC: _plus.Aux[A, B, C], vsC: _value[_suc[C]]): Aux[A, _suc[B], _suc[C]] =
      new _plus[A, _suc[B]] {
        type Out = _suc[C]
        val vOut: _value[_suc[C]] = vsC
      }
  }

  type _1 = _suc[_0]
  type _2 = _suc[_1]
  type _3 = _suc[_2]
  type _4 = _suc[_3]
  type _5 = _suc[_4]
  type _6 = _suc[_5]
  type _7 = _suc[_6]
  type _8 = _suc[_7]
  type _9 = _suc[_8]
  type _10 = _suc[_9]
  type _11 = _suc[_10]
  type _12 = _suc[_11]
  type _13 = _suc[_12]
  type _14 = _suc[_13]
  type _15 = _suc[_14]
  type _16 = _suc[_15]
}
