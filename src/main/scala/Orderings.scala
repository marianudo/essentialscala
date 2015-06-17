package com.codinginflipflops.essentialscala

import scala.math.Ordering

object Rational {
  implicit val absOrdering = Ordering.fromLessThan[Int] {
    (v1, v2) =>
      v1.abs < v2.abs
  }

  implicit val rationalOrdering = Ordering.fromLessThan[Rational] {
    (r1, r2) =>
      r1.num.toDouble / r1.den.toDouble < r2.num.toDouble / r2.den.toDouble
  }
}

final case class Rational(num: Int, den: Int)

object Example {
  //def example =
  //  assert(List(Rational(1, 2), Rational(3, 4), Rational(1, 3)).sorted ==
  //    List(Rational(1, 3), Rational(1, 2), Rational(3, 4)))
  //
  def example = List(Rational(1, 2), Rational(3, 4), Rational(1, 3)).sorted

  //implicit val rationalOrdering = Ordering.fromLessThan[Rational] {
  //  (r1, r2) =>
  //    r1.num.toDouble / r1.den.toDouble > r2.num.toDouble / r2.den.toDouble
  //}
}
