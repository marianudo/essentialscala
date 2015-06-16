package com.codinginflipflops.essentialscala

import com.codinginflipflops.essentialscala.generics._

sealed trait Expression {
  def eval: Sum[String, Double] = this match {
    case Addition(l, r) => lift(l, r)((a, b) => Success(a+b))
    case Substraction(l, r) => lift(l, r)((a, b) => Success(a-b))
    case Division(l, r) => lift(l, r)((a, b) =>
        if(b != 0) Success(a / b) else Failure("Division by zero"))
    case SquareRoot(n) => n.eval.flatMap(v =>
        if(v < 0) Failure("Square root of negative number")
        else Success(math.pow(v, 0.5)))
    case Number(n) => Success(n)
  }

  private def lift(e1: Expression, e2: Expression)(f: (Double, Double) => Sum[String, Double]) =
    e1.eval.flatMap(
      v1 => e2.eval.flatMap(
        v2 => f(v1, v2)))
}

final case class Addition(left: Expression, right: Expression) extends Expression

final case class Substraction(left: Expression, right: Expression) extends Expression

final case class Division(left: Expression, right: Expression) extends Expression

final case class SquareRoot(value: Expression) extends Expression

final case class Number(value: Double) extends Expression
