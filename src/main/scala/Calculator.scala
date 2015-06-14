package com.codinginflipflops.essentialscala

sealed trait Expression {
  def eval: Calculation
}

final case class Addition(left: Expression, right: Expression) extends Expression {
  def eval = left.eval match {
    case Failure(msg) => Failure(msg)
    case Success(lr) => right.eval match {
      case Failure(msg) => Failure(msg)
      case Success(rr) => Success(lr + rr)
    }
  }
}

final case class Substraction(left: Expression, right: Expression) extends Expression {
  def eval = left.eval match {
    case Failure(msg) => Failure(msg)
    case Success(lr) => right.eval match {
      case Failure(msg) => Failure(msg)
      case Success(rr) => Success (lr - rr)
    }
  }

}

final case class Division(left: Expression, right: Expression) extends Expression {
  def eval = left.eval match {
    case Failure(msg) => Failure(msg)
    case Success(lr) => right.eval match {
      case Failure(msg) => Failure(msg)
      case Success(rr) =>
        if(rr == 0) Failure("Division by zero")
        else Success(lr / rr)
    }
  }
}

final case class SquareRoot(value: Double) extends Expression {
  def eval =
    if(value < 0) Failure("Square root of negative number")
    else Success(math.pow(value, 0.5))
}

final case class Number(value: Double) extends Expression {
  def eval = Success(value)
}

sealed trait Calculation

final case class Success(result: Double) extends Calculation

final case class Failure(message: String) extends Calculation
