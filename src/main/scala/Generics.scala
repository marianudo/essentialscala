package com.codinginflipflops.essentialscala.generics

final case class Pair[A,B](one: A, two: B)

sealed trait Sum[A,B] {
  def fold[C](left: A => C)(right: B => C): C = this match {
    case SumFailure(x) => left(x)
    case SumSuccess(x) => right(x)
  }

  def map[C](f: B => C): Sum[A,C] = this match {
    case SumSuccess(v) => SumSuccess(f(v))
    case SumFailure(msg) => SumFailure(msg)
  }

  def flatMap[C](f: B => Sum[A,C]): Sum[A,C] = this match {
    case SumSuccess(v) => f(v)
    case SumFailure(msg) => SumFailure(msg)
  }

  def mapViaFlatMap[C](f: B => C): Sum[A,C] =
    flatMap(x => SumSuccess(f(x)))

}

final case class SumFailure[A,B](value: A) extends Sum[A,B]

final case class SumSuccess[A,B](value: B) extends Sum[A,B]
