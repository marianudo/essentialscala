package com.codinginflipflops.essentialscala.generics

final case class Pair[A,B](one: A, two: B)

sealed trait Sum[+A,+B] {
  def fold[C](left: A => C)(right: B => C): C = this match {
    case Failure(x) => left(x)
    case Success(x) => right(x)
  }

  def map[C](f: B => C): Sum[A,C] = this match {
    case Success(v) => Success(f(v))
    case Failure(msg) => Failure(msg)
  }

  def flatMap[AA >: A, C](f: B => Sum[AA,C]): Sum[AA,C] = this match {
    case Success(v) => f(v)
    case Failure(msg) => Failure(msg)
  }

  def mapViaFlatMap[C](f: B => C): Sum[A,C] =
    flatMap(x => Success(f(x)))

}

final case class Failure[A](value: A) extends Sum[A,Nothing]

final case class Success[B](value: B) extends Sum[Nothing,B]
