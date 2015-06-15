package com.codinginflipflops.essentialscala

sealed trait Maybe[A] {
  def fold[B](empty: B)(full: A => B): B = this match {
    case Empty() => empty
    case Full(x) => full(x)
  }
}

final case class Full[A](value: A) extends Maybe[A]

final case class Empty[A]() extends Maybe[A]
