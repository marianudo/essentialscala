package com.codinginflipflops.essentialscala.generics

final case class Pair[A,B](one: A, two: B)

sealed trait Sum[A,B]

final case class Left[A,B](value: A) extends Sum[A,B]

final case class Right[A,B](value: B) extends Sum[A,B]
