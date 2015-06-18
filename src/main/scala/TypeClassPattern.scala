package com.codinginflipflops.essentialscala

trait Equal[A] {
  def equal(a1: A, a2: A): Boolean
}

object Equal {
  def apply[A](implicit equals: Equal[A]) =
    equals

  object EqualsByEMail extends Equal[Person] {
    def equal(p1: Person, p2: Person) = {
      p1.email == p2.email
    }
  }

  object EqualsByNameAndEMail extends Equal[Person] {
    def equal(p1: Person, p2: Person) = {
      p1.email == p2.email && p1.name == p2.name
    }
  }

  implicit object EqualsIgnoreCase extends Equal[String] {
    def equal(s1: String, s2: String) =
      s1 equalsIgnoreCase s2
  }

  implicit class EqualOps[A](a: A) {
    def ===(other: A)(implicit eq: Equal[A]): Boolean =
      eq.equal(other, a)
  }
}

case class Person(name: String, email: String)

object IntImplicits {
  implicit class IntOps(v: Int) {
    def yeah = times {_ => println("Oh yeah!")}

    def times(f: Int => Unit): Unit = {
      (0 until v).foreach(f(_))
    }
  }
}
