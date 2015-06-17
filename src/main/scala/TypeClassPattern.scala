package com.codinginflipflops.essentialscala

trait Equals[A] {
  def equal(a1: A, a2: A): Boolean
}

object EqualsByEMail extends Equals[Person] {
  def equal(p1: Person, p2: Person) = {
    p1.email == p2.email
  }
}

object EqualsByNameAndEMail extends Equals[Person] {
  def equal(p1: Person, p2: Person) = {
    p1.email == p2.email && p1.name == p2.name
  }
}

case class Person(name: String, email: String)
