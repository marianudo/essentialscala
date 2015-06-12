package com.codinginflipflops.essentialscala

case class Director(
  firstName: String,
  lastName: String,
  yearOfBirth: Int) {

    def name = s"$firstName $lastName"
  }

object Director {
  def older(d1: Director, d2: Director) =
    if(d1.yearOfBirth >= d2.yearOfBirth) d2
    else d1
}

case class Film(
  name: String,
  yearOfRelease: Int,
  imdbRating: Double,
  director: Director) {

    def directorsAge = director.yearOfBirth

    def isDirectedBy(director: Director): Boolean = this.director == director
  }

object Film {
  def newer(film1: Film, film2: Film): Film =
    if (film1.yearOfRelease < film2.yearOfRelease) film1 else film2
    def highestRating(film1: Film, film2: Film): Double = { val rating1 = film1.imdbRating
    val rating2 = film2.imdbRating
    if (rating1 > rating2) rating1 else rating2
  }

  def oldestDirectorAtTheTime(film1: Film, film2: Film): Director =
    if (film1.directorsAge > film2.directorsAge) film1.director else film2.director
}

case class Counter(val count: Int = 0) {
  def inc(inc: Int = 1) = new Counter(count + inc)
  def dec(inc: Int = 1) = new Counter(count - inc)
  def inc: Counter = inc()
  def dec: Counter = dec()
  def adjust(adder: Adder) = new Counter(adder(count))
}

class Adder(amount: Int) {
  def apply(in: Int) = in + amount
}
