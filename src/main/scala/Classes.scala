package com.codinginflipflops.essentialscala

class Director(
  firstName: String,
  lastName: String,
  val yearOfBirth: Int) {

    def name = s"$firstName $lastName"
  }

class Film(
  name: String,
  yearOfRelease: Int,
  imdbRating: Double,
  val director: Director) {

    def directorsAge = director.yearOfBirth

    def isDirectedBy(director: Director): Boolean = this.director == director

    def copy(name: String = "Random film",
      yearOfRelease: Int = 2000,
      imdbRating: Double = 7.5,
      director: Director = new Director("Matilda", "Perez", 1980)) = {
        new Film(name, yearOfRelease, imdbRating, director)
    }

    override def toString = s"$name, $yearOfRelease, $imdbRating, $director"
  }

class Counter(val count: Int) {
  def inc(inc: Int = 1) = new Counter(count + inc)
  def dec(inc: Int = 1) = new Counter(count - inc)
  def inc: Counter = inc()
  def dec: Counter = dec()
  def adjust(adder: Adder) = new Counter(adder add count)
}

class Adder(amount: Int) {
  def add(in: Int) = in + amount
}
