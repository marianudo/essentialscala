package com.codinginflipflops.essentialscala

object MapsAndSets {
  val people = Set(
    "Alice",
    "Bob",
    "Charlie",
    "Derek",
    "Edith",
    "Fred"
  )

  val ages = Map(
    "Alice" -> 10,
    "Bob" -> 30,
    "Charlie" -> 50,
    "Derek" -> 40,
    "Edith" -> 10,
    "Fred" -> 60
  )

  val favoriteColors = Map(
    "Bob" -> "green",
    "Derek" -> "magenta",
    "Fred" -> "yellow"
  )

  val favoriteLolCats = Map(
    "Alice" -> "Long cat",
    "Charlie" -> "Ceiling Cat",
    "Edith" -> "Cloud Cat"
  )

  def favoriteColor(name: String): String =
    favoriteColors.get(name).getOrElse("")

  def printColors = {
    favoriteColors foreach {a => println(a._2)}
  }

  def lookup[A](name: String, map: Map[String, A]): Option[A] =
    map get name

  val oldestColor = {
    val agesList = for {
      ageEntry <- ages
    } yield ageEntry._2

    val maxAge = agesList.max
    val oldestGuyAgeEntry = ages.find { case (a, b) => b == maxAge}
    favoriteColor(oldestGuyAgeEntry.get._1)
  }

  def union[A](s1: Set[A], s2: Set[A]): Set[A] =
    s1.foldRight(s2)((a, b) => b + a)
}
