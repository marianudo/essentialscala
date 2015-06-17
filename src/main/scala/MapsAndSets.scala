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

  def sumUnion[A](m1: Map[A, Int], m2: Map[A, Int]): Map[A, Int] =
    m1.foldLeft(m2)((a: Map[A, Int], b: (A, Int)) => a + (b._1 -> (a.getOrElse(b._1, 0) + b._2)))

  /*
   * This is the book implementation, but it doesn't work as it is.
   * I had to fix it (look at line 68 and compare it with exercise A.5.41 to see the fix)
   */
  def sumUnionRef[A](m1: Map[A, Int], m2: Map[A, Int]): Map[A, Int] = {
    m1.foldLeft(m2) {
      (map, elt) =>
        val (k, v) = elt
        val newV = map.getOrElse(k, 0) + v
        map + (k -> newV)
    }
  }

  /*
   * First implementation from the top of my head. I'll try to remove the Option from the function
   */
  def union[A, B](m1: Map[A, B], m2: Map[A, B])(f: (B, Option[B]) => B): Map[A, B] = {
    val foldFunc: (Map[A, B], (A, B)) => Map[A, B] =
      (map, tup) => map + (tup._1 -> f(tup._2, map.get(tup._1)))

    m1.foldLeft(m2)(foldFunc)
  }

  def unionOk[A, B](m1: Map[A, B], m2: Map[A, B])(f: (B, B) => B): Map[A, B] = {
    m1.foldLeft(m2) { (map, tup) =>
      val (k, v) = tup
      val newV = map.get(k).map(v2 => f(v2, v)).getOrElse(v)
      map + (k -> newV)
    }
  }
}
