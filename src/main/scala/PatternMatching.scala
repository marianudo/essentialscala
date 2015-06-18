package com.codinginflipflops.essentialscala

object TitleCase {
  def unapply(str: String): Option[String] = {
    val words = str.split(" ").toList
    val titledWords = words.map(w => w.substring(0, 1).toUpperCase + w.substring(1))
    Some(titledWords.mkString(" "))
  }
}
