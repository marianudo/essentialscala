package com.codinginflipflops.essentialscala

sealed trait IntList {
  def length: Int = this match {
    case End => 0
    case Pair(_, tl) => 1 + tl.length
  }

  def product: Int = this match {
    case End => 1
    case Pair(hd, tl) => hd * tl.product
  }

  def double: IntList = this match {
    case Pair(hd, tl) => Pair(2 * hd, tl.double)
    case _ => End
  }
}

final case class Pair(hd: Int, tl: IntList) extends IntList
final case object End extends IntList
