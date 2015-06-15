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

  def sum: Int = this match {
    case End => 0
    case Pair(hd, tl) => hd + tl.sum
  }
}

final case class Pair(hd: Int, tl: IntList) extends IntList
final case object End extends IntList

/**
 * Exercise 7.1.3.1
 */
sealed trait LinkedList[A] {
  def length: Int = this match {
    case LinkedEnd() => 0
    case LinkedPair(_, tl) => 1 + tl.length
  }

  def contains(a: A): Boolean = this match {
    case LinkedEnd() => false
    case LinkedPair(hd, tl) => hd == a || (tl contains a)
  }

  @annotation.tailrec
  def apply(n: Int): LinkedListResult[A] = this match {
    case LinkedPair(hd, tl) => {
      if(n==0) LinkedListSuccess(hd)
      else tl(n - 1)
    }
    case LinkedEnd() => LinkedListFailure("No such element")
  }
}

final case class LinkedEnd[A]() extends LinkedList[A]

final case class LinkedPair[A](head: A, tail: LinkedList[A]) extends LinkedList[A]

sealed trait LinkedListResult[A]

final case class LinkedListSuccess[A](result: A) extends LinkedListResult[A]

final case class LinkedListFailure[A](reason: String) extends LinkedListResult[A]