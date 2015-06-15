package com.codinginflipflops.essentialscala

sealed trait IntList {
  def length: Int =
    fold[Int](0, (_, acc) => acc + 1)

  def product: Int =
    fold[Int](1, (hd, acc) => hd * acc)

  def double: IntList =
    fold[IntList](End, (hd, acc) => Pair(hd * 2, acc))

  def sum: Int =
    fold(0, (hd: Int, acc: Int) => hd + acc)

  def fold[A](end: A, f: (Int, A) => A): A = this match {
    case End => end
    case Pair(hd, tl) => f(hd, tl.fold(end, f))
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

  def fold[B](z: B, f: (A, B) => B): B = this match {
    case LinkedEnd() => z
    case LinkedPair(hd, tl) => f(hd, tl.fold(z, f))
  }

}

final case class LinkedEnd[A]() extends LinkedList[A]

final case class LinkedPair[A](head: A, tail: LinkedList[A]) extends LinkedList[A]

sealed trait LinkedListResult[A]

final case class LinkedListSuccess[A](result: A) extends LinkedListResult[A]

final case class LinkedListFailure[A](reason: String) extends LinkedListResult[A]
