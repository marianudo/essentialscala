package com.codinginflipflops.essentialscala.fold

sealed trait Tree[A] {
  def fold[B](leaf: A => B)(node: (B, B) => B): B
}

final case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
  def fold[B](leaf: A => B)(node: (B, B) => B): B =
    node(left.fold(leaf)(node), right.fold(leaf)(node))
}

final case class Leaf[A](a: A) extends Tree[A] {
  def fold[B](leaf: A => B)(node: (B, B) => B): B =
    leaf(a)
}
