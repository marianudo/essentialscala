package com.codinginflipflops.essentialscala

sealed trait Tree {
  def sum: Int = this match {
    case Leaf(v) => v
    case Node(l, r) => l.sum + r.sum
  }

  def double: Tree = this match {
    case Node(l, r) => Node(l.double, r.double)
    case Leaf(v) => Leaf(v * 2)
  }
}

final case class Node(left: Tree, right: Tree) extends Tree {
  override def sum: Int = left.sum + right.sum
  override def double: Tree = Node(left.double, right.double)
}

final case class Leaf(value: Int) extends Tree {
  override def sum: Int = value
  override def double: Tree = Leaf(value * 2)
}
