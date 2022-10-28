package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }

  def depth[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 0
      case Branch(left, right) => 1 + (depth(left) max depth(right))
    }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    tree match {
      case Leaf(value) => Leaf(f(value))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }

  def fold[A, B](tree: Tree[A])(f: A => B, g: (B, B) => B): B =
    tree match {
      case Leaf(value) => f(value)
      case Branch(left, right) => g(fold(left)(f, g), fold(right)(f, g))
    }
}