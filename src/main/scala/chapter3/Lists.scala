package me.ontrait.fun.chapter3

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](a: A*): List[A] = if (a.isEmpty) Nil else Cons(a.head, apply(a.tail: _*))

  def tail[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  @tailrec
  def drop[A](list: List[A], n: Int): List[A] = {
    require(n >= 0, "n must be larger than or equal to 0")

    (list, n) match {
      case (Nil, _) => Nil
      case (_, 0)   => list
      case _ => drop(tail(list), n - 1)
    }
  }

  @tailrec
  def dropWhile[A](list: List[A])(predicate: A => Boolean): List[A] = list match {
    case Nil => Nil
    case Cons(head, tail) if predicate(head) => dropWhile(tail)(predicate)
    case list => list
  }

  def init[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A, B](list: List[A], b: B)(f: (A, B) => B): B = list match {
    case Nil => b
    case Cons(h, t) => f(h, foldRight(t, b)(f))
  }
}