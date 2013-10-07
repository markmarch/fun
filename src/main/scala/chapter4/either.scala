package me.ontrait.fun.chapter4

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case e@Left(_) => e
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    map(f) match {
      case e@Left(_) => e
      case Right(a) => a
    }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case _ => this
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      ra <- this
      rb <- b
    } yield (f(ra, rb))
}

case class Left[+A](get: A) extends Either[A, Nothing]
case class Right[+B](get: B) extends Either[Nothing, B]

object Either {
  def sequence[E, A](e: List[Either[E, A]]): Either[E, List[A]] =
    traverse(e)(identity)

  def traverse[E, A, B](a: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    def helper(list: List[B], rest: List[A]): Either[E, List[B]] = rest match {
      case Nil => Right(list.reverse)
      case x :: xs => f(x) match {
        case e @ Left(_) => e
        case Right(b) => helper(b :: list, xs)
      }
    }

    helper(Nil, a)
  }
}

object EitherTest {
  case class Person(name: Name, age: Age)
  sealed class Name(val value: String) {
    override def toString = s"Name: $value"
  }

  sealed class Age(val value: Int) {
    override def toString = s"Age: $value"
  }

  def mkName(name: String): Either[String, Name] =
    if (name == null || name.length == 0) Left("Invalid name")
    else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0 || age > 150) Left("Age is out of range")
    else Right(new Age(age))

  def mkPerson(name: String, age: Int) =
    mkName(name).map2(mkAge(age))(Person(_, _))

  def main(args: Array[String]): Unit = {
    println(mkPerson("", 10))
    println(mkPerson("Mark", -2))
    println(mkPerson("", -2))
    println(mkPerson("Mark", 22))
  }
}