package me.ontrait.fun.chapter4

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = {
    this match {
      case None => None
      case Some(e) => Some(f(e))
    }
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(e) => e
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)
}

case object None extends Option[Nothing]

case class Some[+A](get: A) extends Option[A]

object Option {
  def variance(xs: Seq[Double]): Option[Double] = {
    def mean(xs: Seq[Double]) =
      if (xs.isEmpty) None
      else Some(xs.sum / xs.size)

    for {
      m <- mean(xs)
      x <- mean(xs.map(x => math.pow(x - m, 2)))
    } yield (x)
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      ra <- a
      rb <- b
    } yield f(ra, rb)

  import java.util.regex.Pattern

  def mkMatcher(pattern: String): Option[String => Boolean] = {
    try {
      val p = Pattern.compile(pattern)
      Some((s: String) => p.matcher(s).matches)
    } catch {
      case e: Exception => None
    }
  }


  def bothMatch_2(pat1: String, pat2: String, s: String): Option[Boolean] =
    map2(mkMatcher(pat1), mkMatcher(pat2))((a, b) => a(s) && b(s))

  def sequence[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(identity)

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    def helper(list: List[B], rest: List[A]): Option[List[B]] = rest match {
      case Nil => Some(list.reverse)
      case x :: xs => f(x) match {
        case Some(b) => helper(b :: list, xs)
        case None => None
      }
    }

    helper(Nil, a)
  }

}