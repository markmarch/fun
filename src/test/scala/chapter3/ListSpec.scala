package me.ontrait.fun.chapter3

import org.scalatest.FreeSpec
import org.scalatest.matchers.ShouldMatchers

class ListSpec extends FreeSpec with ShouldMatchers {
  import List._

  "Lists Specification" - {
    "tail should take the 'tail' of the list" in {
      tail(List(1, 2, 3)) should be (List(2, 3))
      tail(List(1)) should be (Nil)
      tail(List()) should be (Nil)
    }

    "drop should drop the given number of elements from the head of the list" in {
      drop(List(1, 2, 3), 0) should be (List(1, 2, 3))
      drop(List(1, 2, 3), 2) should be (List(3))
      drop(List(1, 2, 3), 10) should be (Nil)
      drop(List(), 10) should be (Nil)
    }

    "dropWhile should drop while the head of list satisfic the predicate" in {
      dropWhile(List(1, 2, -2, 3))((x: Int) => x > 0) should be (List(-2, 3))
      dropWhile(List(1, 2, -2, 3))(_ % 2 != 0) should be (List(2, -2, 3))
      dropWhile(List(1, 2, -2, 3))(_ < 10) should be (Nil)
    }

    "init should take every element except the last element in the list" in {
      init(List(1, 2, 3)) should be (List(1, 2))
      init(List(1)) should be (Nil)
      init(List()) should be (Nil)
    }

    "foldRight should fold the list into a single value" in {
      foldRight(List(1, 2, 3), 0)(_ + _) should be (6)
      foldRight(List[Int](), 0)(_ + _) should be (0)
      foldRight(List(1, 2, 3), 1.0)(_ * _) should be (6.0)
    }
  }
}