package scala.collection.deque

import org.scalatest.{FunSuite, Matchers, OptionValues}

class DequeSpec extends FunSuite with Matchers with OptionValues {
  test("Uncons empty Deque") {
    val sut = Deque.empty[Int]

    Uncons.unapply(sut) shouldBe None
  }

  test("cons-ed Deque") {
    val sut = 1 +: 2 +: 3 +: Deque.empty

    sut match {
      case Uncons(h, _) =>
        h shouldBe 1
    }

    sut match {
      case Unsnoc(_, l) =>
        l shouldBe 3
    }
  }

  test("snoc-ed Deque") {
    val sut = Deque.empty[Int] :+ 3 :+ 2 :+ 1

    sut match {
      case Uncons(h, _) =>
        h shouldBe 3
    }

    sut match {
      case Unsnoc(_, last) =>
        last shouldBe 1
    }
  }
}
