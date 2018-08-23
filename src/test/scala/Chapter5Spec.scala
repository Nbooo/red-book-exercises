import org.scalatest.{FreeSpec, Matchers}
import Chapter5.{Cons, Empty, Stream => MyStream}
class Chapter5Spec extends FreeSpec with Matchers {

  "Exercises in chapter 5 should contain implementation of" - {
    "toList helper function" in {
      MyStream(1, 3, 5, 7, 10).toList shouldBe List(1, 3, 5, 7, 10)
      MyStream().toList shouldBe Nil
    }

    "take" in {
      MyStream(1, 2, 3, 4, 5).take(2).toList shouldBe List(1, 2)
      MyStream(1, 2, 3).take(0).toList shouldBe Nil
      MyStream(1, 2, 3).take(9).toList shouldBe List(1, 2, 3)
    }

    "drop" in {
      MyStream(1, 2, 3, 4, 5).drop(2).toList shouldBe List(3, 4, 5)
      MyStream(1, 2, 3).drop(0).toList shouldBe List(1, 2, 3)
      MyStream(1, 2, 3).drop(9).toList shouldBe Nil
    }

    "takeWhile" in {
      MyStream(1, 2, 3, 4, 5).takeWhile(_ < 4).toList shouldBe List(1, 2, 3)
      MyStream(1, 2, 3).takeWhile(_ > 100).toList shouldBe Nil
    }

    "forAll" in {
      MyStream(1 to 10 : _*).forAll(_ < 11) shouldBe true
      // not stack safe, but will terminate as soon as 1000th element will be processed
      MyStream(1 to Int.MaxValue : _*).forAll(_ < 1000) shouldBe false
    }

    "frTakeWhile" in {
      val r = MyStream(1,2,3,4,5,6).frTakeWhile(_ < 5)
      println(r)
    }

    "fibs" in {
      val fib = MyStream.fibs
      fib.take(10).toList shouldBe List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
    }

    "unfoldFibs" in {
      val fib = MyStream.unfoldFibs
      fib.take(10).toList shouldBe List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
    }

    "unfoldFrom" in {
      val from = MyStream.unfoldFrom(3)
      from.take(5).toList shouldBe List(3, 4, 5, 6, 7)
    }

    "unfoldConstant" in {
      val actual = MyStream.unfoldConstant(3).take(10).toList
      val expected = (for (_ <- 1 to 10) yield 3).toList

      actual shouldBe expected
    }

    "unfoldOnes" in {
      val actual = MyStream.unfoldOnes.take(10).toList
      val expected = (for (_ <- 1 to 10) yield 1).toList

      actual shouldBe expected
    }

    "unfoldMap" in {
      val input = MyStream(1, 2, 3, 4, 5, 6)
      input.unfoldMap(_ + 2).toList shouldBe List(3,4,5,6,7,8)
    }

    "unfoldTake" in {
      MyStream(1, 2, 3, 4, 5).unfoldTake(3).toList shouldBe List(1, 2, 3)
      MyStream(1, 2, 3).unfoldTake(0).toList shouldBe Nil
      MyStream(1, 2, 3).unfoldTake(-1).toList shouldBe Nil
      MyStream(1, 2, 3).unfoldTake(9).toList shouldBe List(1, 2, 3)
    }

    "unfoldTakeWhile" in {
      MyStream(1, 2, 3, 4, 5).unfoldTakeWhile(_ < 4).toList shouldBe List(1, 2, 3)
      MyStream(1, 2, 3).unfoldTakeWhile(_ < 0).toList shouldBe Nil
      MyStream(1, 2, 3).unfoldTakeWhile(_ > 1000).toList shouldBe Nil
      MyStream(1, 2, 3).unfoldTakeWhile(_ >= 2).toList shouldBe Nil
    }

    "unfoldZipWith" in {
      MyStream(1, 3, 5).unfoldZipWith(MyStream(2, 4, 6)).toList shouldBe List((1, 2), (3,4), (5, 6))
      MyStream(1, 3, 5).unfoldZipWith(MyStream(2, 4, 6, 8)).toList shouldBe List((1, 2), (3,4), (5, 6))
      MyStream(1, 3, 5, 7).unfoldZipWith(MyStream(2, 4, 6)).toList shouldBe List((1, 2), (3,4), (5, 6))
      MyStream().unfoldZipWith(MyStream(2, 4, 6)).toList shouldBe Nil
      MyStream(1, 3, 5).unfoldZipWith(MyStream()).toList shouldBe Nil
    }

    "unfoldZipAll" in {
      MyStream(1, 3, 5).unfoldZipAll(MyStream("a", "b", "c"), -1, "missing").toList shouldBe
        List((1, "a"), (3, "b"), (5, "c"))

      MyStream(1, 3).unfoldZipAll(MyStream("a", "b", "c"), -1, "missing").toList shouldBe
        List((1, "a"), (3, "b"), (-1, "c"))

      MyStream(1, 3, 5).unfoldZipAll(MyStream("a", "b"), -1, "missing").toList shouldBe
        List((1, "a"), (3, "b"), (5, "missing"))

      MyStream(1, 3, 5).unfoldZipAll(MyStream(), -1, "missing").toList shouldBe
        List((1, "missing"), (3, "missing"), (5, "missing"))

      MyStream().unfoldZipAll(MyStream("a", "b", "c"), -1, "missing").toList shouldBe
        List((-1, "a"), (-1, "b"), (-1, "c"))
    }

    "startsWith" in {
      MyStream(1, 3, 5).startsWith(MyStream()) shouldBe true
      MyStream(1, 3, 5).startsWith(MyStream(1, 3, 5)) shouldBe true
      MyStream(1, 3, 5).startsWith(MyStream(1, 3)) shouldBe true
      MyStream().startsWith(MyStream(1, 3, 5)) shouldBe false
      MyStream(1, 3).startsWith(MyStream(1, 3, 5)) shouldBe false
    }

    "tails" in {
      MyStream(1, 2, 3, 4, 5).tails.map(_.toList).toList shouldBe
        List(List(1, 2, 3, 4, 5), List(2, 3, 4, 5), List(3, 4, 5), List(4, 5), List(5))
    }

    "scanRight" in {
      MyStream(1, 2, 3).scanRight(0)(_ + _).toList shouldBe List(6, 5, 3, 0)
    }
  }

}
