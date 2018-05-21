import org.scalatest.{FreeSpec, Matchers}
import Chapter5.{Empty, Stream => MyStream}
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

  }

}
