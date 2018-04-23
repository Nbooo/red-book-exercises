import org.scalatest.{FreeSpec, Matchers}
import Chapter4._
class Chapter4Spec extends FreeSpec with Matchers {
  "Among exercises in chapter 4 there" - {
    "should be correct implementation of" - {
      "Some case of Option with all methods" in {
        Some(40).get shouldBe 40
        Some(2).map(_ * 2) shouldBe Some(4)
        Some(30).flatMap(x => Some(x + " is a string now")) shouldBe Some("30 is a string now")
        Some("value").getOrElse("default") shouldBe "value"
        Some("value").orElse(Some("default")) shouldBe Some("value")
        Some(30).filter(_ > 10) shouldBe Some(30)
        Some(30).filter(_ < 10) shouldBe None
      }

      "None case of Option with all methods" in {
        None.map(x => x.toString) shouldBe None
        None.orElse(Some(3)) shouldBe Some(3)
        None.getOrElse(3) shouldBe 3
        None.flatMap(_ => Some(3)) shouldBe None
        None.filter(_ => true) shouldBe None
      }

      "variance" in {
        val xs = List[Double](1, 2, 3, 4, 5)
        variance(xs) shouldBe Some(2d)
        variance(List.empty) shouldBe None
      }

      "map2" in {
        map2(Some(4), Some(34))((a,b) => b - a) shouldBe Some(30)
        map2(None, Some(34))((a: Int, b: Int) => b - a) shouldBe None
        map2(Some(4), None)((a: Int, b: Int) => b - a) shouldBe None
      }

      "sequence" in {
        sequence(List(None, None, None)) shouldBe None
        sequence(List(Some(1), None, Some(3))) shouldBe Some(List(1,3))
        sequence(List.empty) shouldBe None
       }

      "Left case of Either" in {
        val ex = new Throwable("test")
        val left = Left(ex)

        left.map(_ => "whatever") shouldBe Left(ex)
        left.flatMap(_ => Right("whatever")) shouldBe Left(ex)
        left.orElse(Right("whatever")) shouldBe Right("whatever")
        left.map2(Right("s"))((a: Int, b: String) => 3d) shouldBe Left(ex)
      }

      "Right case of Either" in {
        val right = Right("some")
        right.map(x => x + " value") shouldBe Right("some value")
        right.flatMap(_ => Right(42)) shouldBe Right(42)
        right.orElse(Right(42)) shouldBe Right("some")
        right.map2(Right(42))((x, y) => s"${x}times answer is $y") shouldBe Right("sometimes answer is 42")
      }

      "mkPerson" in {
        mkPerson("", -1) shouldBe Left("Name is empty and Age is out of range")
        mkPerson("named", -1) shouldBe Left("Age is out of range")
        mkPerson("", 10) shouldBe Left("Name is empty")
      }
    }
  }
}
