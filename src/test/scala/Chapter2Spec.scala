import org.scalatest.WordSpec
import org.scalatest.Matchers._

import scala.util.Random

class Chapter2Spec extends WordSpec {

  "In Chapter 2 exercises" should {
    "be isSorted correct implementation" in {
      import Chapter2._
      val sorted = (for (i <- 1 to 10) yield i).toArray
      val unsorted = (for (_ <- 1 to 10) yield Random.nextInt()).toArray

      isSorted(sorted, (a: Int,b: Int) => a < b) shouldBe true
      isSorted(sorted.reverse, (a: Int, b: Int) => a > b) shouldBe true
      isSorted(sorted.reverse, (a: Int, b: Int) => a < b) shouldBe false
      isSorted(unsorted, (a: Int, b: Int) => a < b) shouldBe false
      isSorted(unsorted , (a: Int, b: Int) => a > b) shouldBe false
      isSorted(unsorted, (_: Int, _: Int) => true) shouldBe true
      isSorted(sorted, (_: Int, _: Int) => false) shouldBe false
    }
  }

}
