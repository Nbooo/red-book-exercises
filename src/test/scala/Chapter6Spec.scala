import Chapter6._
import StateMachine._
import org.scalatest.{FlatSpec, Matchers}
import scala.annotation.tailrec

class Chapter6Spec extends FlatSpec with Matchers {
  private def gen[A](length: Int)(op: Rand[A]): List[A] = {
    @tailrec
    def iter(acc: List[A], rng: RNG, count: Int): List[A] = {
      if (count == 0) acc
      else {
        val (i, nextRng) = op(rng)
        iter(i::acc, nextRng, count - 1)
      }
    }
    iter(List.empty, SimpleRng(302), length)
  }

  "nonNegative" should "return only positive integers" in {
    gen(1000000)(nonNegative) forall (_ >= 0) shouldBe true
  }

  "double" should "return a double in a range [0;1)" in {
    gen(1000000)(double) forall (n => n >= 0 && n < 1) shouldBe true
  }

  "ints" should "return a list of integers of a given length" in {
    val len = 1024
    val (list: List[Int], _) = ints(len)(SimpleRng(10))
    list.length shouldBe len
  }

  "map2" should "compose two values" in {
    val range = SimpleRng(3)

    val randA = intRand
    val randB = intRand

    val (a, ra) = randA(range)
    val (b, _) = randB(ra)

    val (sum, _) = map2(randA, randB)(_ + _)(range)

    sum shouldBe a + b
  }

  "nonNegativeLessThan" should "generate an integer 0 <= x < n" in {
    val n = 20
    val list = gen(10000)(nonNegativeLessThan(n))

    list.forall { value => value >= 0 && value < n } shouldBe true
  }

  "map2 in State" should "compose two values" in {
    val rng = SimpleRng(3)

    val sa = State.unit[Int, RNG](3)
    val sb = State.unit[Int, RNG](6)

    val sum = sa.map2(sb)(_ + _)
    val (c, _) = sum.run(rng)
    c shouldBe 9
  }

  "sequence in State" should "turn a sequence of States into a state of sequence" in {
    import State.{unit, sequence}

    val u = unit[Int, RNG] _
    val rng = SimpleRng(34)

    val numbers = List(3, 54, 34, 534, 3123)
    val states = numbers map u

    val (result, _) = sequence(states).run(rng)
    result shouldBe numbers
  }
}
