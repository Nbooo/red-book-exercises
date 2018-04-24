import Chapter3.{MyList, Trees}
import org.scalatest.WordSpec
import org.scalatest.Matchers._
import Chapter3.MyList._

class Chapter3Spec extends WordSpec {
  "List exercises in chapter 3" should {
    "return correct tail" in {

      val list = MyList(1, 2, 3, 4, 5, 6, 7)
      tail(list) shouldBe MyList(2, 3, 4, 5, 6, 7)
      tail(tail(tail(list))) shouldBe MyList(4, 5, 6, 7)
    }

    "replace head of the list correctly" in {
      val list = MyList(1, 2, 3)

      setHead(5, list) shouldBe MyList(5, 2, 3)
      setHead(1, setHead(4, list)) shouldBe list
    }

    "implement drop correctly" in {
      val list = MyList(1 to 20 :_*)

      drop(10, list) shouldBe MyList(11 to  20 :_*)
    }

    "implement dropWhile" in {
      val list = MyList(1 to 100 :_*)
      dropWhile(list)(x => x % 2 != 0) shouldBe MyList(2 to 100 :_*)
      dropWhile(list)(x => x != 50) shouldBe MyList(50 to 100 :_*)
    }

    "implement init" in {
      val list = MyList(1 to 10 :_*)
      init(list) shouldBe MyList(1 to 9 :_*)
    }

    "implement product" in {
      val list = MyList[Double](1, 2, 3)
      product(list) shouldBe 6
    }

    "length computed correctly" in {
      val list = MyList(1,2,3,4,5)
      MyList.length(list) shouldBe 5
    }

    "flSum computes sum correctly" in {
      val list = MyList(1,1,1,1,1)
      flSum(list) shouldBe 5
    }

    "flProduct computes product correctly" in {
      val list = MyList[Double](1, 1, 1, 1, 1, 1)
      flProduct(list) shouldBe 1
    }

    "flLength computes length correctly" in {
      val list = MyList(1, 3, 5, 6)
      flLength(list) shouldBe 4
    }

    "reverse should work correctly" in {
      val list = MyList(1, 2, 3)
      reverse(list) shouldBe MyList(3, 2, 1)
    }

    "frAppend should work correctly" in {
      val list = MyList(1, 2, 3)
      frAppend(5, list) shouldBe MyList(1, 2, 3, 5)
    }

    "flatten should work correctly" in {
      val list = MyList(MyList(1), MyList(2), MyList(3, 4))
      val fla = flatten(list)
      fla shouldBe MyList(1, 2, 3, 4)
    }

    "map should work correctly" in {
      map(MyList(1, 3, 5))(_ + 1) shouldBe MyList(2, 4, 6)
    }

    "filter should work correctly" in {
      filter(MyList(1, 2, 3, 4, 5, 6))(_ % 2 == 0) shouldBe MyList(2, 4, 6)
    }

    "flatMap should work correctly" in {
      flatMap(MyList(1, 2, 3))(x => MyList(x, x)) shouldBe MyList(1, 1, 2, 2, 3, 3)
    }

    "flmFilter should work correctly" in {
      flmFilter(MyList(1, 2, 3, 4, 5, 6))(_ % 2 == 0) shouldBe MyList(2, 4, 6)
    }

    "combine should work correctly" in {
      combine(MyList(1, 2, 3), MyList(4, 5, 6)) shouldBe MyList(5, 7, 9)
    }

    "implement zipWith" in {
      zipWith(MyList(1,2,3), MyList(3,2,1))((a,b) => s"$a&$b") shouldBe MyList("1&3", "2&2", "3&1")
    }

    "implement hasSubsequence" in {
      val list = MyList(1, 2, 3, 4, 5, 6, 7, 8, 4, 5, 8)
      hasSubsequence(list, MyList(1, 2)) shouldBe true
      hasSubsequence(list, MyList(3, 4)) shouldBe true
      hasSubsequence(list, MyList(4)) shouldBe true
      hasSubsequence(list, MyList(8, 4, 5)) shouldBe true

      hasSubsequence(list, MyList(2, 5, 6, 7)) shouldBe false
      hasSubsequence(list, MyList(5, 6, 7, 2)) shouldBe false

      hasSubsequence(MyList[Int](), MyList(3)) shouldBe false
    }
  }

  "Tree exercises in chapter 3" should {
    import Chapter3.Trees._
    import Chapter3.Trees.Tree._

    val tree = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))

    "implement size" in {
      size(tree) shouldBe 3
    }

    "implement maximum" in {
      maximum(tree) shouldBe 3
    }

    "implement depth" in {
      depth(tree) shouldBe 2
    }

    "implement map" in {
      map(tree)(_ + 1) shouldBe Branch(Branch(Leaf(2), Leaf(3)), Leaf(4))
    }

    "implement fold" in {
      fold(tree, 0)(_+_) shouldBe 6
    }

    "implement size via fold" in {
      flSize(tree) shouldBe size(tree)
    }

    "implement maximum via fold" in {
      flMax(tree) shouldBe maximum(tree)
    }

    "implement depth via fold" in {

    }

    "implement map via fold" in {

    }
  }
}
