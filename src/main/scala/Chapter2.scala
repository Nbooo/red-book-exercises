import scala.annotation.tailrec

object Chapter2 {

  // Ex 2.1 tail recursive implementation
  def fib(n: Int): Int = {
    @tailrec
    def iter(i: Int, curr: Int, prev: Int, acc: Int): Int = i match {
      case any if any > n =>  acc
      case 0              => iter(i + 1, 1, 0, 0)
      case 1 | 2          => iter(i + 1, 1, 0, 1)
      case _              => iter(i + 1,  curr + prev, curr, acc + curr)
    }
    iter(0, 0, 0, 0)
  }

  // recursive implementation
  def fibrec(n: Int): Int = n match {
    case 0      => 0
    case 1 | 2  => 1
    case _      => fibrec(n - 1) + fibrec(n - 2)
  }

  def main(args: Array[String]): Unit = {
    println("Recursive:")
    for { i <- 0 to 15 }
      yield println(fibrec(i))

    println("\nTailrec:")

    for { i <- 0 to 15 }
      yield println(fib(i))
   }
}
