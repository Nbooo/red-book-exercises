import scala.annotation.tailrec

object Chapter2 {

  /**
    * Exercise 2.1
    * */
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

  /**
    * Exercise 2.2
    * */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def iter(i: Int): Boolean = {
      if (i == as.length - 1) true
      else ordered(as(i - 1), as(i)) && iter(i + 1)
    }

    if (as.length <= 1) true
    else iter(1)
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = {
    f(a, _)
  }

  /**
    * Exercise 2.3
    * */
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => f(a, _)
  }

  /**
    * Exercise 2.4
    * */
  def uncurry[A, B, C] (f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  /**
    * Exercise 2.5
    * */
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a => (g andThen f)(a)
  }

  def main(args: Array[String]): Unit = {

  }
}
