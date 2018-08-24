import scala.annotation.tailrec

object Chapter6 {
  trait RNG {
    def nextInt: (Int, RNG)
  }

  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, nextRng) = s(rng)
      (f(a), nextRng)
    }


  case class SimpleRng(seed: Long) extends RNG {

    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5deece66dl + 0xBL) & 0xffffffffffffffL
      val nextRng = SimpleRng(newSeed)
      val n = (newSeed >> 16).toInt
      (n, nextRng)
    }
  }

  def intRand: Rand[Int] = _.nextInt
  def doubleRand: Rand[Double] = double
  def nonNegativeRand: Rand[Int] = rng => nonNegative(rng)
  /**
    * Exercise 6.1
    * */
  @tailrec
  def nonNegative(rng: RNG): (Int, RNG) = {
    def toPositive(n: Int): Option[Int] = n match {
      case x if x == Int.MinValue => None
      case x if x < 0             => Some(-x)
      case x                      => Some(x)
    }

    val (n, nextRng) = rng.nextInt

    toPositive(n) match {
      case Some(x) => (x, nextRng)
      case None    => nonNegative(nextRng)
    }
  }

  /**
    * Exercise 6.2
    * */
  @tailrec
  def double(rng: RNG): (Double, RNG) = {
    nonNegative(rng) match {
      case (Int.MaxValue, nextRng)  => double(nextRng)
      case (value, nextRng)         => (value.toDouble / Int.MaxValue, nextRng)
    }
  }

  /**
    * Exercise 6.3
    * intDouble, doubleInt, double3
    * */
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (nextInt, nextRng) = rng.nextInt
    val (nextDouble, nextRng2) = double(nextRng)
    ((nextInt, nextDouble), nextRng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((int, double), nextRng) = intDouble(rng)
    ((double, int), nextRng)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  /**
    * Exercise 6.4
    * */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def gen(acc: List[Int], rng: RNG, size: Int): (List[Int], RNG) = {
      if (size < count) {
        val (elem, nextRng) = rng.nextInt
        gen(elem::acc, nextRng, size + 1)
      }
      else
        (acc, rng)

    }
    gen(List.empty, rng, 0)
  }

  /**
    * Exercise 6.5
    * */
  def doubleViaMap: Rand[Double] = map(nonNegative)(_ / (Int.MaxValue.toDouble + 1))

  /**
    * Exercise 6.6
    * */
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, r1) = ra(rng)
    val (b, r2) = rb(r1)
    (f(a, b), r2)
  }

  /**
    * Exercise 6.7
    * */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
    @tailrec
    def iter(rng: RNG, xs: List[Rand[A]], ys: List[A]): (List[A], RNG) = xs match {
      case h::t =>
        val (a, nextRng) = h(rng)
        iter(nextRng, t, a::ys)
      case Nil  => (ys.reverse, rng)
    }
    iter(rng, fs, List.empty)
  }

  /**
    * Exercise 6.8
    * */
  def flatMap[A, B](r: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, nextRng) = r(rng)
    g(a)(nextRng)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeRand) {
      value =>
        val mod = value % n
        if (value + (n - 1) - mod >= 0) (rng: RNG) => (mod, rng)
        else nonNegativeLessThan(n)
    }
  }

  /**
    * Exercise 6.9
    * map and map2 via flatMap
    * */
  def mapViaFlatMap[A, B](r: Rand[A])(op: A => B): Rand[B] = flatMap(r) {
    a => rng => (op(a), rng)
  }

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(op: (A, B) => C): Rand[C] =
    flatMap(ra) {
      a => flatMap(rb) {
        b => rng => (op(a, b), rng)
      }
    }

}


/**
  * Exercise 6.10
  * Generalised unit, map, map2, flatMap and sequence as members of State class
  * */

object State {
  def sequence[S, A](states: List[State[S, A]]): State[S, List[A]] = {
    def iter(s: S, xs: List[State[S, A]], acc: List[A]): List[A] = xs match {
      case h::t =>
        val (a, next) = h.run(s)
        iter(next, t, a::acc)
      case _    => acc.reverse
    }
    State(s => (iter(s, states, List.empty), s))

  }
  def unit[T, S](a: T): State[S, T] = State(s => (a, s))
}

case class State[S, +A](run: S => (A, S)) {
  import State._

  def map[B](f: A => B): State[S, B] = flatMap(a => unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap( a =>
      sb.flatMap(b =>
        unit(f(a,b))
      )
    )

  def flatMap[B](g: A => State[S, B]): State[S, B] = State(s => {
    val (a, next) = run(s)
    g(a).run(next)
  })
}

object StateMachine {
  /**
    * Exercise 6.11
    * Candy dispenser
    * */

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  type CandyAmount = Int
  type CoinsAmount = Int

  val Locked: Boolean = true
  val Unlocked: Boolean = false

  case class Machine(locked: Boolean, candies: CandyAmount, coins: CoinsAmount)

  def simulateMachine(inputs: List[Input]): State[Machine, (CandyAmount, CoinsAmount)] = {
    State(m => {
      val (_, updated) = modify(handleInputs(inputs)).run(m)
      ((updated.candies, updated.coins), updated)
    })
  }

  def simulateMachine2(inputs: List[Input]): State[Machine, (CandyAmount, CoinsAmount)] = {
    State(machine => {
      val updated = handleInputs(inputs)(machine)
      ((updated.candies, updated.coins), updated)
    })
  }

  @tailrec
  private def handleInputs(inputs: List[Input])( machine: Machine): Machine = inputs match {
    case h::t => handleInputs(t)(handleInput(h)(machine))
    case Nil  => machine
  }

  private def handleInput(input: Input)(machine: Machine): Machine = input match {
    case Coin => handleCoin(machine)
    case Turn => handleTurn(machine)
  }

  private def handleCoin(m: Machine): Machine = m match {
    case m @ Machine(Locked, candies, coins) if candies > 0 => m.copy(locked = Unlocked, candies, coins + 1)
    case anyOther                                           => anyOther
  }

  private def handleTurn(m: Machine): Machine = m match {
    case m @ Machine(Unlocked, candies, _) if candies > 0   => m.copy(Locked, candies = candies - 1)
    case anyOther                                           => anyOther
  }

  private def get[S]: State[S, S] = State(s => (s, s))

  private def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  private def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()



}