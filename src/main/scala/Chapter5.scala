object Chapter5 {
  sealed trait Stream[+A] {
    def toList: List[A]
    def take(n: Int): Stream[A]
    def drop(n: Int): Stream[A]
    def takeWhile(p: A => Boolean): Stream[A]
    def forAll(p: A => Boolean): Boolean
    def foldRight[B](z: => B)(f: (A, => B) => B): B = z
    def exists(p: A => Boolean): Boolean = false
    def frTakeWhile(p: A => Boolean): Stream[A] = Empty
    def headOption: Option[A] = None
    def map[B](f: A => B): Stream[B]
    def filter(p: A => Boolean): Stream[A]
    def append[B >: A](a: => B): Stream[B]
    def flatMap[B](f: A => Stream[B]): Stream[B]
    def unfoldMap[B](f: A => B): Stream[B]
    def unfoldTake(n: Int): Stream[A]
    def unfoldTakeWhile(f: A => Boolean): Stream[A]
    def unfoldZipWith[C](that: Stream[C]): Stream[(A, C)]
    def startsWith[T >: A](that: Stream[T]): Boolean
    def unfoldZipAll[T >: A, C](that: Stream[C], missingThis: T, missingThat: C): Stream[(T, C)] = {
      import Stream._
      val thisStream: Stream[A] = this
      unfold(that, thisStream) {
        case (Cons(thatH, thatT), Cons(thisH, thisT))   =>
          Some((thisH(), thatH()), (thatT(), thisT()))
        case (Cons(thatH, thatT), _)                    =>
          Some((missingThis, thatH()), (thatT(), Empty))
        case (_, Cons(thisH, thisT))                    =>
          Some((thisH(), missingThat), (Empty, thisT()))
        case any                                        =>
          None
      }
    }

  }

  case object Empty extends Stream[Nothing] {
    def toList: List[Nothing]                                   = Nil
    def take(n: Int): Stream[Nothing]                           = Empty
    def drop(n: Int): Stream[Nothing]                           = Empty
    def takeWhile(p: Nothing => Boolean): Stream[Nothing]       = Empty
    def forAll(p: Nothing => Boolean): Boolean                  = true
    def map[B](f: Nothing => B): Stream[B]                      = Empty
    def filter(p: Nothing => Boolean): Stream[Nothing]          = Empty
    def append[B >: Nothing](a: => B): Stream[B]                = Empty
    def flatMap[B](f: Nothing => Stream[B]): Stream[B]          = Empty
    def unfoldMap[B](f: Nothing => B): Stream[B]                = Empty
    def unfoldTake(n: Int): Stream[Nothing]                     = Empty
    def unfoldTakeWhile(f: Nothing => Boolean): Stream[Nothing] = Empty
    def unfoldZipWith[C](that: Stream[C]): Stream[(Nothing, C)] = Empty
    def startsWith[A](that: Stream[A]): Boolean                 = false
  }

  case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A] {
    /**
      * Exercise 5.1
      **/
    def toList: List[A] = {
      def iter(stream: Stream[A], acc: List[A]): List[A] = stream match {
        case Cons(h, t) => iter(t(), acc :+ h())
        case Empty      => acc
      }
      iter(this, Nil)
    }

    /**
      * Exercise 5.2
      * */
    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 0 => Cons(h, () => t().take(n - 1))
      case _ => Empty
    }

    def drop(n: Int): Stream[A] = this match {
      case Cons(_, t) if n > 0  => t().drop(n - 1)
      case c: Cons[A] if n == 0 => c
      case _ => Empty
    }

    /**
      * Exercise 5.3
      * */
    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
      case _ => Empty
    }

    override def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }


    override def exists(p: A => Boolean): Boolean = {
      foldRight(false)((a, b) => p(a) || b)
    }

    /**
      * Exercise 5.4
      * */
    def forAll(p: A => Boolean): Boolean = this match {
      case Cons(h, t) => if (p(h())) t().forAll(p) else false
      case _          => true
    }

    /**
      * Exercise 5.5
      * */
    override def frTakeWhile(p: A => Boolean): Stream[A] = {
      foldRight[Stream[A]](Stream())((a, z) => {
        if (p(a)) Cons(() => a, () => z)
        else z
      })
    }

    /**
      * Exercise 5.6
      **/
    override def headOption: Option[A] = this match {
      case Cons(h, t) => t() match {
        case Empty  => Some(h())
        case stream => stream.headOption
      }
      case _ => None
    }

    /**
      * Exercise 5.7
      * */
    def map[B](f: A => B): Stream[B] = {
      foldRight(Stream[B]())((elem,acc) => {
        Cons(() => f(elem), () => acc)
      })
    }

    def filter(p: A => Boolean): Stream[A] = {
      foldRight(Stream[A]())((elem, acc) => {
        if (p(elem)) Cons(() => elem, () => acc)
        else acc
      })
    }

    def append[B >: A](elem: => B): Stream[B] = {
      foldRight(Cons(() => elem, () => Empty))((e, acc) => Cons(() => e, () => acc))
    }

    def flatMap[B](f: A => Stream[B]): Stream[B] = {
      foldRight(Stream[B]())((e, acc) => {
        f(e).foldRight(acc)((e2, acc2) => Cons(() => e2, () => acc2))
      })
    }

    /**
      * Exercise 5.13
      * map, take, takeWhile, zipWith, zipAll in terms of unfold
      * */

    def unfoldGeneral[B](f: A => B)(p: A => Boolean): Stream[B] = {
      import Stream._
      val stream: Stream[A] = this
      unfold(stream)({
        case Cons(h, t) if p(h()) => Some((f(h()), t()))
        case _                    => None
      })
    }

    def unfoldMap[B](f: A => B): Stream[B] = unfoldGeneral(f)(_ => true)

    def unfoldTake(n: Int): Stream[A] = {
      import Stream._
      val stream: Stream[A] = this
      unfold((stream, n))(state => {
        val (stream, number) = state
        stream match {
          case Cons(h, t) if number > 0 => Some((h(), (t(), number - 1)))
          case _                        => None
        }
      })
    }

    def unfoldTakeWhile(f: A => Boolean): Stream[A] = unfoldGeneral(identity)(f)

    def unfoldZipWith[C](that: Stream[C]): Stream[(A, C)] = {
      import Stream._
      val thisStream: Stream[A] = this
      unfold((that, thisStream))(state => {
        val (that, thisStream) = state
        (that, thisStream) match {
          case (Cons(thatH, thatT), Cons(thisH, thisT)) => Some((thisH(), thatH()), (thatT(), thisT()))
          case _                                        => None
        }
      })
    }

    def startsWith[T >: A](that: Stream[T]): Boolean =
      unfoldZipAll(that, -1, -2)
        .takeWhile(pair => pair._2 != -2)
        .forAll(pair => pair._1 == pair._2)
  }


  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

    /**
      * Exercise 5.8
      * */
    def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

    /**
      * Exercise 5.9
      * */
    def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

    /**
      * Exercise 5.10
      * */
    def fibs: Stream[Int] = {
      def fib(n: Int):Stream[Int] = Stream.cons(Chapter2.fib(n), fib(n + 1))
      fib(0)
    }

    /**
      * Exercise 5.11
      * */
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
      f(z) match {
        case Some((elem, state))  => Stream.cons(elem, unfold(state)(f))
        case None                 => Stream.empty
      }
    }

    /**
      * Exercise 5.12
      * fibs, from, constant and ones in terms of unfold
      * */
    def unfoldFibs: Stream[Int] = {
      import Chapter2.fib
      unfold(0)(s => Some((fib(s), s + 1)))
    }

    def unfoldFrom(n: Int): Stream[Int] = unfold(n)(s => Some((s, s + 1)))
    def unfoldConstant[A](a: A): Stream[A] = unfold(a)(_ => Some((a, a)))
    def unfoldOnes: Stream[Int] = unfold(1)(_ => Some((1, 1)))

  }
}
