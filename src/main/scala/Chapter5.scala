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
  }

  case object Empty extends Stream[Nothing] {
    def toList: List[Nothing] = Nil
    def take(n: Int): Stream[Nothing] = Empty
    def drop(n: Int): Stream[Nothing] = Empty
    def takeWhile(p: Nothing => Boolean): Stream[Nothing] = Empty
    def forAll(p: Nothing => Boolean): Boolean = true
    def map[B](f: Nothing => B): Stream[B] = Empty
    def filter(p: Nothing => Boolean): Stream[Nothing] = Empty
    def append[B >: Nothing](a: => B): Stream[B] = Empty
    def flatMap[B](f: Nothing => Stream[B]): Stream[B] = Empty
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
  }


  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  }
}
