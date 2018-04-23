import scala.annotation.tailrec

object Chapter4 {
  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B]
    def flatMap[B](f: A => Option[B]): Option[B]
    def getOrElse[B >: A](value: => B): B
    def orElse[B >: A](valueOpt: => Option[B]): Option[B]
    def filter(f: A => Boolean): Option[A]
  }

  /**
    * Exercise 4.1
    * */
  case object None extends Option[Nothing] {
    def map[B](f: Nothing => B): Option[B] = None
    def flatMap[B](f: Nothing => Option[B]): Option[B] = None
    def getOrElse[B >: Nothing](value: => B): B = value
    def orElse[B >: Nothing](valueOpt: => Option[B]): Option[B] = valueOpt
    def filter(f: Nothing => Boolean): Option[Nothing] = None
  }

  case class Some[+A](get: A) extends Option[A] {
    def map[B](f: A => B): Option[B] = Some(f(get))
    def flatMap[B](f: A => Option[B]): Option[B] = f(get)
    def getOrElse[B >: A](value: => B): B = get
    def orElse[B >: A](valueOpt: => Option[B]): Option[B] = this
    def filter(f: A => Boolean): Option[A] = {
      if (f(get)) this
      else None
    }
  }

  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  /**
    * Exercise 4.2
    * */
  def variance(xs: Seq[Double]): Option[Double] = {
    for {
      m <- mean(xs)
      v <- mean(xs.map(x => math.pow(x - m, 2)))
    } yield v
  }

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  /**
    * Exercise 4.3
    * */
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = { a.flatMap(x => b.map(f(x, _))) }

  /**
    * Exercise 4.4
    * */
  def sequence[A](a: List[Option[A]]): Option[List[A]] =  {
    @tailrec
    def iter(xs: List[Option[A]], acc: List[A]): Option[List[A]] = xs match {
      case h::t => h match {
        case Some(v) => iter(t, acc :+ v)
        case None    => iter(t, acc)
      }
      case Nil =>
        if (acc.nonEmpty)
          Some(acc)
        else
          None
    }
    iter(a, Nil)
  }

  /**
    * Exercise 4.5
    * */
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    @tailrec
    def iter(xs: List[A], acc: List[B]): Option[List[B]] = xs match {
      case h::t => f(h) match {
        case Some(v)  => iter(t, acc :+ v)
        case None     => iter(t, acc)
      }
      case Nil  =>
        if (acc.nonEmpty) Some(acc)
        else None
    }
    iter(a, Nil)
  }

  /**
    * Exercise 4.6
    * */
  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B]
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]
    def orElse[EE >: E, B >: A](that: Either[EE, B]): Either[EE, B]
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]

  }

  case class Left[+E, +A](value: E) extends Either[E, A] {
    def map[B](f: A => B): Either[E, B] = Left(value)
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = Left(value)
    def orElse[EE >: E, B >: A](that: Either[EE, B]): Either[EE, B] = that
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = Left(value)
  }

  case class Right[+E, +A](value: A) extends Either[E, A] {
    def map[B](f: A => B): Either[E, B] = Right(f(value))
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = f(value)
    def orElse[EE >: E, B >: A](that: Either[EE, B]): Either[EE, B] = this
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = b map (f(value, _))
  }

  /**
    * Exercise 4.7
    * */
  def eitherSequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    def iter(xs: List[Either[E, A]], acc: List[A]): Either[E, List[A]] = xs match {
      case h::t => h match {
        case Left(err) => Left(err)
        case Right(v) => iter(t, acc :+ v)
      }
      case Nil  => Right(acc)
    }
    iter(es, Nil)
  }

  def eitherTraverse[E, A, B](es: List[Either[E, A]])(f: A => Either[E, B]): Either[E, List[B]] = {
    def iter(xs: List[Either[E, A]], acc: List[B]): Either[E, List[B]] = xs match {
      case h::t => h flatMap f match {
        case Left(e)  => Left(e)
        case Right(v) => iter(t, acc :+ v)
      }
      case Nil  => Right(acc)
    }
    iter(es, Nil)
  }

  /**
    * Exercise 4.8
    * */
  case class Person(name: Name, age: Age)
  sealed class Name(val value: String)
  sealed class Age(val value: Int)

  def mkName(name: String): Either[String, Name] = {
    if (name == "" || name == null) Left("Name is empty")
    else Right(new Name(name))
  }

  def mkAge(age: Int): Either[String, Age] = {
    if (age < 0) Left("Age is out of range")
    else Right(new Age(age))
  }

  def mkPerson(name: String, age: Int): Either[String, Person] = (mkName(name), mkAge(age)) match {
    case (Right(n), Right(a))   => Right(Person(n, a))
    case (Left(ne), Left(ae))   => Left(s"$ne and $ae")
    case (Left(ne), _)          => Left(ne)
    case (_, Left(ae))          => Left(ae)
  }
}

