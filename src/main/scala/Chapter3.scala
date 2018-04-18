import scala.annotation.tailrec

object Chapter3 {
  sealed trait MyList[+T] {
    def ::[A >: T](h: A): MyList[A]
    def size: Int
  }
  case object Nil extends MyList[Nothing] {
    def ::[A](h: A): MyList[A] = Cons(h, Nil)
    def size: Int = 0
  }

  case class Cons[+T](head: T, tail: MyList[T]) extends MyList[T] {
    def ::[A >: T](h: A): MyList[A] = Cons(h, Cons(head, tail))
    def size: Int = 1 + tail.size
  }

  object MyList {
    def apply[A](elements: A*): MyList[A] = {
      @tailrec
      def iter(xs: MyList[A], rest: A*): MyList[A] = {
        if (rest.nonEmpty) iter(frAppend(rest.head, xs), rest.tail :_*)
        else xs
      }
      iter(Nil, elements:_*)
    }

    def append[A](t: A, list: MyList[A]): MyList[A] = list match {
      case Cons(x, Nil) => x::Cons(t, Nil)
      case Cons(x, xs)  => x::append(t, xs)
      case Nil          => MyList(t)
    }

    /**
      * Exercise 3.2
      */
    def tail[T](MyList: MyList[T]): MyList[T] = MyList match {
      case Cons(_, xs)  => xs
      case _      => Nil
    }

    /**
      * Exercise 3.3
      */
    def setHead[T](h: T, MyList: MyList[T]): MyList[T] = MyList match {
      case Cons(_, xs)  => h::xs
      case Nil          => apply(h)
    }

    /**
      * Exercise 3.4
      * */
    @tailrec
    def drop[A](n: Int, MyList: MyList[A]): MyList[A] = {
      if (n > MyList.size)
        Nil
      else if (n <= 0)
        MyList
      else
        drop(n - 1, tail(MyList))
    }

    /**
      * Exercise 3.5
      * */
    @tailrec
    def dropWhile[A](l: MyList[A])(f: A => Boolean): MyList[A] = l match {
      case Cons(x, xs) if f(x)  => dropWhile(xs)(f)
      case any                  => any
    }

    /**
      * Exercise 3.6
      * */
    def init[A](l: MyList[A]): MyList[A] = {
      @tailrec
      def iter(acc: MyList[A], rest: MyList[A]): MyList[A] = rest match {
        case Cons(_, Nil) => acc
        case Cons(x, xs)  => iter(append(x, acc), xs)
        case _      => acc
      }
      iter(Nil, l)
    }

    def foldRight[A, B](as: MyList[A], z: B)(f: (A, B) => B): B =
      as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }

    /**
      * Exercise 3.7
      * */
    def product(l: MyList[Double]): Double = foldRight(l, 1d)(_ * _)

    /**
      * Exercise 3.9
      * */
    def length[A](l: MyList[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)

    /**
      * Exercise 3.10
      * */
    def foldLeft[A, B](as: MyList[A], z: B)(f: (B, A) => B): B = {
      @tailrec
      def iter(acc: B, xs: MyList[A]): B = xs match {
        case Nil          => acc
        case Cons(x, ds)  => iter(f(acc, x), ds)
      }
      iter(z, as)
    }

    /**
      * Exercise 3.11
      * */
    def flSum(ns: MyList[Int]): Int = foldLeft(ns, 0)(_+_)
    def flProduct(ns: MyList[Double]): Double = foldLeft(ns, 1d)(_*_)
    def flLength[A](ns: MyList[A]): Int = foldLeft(ns, 0)((z, _) => z + 1)

    /**
      * Exercise 3.12
      * */
    def reverse[A](ns: MyList[A]): MyList[A] = foldRight[A, MyList[A]](ns, Nil)((acc, x) => append[A](acc, x))

    /**
      * Exercise 3.13
      * */
    def flFoldRight[A, B](as: MyList[A], z: B)(f: (A, B) => B): B = {
      foldLeft(as, (b: B) => b)((g, a) => b => g(f(a, b)))(z)
    }

    /**
      * Exercise 3.14
      * */
    def frAppend[A](x: A, xs: MyList[A]): MyList[A] = {
      flFoldRight(xs, Cons(x, Nil))((x, as) => Cons(x, as))
    }

    def flAppend[A](x: A, xs: MyList[A]): MyList[A] = ???

    /**
      * Exercise 3.15
      * */
    def flatten[A](xs: MyList[MyList[A]]): MyList[A] = {
      flFoldRight(xs, MyList[A]())((inner, acc) => {
        flFoldRight(inner, acc)((elem, acc) => elem::acc)
      })
    }

    /**
      * Exercise 3.16
      * */
    def addOne(xs: MyList[Int]): MyList[Int] = {
      map(xs)(x => x + 1)
    }

    /**
      * Exercise 3.17
      * */
    def doubleToString(xs: MyList[Double]): MyList[String] = {
      map(xs)(x => x.toString)
    }

    /**
      * Exercise 3.18
      * */
    def map[A, B](xs: MyList[A])(f: (A => B)): MyList[B] = {
      @tailrec
      def iter(l: MyList[A], acc: MyList[B]): MyList[B] = l match {
        case Cons(h, t) => iter(t, frAppend(f(h), acc))
        case Nil        => acc
      }
      iter(xs, MyList[B]())
    }

    /**
      * Exercise 3.19
      * */
    def filter[A](xs: MyList[A])(p: A => Boolean): MyList[A] = {
      @tailrec
      def iter(l: MyList[A], acc: MyList[A]): MyList[A] = l match {
        case Cons(h, t) =>
          if (p(h)) iter(t, frAppend(h, acc))
          else iter(t, acc)
        case Nil => acc
      }
      iter(xs, Nil)
   }

    /**
      * Exercise 3.20
      * */
    def flatMap[A, B](as: MyList[A])(f: A => MyList[B]): MyList[B] = {
      flatten(map(as)(f))
    }

    /**
      * Exercise 3.21
      * */
    def flmFilter[A](l: MyList[A])(p: A => Boolean): MyList[A] = {
      flatMap(l)(x => {
        if (p(x)) MyList(x)
        else Nil
      })
    }

    /**
      * Exercise 3.22
      * */
    def combine(listOne: MyList[Int], listTwo: MyList[Int]): MyList[Int] = {
      @tailrec
      def iter(as: MyList[Int], bs: MyList[Int], acc: MyList[Int]): MyList[Int] = (as, bs) match {
        case (Cons(ah, at), Cons(bh, bt)) => iter(at, bt, append(ah + bh, acc))
        case _                            => acc
      }
      iter(listOne, listTwo, Nil)
    }

    /**
      * Exercise 3.23
      * */
    def zipWith[A, B](a: MyList[A], b: MyList[A])(f: (A, A) => B): MyList[B] = {
      @tailrec
      def iter(as: MyList[A], bs: MyList[A], acc: MyList[B]): MyList[B] = (as, bs) match {
        case (Cons(ah, at), Cons(bh, bt)) => iter(at, bt, append(f(ah, bh), acc))
        case _                            => acc
      }
      iter(a, b, Nil)
    }

    /**
      * Exercise 3.24 (marked as hard, however classical back propagation approach works quite well here.)
      * */
    def hasSubsequence[A](list: MyList[A], pattern: MyList[A]): Boolean = {
      @tailrec
      def iter(sup: MyList[A], sub: MyList[A]): Boolean = (sup, sub) match {
        case (Cons(ah, at), Cons(bh, bt)) =>
          if (ah == bh)
            iter(at, bt)
          else
            iter(at, bh::bt)
        case (_, Nil)                   => true
        case (Nil, _)                   => false
      }
      iter(list, pattern)
    }
  }

  object Trees {
    sealed trait Tree[+A]
    case class Leaf[+A](value: A) extends Tree[A]
    case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

    object Tree {

      /**
        * Exercise 3.25
        * */
      def size[A](tree: Tree[A]): Int = tree match {
        case Branch(l, r) => size(l) + size(r)
        case Leaf(_)      => 1
      }

      /**
        * Exercise 3.26
        * */
      def maximum(tree: Tree[Int]): Int = tree match {
        case Branch(l, r) => maximum(l) max maximum(r)
        case Leaf(v) => v
      }

      /**
        * Exercise 3.27
        */
      def depth[A](tree: Tree[A]): Int = {
        def iter(t: Tree[A], acc: Int): Int = t match {
          case Branch(l, r) => iter(l, acc + 1) max iter(r, acc + 1)
          case Leaf(_)      => acc
        }
        iter(tree, 0)
      }

      /**
        * Exercise 3.28
        * */
      def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
        case Branch(l, r) => Branch(map(l)(f), map(r)(f))
        case Leaf(v)      => Leaf(f(v))
      }

      /**
        * Exercise 3.29
        * */
      def fold[A, B](tree: Tree[A], z: B)(f: (A , B) => B): B = tree match {
        case Branch(l, r) => fold(r, fold(l, z)(f))(f)
        case Leaf(v) => f(v, z)
      }

      def flDepth[A](tree: Tree[A]): Int = ???
      def flMap[A, B](tree: Tree[A])(f: A => B) : Tree[B] = ???

      def flSize[A](tree: Tree[A]): Int = fold(tree, 0)((_, x) => x + 1)
      def flMax(tree: Tree[Int]): Int = fold(tree, 0)((a, b) => a max b)
    }
  }
}
