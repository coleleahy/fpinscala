package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A] {
  override def toString: String = {
    @annotation.tailrec
    def loop(n: Int, l: List[A], acc: String): String = {
      l match {
        case Nil => s"${acc}Nil"
        case Cons(_, _) if n > 6 => s"${acc}..."
        case Cons(h, t) => loop(n + 1, t, s"${acc}$h -> ")
      }
    }

    loop(0, this, "")
  }
}

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def applyStackUnsafe[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, applyStackUnsafe(as.tail: _*))

  def apply[A](as: A*): List[A] = {
    @annotation.tailrec
    def loop(n: Int, acc: List[A]): List[A] =
      n match {
        case 0 => acc
        case _ => loop(n - 1, Cons(as(n - 1), acc))
      }

    loop(as.length, Nil)
  }

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => throw new RuntimeException("No tail of empty list")
      case Cons(_, t) => t
    }

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => throw new RuntimeException("No head of empty list")
      case Cons(_, t) => Cons(h, t)
    }

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    (l, n) match {
      case (_, 0) => l
      case (Nil, _) => Nil
      case (Cons(_, t), _) => drop(t, n - 1)
    }

  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }

  def reverse[A](l: List[A]): List[A] = {
    @annotation.tailrec
    def loop(l: List[A], acc: List[A]): List[A] =
      l match {
        case Nil => acc
        case Cons(h, t) => loop(t, Cons(h, acc))
      }

    loop(l, Nil)
  }

  def initStackUnsafe[A](l: List[A]): List[A] =
    l match {
      case Nil => throw new RuntimeException("No init of empty list")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, initStackUnsafe(t))
    }

  def init[A](l: List[A]): List[A] =
    reverse(tail(reverse(l)))

  // foldRight: f(x0, f(x1, f(x2, z)))
  // foldLeft:  f(f(f(z, x0), x1), x2)))
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def foldRightLazy[A, B](as: List[A], z: B)(f: (A, => B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRightLazy(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def multiply(n: Double, acc: Double): Double = {
    println(n.toString)

    if (n == 0.0)
      0.0
    else
      n * acc
  }

  def multiplyLazy(n: Double, acc: => Double): Double = {
    println(n.toString)

    if (n == 0.0)
      0.0
    else
      n * acc
  }

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(multiply)

  def product2Lazy(ns: List[Double]) =
    foldRightLazy(ns, 1.0)(multiplyLazy)

  def length[A](l: List[A]): Int =
    foldRight(l, 0) { (_, acc) => acc + 1 }

  def reverseViaFoldLeft[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A]) { (acc, x) => Cons(x, acc) }

  // f(x0, f(x1, z))
  // g(x1, g(x0, z))
  // g(x1, f(z, x0))
  // f(f(z, x0), x1)
  def foldLeftViaRight[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(as), z) { (a, b) => f(b, a) }

  // f(f(z, x0), x1)
  // g(g(z, x1), x0)
  // g(f(x1, z), x0)
  // f(x0, f(x1, z))
  def foldRightViaLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z) { (b, a) => f(a, b) }

  def appendViaFold[A](xs: List[A], ys: List[A]): List[A] =
    foldRight(xs, ys)(Cons(_, _))

  def flatten[A](ls: List[List[A]]): List[A] =
    foldRight(ls, Nil: List[A])(append)

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B]) { (a, bs) => Cons(f(a), bs)}

  def filter[A](as: List[A])(p: A => Boolean): List[A] =
    foldRight(as, Nil: List[A]) { (a, acc) =>
      if (p(a))
        Cons(a, acc)
      else
        acc
    }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    flatten(map(as)(f))

  def zip[A, B](as: List[A], bs: List[B]): List[(A, B)] = {
    @annotation.tailrec
    def loop(as: List[A], bs: List[B], acc: List[(A, B)]): List[(A, B)] =
      (as, bs) match {
        case (Nil, Nil) => acc
        case (Cons(ha, ta), Cons(hb, tb)) => loop(ta, tb, Cons((ha, hb), acc))
        case _ => throw new RuntimeException("Cannot zip unequal-length lists")
      }

    reverse(loop(as, bs, Nil))
  }

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] =
    map(zip(as, bs)) {
      case (a, b) => f(a, b)
    }

  @annotation.tailrec
  def startsWith[A](x: List[A], y: List[A]): Boolean =
    (x, y) match {
      case (_, Nil) => true
      case (Cons(hx, tx), Cons(hy, ty)) if hx == hy => startsWith(tx, ty)
      case _ => false
    }

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    sup match {
      case Nil => sub == Nil
      case _ if startsWith(sup, sub) => true
      case Cons(_, t) => hasSubsequence(t, sub)
    }
}