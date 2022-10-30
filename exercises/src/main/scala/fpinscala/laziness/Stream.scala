package fpinscala.laziness

import annotation._
import scala.collection.mutable.ListBuffer

trait Stream[+A] {
  import Stream._

  def toList: List[A] = {
    val buf = ListBuffer.empty[A]

    @tailrec
    def go(s: Stream[A]): List[A] =
      s match {
        case Empty => buf.toList
        case Cons(h, t) => {
          buf += h()
          go(t())
        }
      }

    go(this)
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] =
    (n, this) match {
      case (0, _) | (_, Empty) => empty[A]
      case (_, Cons(h, t)) => cons(h(), t().take(n - 1))
    }

  @tailrec
  final def drop(n: Int): Stream[A] =
    (n, this) match {
      case (0, _) | (_, Empty) => this
      case (_, Cons(_, t)) => t().drop(n - 1)
    }

  def drop2(n: Int): Stream[A] = {
    @tailrec
    def go(n: Int, s: Stream[A]): Stream[A] =
      (n, s) match {
        case (0, _) | (_, Empty) => s
        case (_, c: Cons[A]) => go(n - 1, c.t())
      }

    go(n, this)
  }

  def takeWhile(p: A => Boolean): Stream[A] = ???

  def forAll(p: A => Boolean): Boolean = ???

  def headOption: Option[A] = ???

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = ???
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def from(n: Int): Stream[Int] = ???

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???
}