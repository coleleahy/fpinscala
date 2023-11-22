package fpinscala.laziness

import annotation._
import scala.collection.mutable.ListBuffer

sealed trait Stream[+A] {
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

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z))) { (a, acc) =>
      lazy val memo = acc
      val b = f(a, memo._1)
      (b, cons(b, memo._2))
    }._2
//    this match {
//      case Empty =>
//        Stream(z)
//      case Cons(h, t) =>
//        val rest = t().scanRight(z)(f)
//        val hh = rest.asInstanceOf[Cons[B]].h()
//        cons(f(h(), hh), rest)
//    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] =
    this match {
      case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
      case _ => empty
    }

  def takeViaUnfold(n: Int): Stream[A] =
     unfold((this, n)) {
      case (Cons(h, t), i) if i > 1 =>
        Some((h(), (t(), i - 1)))
      case (Cons(h, t), 1) =>
        Some((h(), (empty, 0))) // Passing empty instead of t() avoids forcing t unnecessarily.
      case _ =>
        None
    }

  @tailrec
  final def drop(n: Int): Stream[A] =
    this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _ => this
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

  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Cons(h, t) =>
        val head = h()
        if (p(head)) cons(head, t() takeWhile p)
        else empty
      case Empty =>
        empty
    }
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) =>
        val hh = h()
        if (p(hh))
          Some((hh, t()))
        else
          None
      case _ =>
          None
    }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true) { (a, acc) => p(a) && acc }

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](empty) { (a, acc) =>
      if (p(a))
        cons(a, acc)
      else
        empty
    }

  def headOption: Option[A] =
    foldRight[Option[A]](None) { (a, _) => Some(a) }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B]) { (a, acc) =>
      cons(f(a), acc)
    }

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) =>
        Some((f(h()), t()))
      case _ =>
        None
    }

  def zipWith[B, C](that: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, that)) {
      case (Cons(hThis, tThis), Cons(hThat, tThat)) =>
        Some((f(hThis(), hThat()), (tThis(), tThat())))
      case _ =>
        None
    }

  def zipAll[B](that: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, that)) {
      case (Cons(hThis, tThis), Cons(hThat, tThat)) =>
        Some((Some(hThis()), Some(hThat())), (tThis(), tThat()))
      case (Cons(hThis, tThis), Empty) =>
        Some((Some(hThis()), None), (tThis(), that))
      case (Empty, Cons(hThat, tThat)) =>
        Some((None, Some(hThat())), (this, tThat()))
      case (Empty, Empty) =>
        None
    }

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A]) { (a, acc) =>
      if (p(a))
        cons(a, acc)
      else
        acc
    }

  def append[B >: A](other: => Stream[B]): Stream[B] =
    foldRight(other) { (a, acc) =>
      cons(a, acc)
    }

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B]) { (a, acc) =>
      f(a) append acc
    }

  def startsWith[B](that: Stream[B]): Boolean =
    zipAll(that)
      .takeWhile {
        case (_, Some(_)) => true
        case _ => false
      }
      .forAll {
        case (thisH, thatH) => thisH == thatH
      }

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case s @ Cons(_, t) =>
        Some((s, t()))
      case _ =>
        None
    } append Stream(empty)

  def hasSubsequence[B](that: Stream[B]): Boolean =
    // tails.exists(_.startsWith(that))
    tails exists (_ startsWith that)
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

  val ones: Stream[Int] = cons(1, ones)

  def constant[A](a: A): Stream[A] = {
    // lazy val s: Stream[A] = cons(a, s)
    lazy val s: Stream[A] = Cons(() => a, () => s)
    s
  }

  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  val fibs: Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] =
      cons(a, go(b, a + b))

    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z)
      .map { case (a, s) => cons(a, unfold(s)(f)) }
      .getOrElse(empty[A])

  val onesViaUnfold: Stream[Int] =
    unfold(1) { i => Some(i, i) }

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a) { s => Some(s, s) }

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n) { s => Some(s, s + 1) }

  val fibsViaUnfold: Stream[Int] =
    unfold((0, 1)) { case (j, k) => Some(j, (k, j + k)) }
}