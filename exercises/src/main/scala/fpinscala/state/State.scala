package fpinscala.state

import annotation.tailrec
import scala.collection.mutable.ListBuffer

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  // Can think of Rand[A] as signifying "a random A" but that's not quite accurate.
  // Rather, Rand[A] signifies a *state action* that transforms one state (i.e. one RNG)
  // into another, emitting an A as part of the transition.
  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
    }

  def sequenceStackUnsafe[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs match {
      case Nil =>
        rng => (Nil, rng)
      case h :: t =>
        map2(h, sequenceStackUnsafe(t))(_ :: _)
    }

  def sequenceTailrec[A](fs: List[Rand[A]]): Rand[List[A]] = {
    def loop(fs: List[Rand[A]], acc: Rand[List[A]]): Rand[List[A]] =
      fs match {
        case Nil => acc
        case h :: t => loop(t, map2(h, acc)(_ :: _))
      }

    loop(fs.reverse, rng => (Nil, rng))
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List.empty[A])) { (f, acc) =>
      map2(f, acc)(_ :: _)
    }

  // Whereas map(f)(g) would yield a Rand[Rand[B]] = RNG => (RNG => (B, RNG)),
  // flatMap(f)(g) will yield a Rand[B] = RNG => (B, RNG).
  // Input: r0
  // f(r0) = (a, r1)
  // g(a)(r1) = (b, r2)
  // Output: (b, r2)
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    r0 => {
      val (a, r1) = f(r0)
      g(a)(r1)
    }

  // Shows that flatMap is "as strong as (or stronger than)" map.
  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s) { (a: A) => unit(f(a)) }

  // Shows that flatMap is "as strong as (or stronger than)" map2.
  // Input: r0
  // ra(r0) = (a, r1)
  // rb(r1) = (b, r2)
  // Output: (f(a, b), r2)
  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) { a =>
      map(rb) { b =>
        f(a, b)
      }
    }

  val int: Rand[Int] = _.nextInt

  // Generates number between 0 and Int.maxValue inclusive.
  // Handles corner case where RNG.nextInt returns Int.minValue, which doesn't have a positive counterpart.
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt

    if (i < 0)
      (-(i + 1), r)
    else
      (i, r)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n

      if (i + (n - 1) - mod >= 0)
        unit(mod)
      else
        nonNegativeLessThan(n)
    }

  val double: Rand[Double] =
    map(nonNegativeInt) { n =>
      (-n.toDouble / Int.MinValue)
    }

  // Generates double between 0 and 1, not including 1.
  def doubleImperative(rng: RNG): (Double, RNG) = {
    var (n, r) = nonNegativeInt(rng)

    // Since n was generated as a non-negative integer, multiplying by -1 yields a
    // non-positive integer no smaller than -Int.MaxValue, which itself is strictly
    // larger than Int.MinValue.
    n *= -1

    // Since n is now a non-positive integer larger than Int.MinValue, dividing n.toDouble
    // by Int.MinValue yields a non-negative real smaller than 1.
    (n.toDouble / Int.MinValue, r)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r) = rng.nextInt
    val (d, rr) = double(r)

    ((i, d), rr)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), r) = intDouble(rng)

    ((d, i), r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)

    ((d1, d2, d3), r3)
  }

  def ints(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def intsTailrec(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def loop(n: Int, acc: (List[Int], RNG)): (List[Int], RNG) =
      if (n <= 0)
        acc
      else {
        val (xs, r) = acc
        val (x, rr) = r.nextInt
        loop(n - 1, (x :: xs, rr))
      }

    loop(count, (Nil, rng))
  }

  def intsImperative(count: Int)(rng: RNG): (List[Int], RNG) = {
    val buf = ListBuffer.empty[Int]
    var _rng = rng

    (1 to count).foreach { i =>
      val (i, r) = _rng.nextInt
      buf.append(i)
      _rng = r
    }

    (buf.toList, _rng)
  }
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    ???
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    ???
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    ???
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
