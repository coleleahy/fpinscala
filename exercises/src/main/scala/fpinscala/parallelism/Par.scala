package fpinscala.parallelism

import java.util.concurrent._
import language.implicitConversions

object Par {
  type Par[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = _ => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.

  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit {
      new Callable[A] {
        def call = a(es).get
      }
    }

  def lazyUnit[A](a: => A): Par[A] =
    fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    es => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get)) // This implementation of `map2` does _not_ respect timeouts, and eagerly waits for the returned futures. This means that even if you have passed in "forked" arguments, using this map2 on them will make them wait. It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
    }

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] = {
    val g = f.curried
    val cToD = map2(a, b) { (a, b) => g(a)(b) }
    map2(cToD, c) { (cToD, c) => cToD(c) }
  }

  def map4[A, B, C, D, E](
    a: Par[A],
    b: Par[B],
    c: Par[C],
    d: Par[D]
  )(f: (A, B, C, D) => E): Par[E] = {
    val g = f.curried
    val dToE = map3(a, b, c) { (a, b, c) => g(a)(b)(c) }
    map2(dToE, d) { (dToE, d) => dToE(d) }
  }

  def map5[A, B, C, D, E, F](
    a: Par[A],
    b: Par[B],
    c: Par[C],
    d: Par[D],
    e: Par[E]
  )(f: (A, B, C, D, E) => F): Par[F] = {
    val g = f.curried
    val eToF = map4(a, b, c, d) { (a, b, c, d) => g(a)(b)(c)(d) }
    map2(eToF, e) { (eToF, e) => eToF(e) }
  }

  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] =
    fork {
      if (as.isEmpty)
        unit(Vector.empty)
      else if (as.length == 1)
        map(as.head)(Vector.apply(_))
      else {
        val (l, r) = as.splitAt(as.length / 2)
        map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
      }
    }

  def sequence[A](as: List[Par[A]]): Par[List[A]] =
    map(sequenceBalanced(as.toIndexedSeq))(_.toList)

  def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] =
    fork {
      sequence(as.map(asyncF(f)))
    }

  def parFilterOldOld[A](as: List[A])(p: A => Boolean): Par[List[A]] = {
    val parListPairs = parMap(as) { a => (a, p(a)) }
    map(parListPairs) { pairs => pairs.filter(_._2).map(_._1) }
  }

  def parFilterOld[A](as: List[A])(p: A => Boolean): Par[List[A]] = {
    val pars: List[Par[Option[A]]] =
      as.map {
        asyncF { a =>
          Some(a).filter(p)
        }
      }

    map(sequence(pars))(_.flatten)
  }

  def parFilter[A](as: List[A])(p: A => Boolean): Par[List[A]] = {
    val parOptions = parMap(as) { a => Some(a).filter(p) }
    map(parOptions)(_.flatten)
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] =
    es => {
      val aa = run(es)(a).get()
      run(es)(f(aa))
    }

  def choiceN[A](cond: Par[Int])(choices: List[Par[A]]): Par[A] =
    flatMap(cond)(choices(_))

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    flatMap(cond) {
      case true => t
      case false => f
    }

  /* Gives us infix syntax for `Par`. */
  implicit class ParOps[A](a: Par[A]) {
    def map2[B, C](b: Par[B])(f: (A, B) => C): Par[C] =
      Par.map2(a, b)(f)
  }

  /*
  map(y)(g) == map(y)(g)                      // Identity.
  map(map(y)(g))(id) == map(y)(g)             // By law "map(y)(id) == y", taking map(y)(g) as "y".
  map(map(y)(g))(id) == map(y)(id compose g)  // Because id compose g is just g.
  map(map(y)(g))(f) == map(y)(f compose g)    // By abstracting away from "id" on the previous line. Why can we do this? Because map can't possibly behave differently for "id" than for any other function "f", since map[A, B] is parametrically polymorphic (which means it couldn't possibly follow different code paths based on the details of the function of type A => B that's passed in).
   */
}

object Examples {
  import Par._
  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else {
      val (l,r) = ints.splitAt(ints.length/2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

  def sortPar(parList: Par[List[Int]]) =
    map(parList)(_.sorted)

  def mapReduce[A, B](as: Seq[A])(f: A => B)(g: (B, B) => B): Par[B] =
    map(parMap(as.toList)(f))(_.reduce(g))

  def wordCount(paragraphs: Seq[String]): Par[Int] =
    mapReduce(paragraphs)(_.split(" ").length)(_ + _)
}
