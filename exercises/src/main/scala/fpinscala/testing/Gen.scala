package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors, ExecutorService}
import scala.annotation.tailrec

// trait RNG { def nextInt: (Int, RNG) }
//
// case class State[S, +A](run: S => (A, S)) { ... }
//
// case class Gen[A](sample: State[RNG, A]) { ... }
//
// sealed trait Result { def isFalsified: Boolean }
// case object Passed extends Result { ... }
// case class Falsified(failedCase: FailedCase, successCount: SuccessCount) extends Result { ... }
//
// case class Prop(run: (TestCases, RNG) => Result) { ... }
case class Prop(run: (TestCases, RNG, Lineage, MaxSize) => Result) {
  def &&(that: Prop): Prop = Prop {
    (testCases, rng, lineage, maxSize) => {
      val thisLineage = lineage.map(_ + "L")
      val thatLineage = lineage.map(_ + "R")

      (this.run(testCases, rng, thisLineage, maxSize), that.run(testCases, rng, thatLineage, maxSize)) match {
        case (Falsified(thisFailedCase, thisSuccessCount), Falsified(thatFailedCase, thatSuccessCount)) =>
          Falsified(
            s"$thisFailedCase, $thatFailedCase",
            thisSuccessCount.min(thatSuccessCount)
          )
        case (f: Falsified, Passed) =>
          f
        case (Passed, f: Falsified) =>
          f
        case (Passed, Passed) =>
          Passed
      }
    }
  }

  def ||(that: Prop): Prop = Prop {
    (testCases, rng, lineage, maxSize) => {
      val thisLineage = lineage.map(_ + "L")
      val thatLineage = lineage.map(_ + "R")

      (this.run(testCases, rng, thisLineage, maxSize), that.run(testCases, rng, thatLineage, maxSize)) match {
        case (Passed, _) =>
          Passed
        case (_, Passed) =>
          Passed
        case (Falsified(thisFailedCase, thisSuccessCount), Falsified(thatFailedCase, thatSuccessCount)) =>
          Falsified(
            s"$thisFailedCase, $thatFailedCase",
            thisSuccessCount.min(thatSuccessCount)
          )
      }
    }
  }
}

object Prop {
  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int
  type Lineage = Option[String]
  type MaxSize = Int

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    override val isFalsified: Boolean = false
  }
  case class Falsified(failedCase: FailedCase, successCount: SuccessCount) extends Result {
    override val isFalsified: Boolean = true
  }

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = Prop {
    (testCases, rng, lineage, _) => {
      @tailrec
      def loop(
        rng: RNG,
        testCases: TestCases,
        successCount: SuccessCount
      ): Result = testCases match {
        case i if i <= 0 =>
          Passed
        case i if i > 0 =>
          val (a, newRng) = gen.sample.run(rng)
          if (f(a)) {
            loop(newRng, testCases - 1, successCount + 1)
          } else {
            Falsified(
              lineage.map(l => s"$l: ").getOrElse("") + s""""$a"""",
              successCount
            )
          }
      }

      loop(rng, testCases, 0)
    }
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = Prop {
    (testCases, rng, lineage, maxSize) =>
      forAll(g.forSize(maxSize))(f).run(testCases, rng, lineage, maxSize)
  }
}

case class Gen[A](sample: State[RNG, A]) {
  import Gen._

  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(f(_).sample))

  def listOf(g: Gen[Int]): Gen[List[A]] =
    g.flatMap(Gen.listOfN(_, this))

  def unsized: SGen[A] = SGen(_ => this)
}

object Gen {
  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.boolean))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeLessThan(stopExclusive - start)).map(_ + start))

  def chooseTwo(start: Int, stopExclusive: Int): Gen[(Int, Int)] = {
    val chooseOne = choose(start, stopExclusive).sample
    val chooseTwo = chooseOne.map2(chooseOne) { (_, _) }
    Gen(chooseTwo)
  }

  def option[A](g: Gen[A]): Gen[Option[A]] =
    Gen(g.sample.map(Option.apply))

  def string(maxLength: Int): Gen[String] = {
    val length = RNG.nonNegativeLessThan(maxLength)

    val codes = RNG.flatMap(length) { n =>
      RNG.sequence(
        List.fill(n)(
          RNG.map(RNG.nonNegativeLessThan(126 - 32)) { i =>
            (i + 32).toChar.toString
          }
        )
      )
    }

    val string = RNG.map(codes)(_.mkString)

    Gen(State(string))
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap {
      case true => g1
      case false => g2
    }

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val (d1, d2) = (g1._2.abs, g2._2.abs)
    val threshold = d1 / (d1 + d2)

    val sample =
      State(RNG.double).flatMap { d =>
        if (d <= threshold) {
          g1._1.sample
        } else {
          g2._1.sample
        }
      }

    Gen(sample)
  }
}

case class SGen[A](forSize: Int => Gen[A]) {
  def map[B](f: A => B): SGen[B] =
    SGen(i => forSize(i).map(f))

  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen(i => forSize(i).flatMap(f(_).forSize(i)))
}

object SGen {
  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(i => Gen.listOfN(i, g))
}
