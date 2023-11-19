package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors, ExecutorService}

case class Prop(run: (RNG, TestCases) => Result) {}

object Prop {
  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    override val isFalsified: Boolean = false
  }
  case class Falsified(failedCase: FailedCase, successCount: SuccessCount) extends Result {
    override val isFalsified: Boolean = true
  }

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop =
    Prop { (rng, testCases) =>
      val (failedCase, successCount) =
        Gen.listOfN(testCases, gen)
          .map { as =>
            as.foldLeft[(FailedCase, SuccessCount)](("", 0)) { (acc, a) =>
              if (f(a)) {
                (acc._1, acc._2 + 1)
              } else {
                (acc._1 + ", " + a.toString, acc._2)
              }
            }
          }
          .sample
          .run(rng)
          ._1

      if (successCount < testCases) {
        Falsified(failedCase, successCount)
      } else {
        Passed
      }
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

trait SGen[+A] {}
