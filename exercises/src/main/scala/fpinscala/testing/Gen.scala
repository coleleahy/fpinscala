package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount] = ???
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

//trait Gen[A] {
//  def map[A,B](f: A => B): Gen[B] = ???
//  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
//}

case class Gen[A](sample: State[RNG, A]) {
  import Gen._

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(f(_).sample))

  def listOf(g: Gen[Int]): Gen[List[A]] =
    g.flatMap(Gen.listOfN(_, this))

  def union(other: Gen[A]): Gen[A] = {
    boolean.flatMap {
      case true => this
      case false => other
    }
  }
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

  def choose2(start: Int, stopExclusive: Int): Gen[(Int, Int)] = {
    val choose1 = choose(start, stopExclusive).sample
    val choose2 = choose1.map2(choose1) { (_, _) }
    Gen(choose2)
  }

  def option[A](g: Gen[A]): Gen[Option[A]] =
    Gen(g.sample.map(Option.apply))
}

trait SGen[+A] {

}

