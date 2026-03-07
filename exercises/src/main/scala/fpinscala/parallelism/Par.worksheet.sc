import fpinscala.parallelism.Par.Par
import fpinscala.parallelism._

import java.util.concurrent.{ExecutorService, Executors, TimeUnit}

val es: ExecutorService = Executors.newCachedThreadPool()

Par.parFilterOldOld(List(1, 2, 3, 4, 5))(_ % 2 == 0).apply(es).get()

Par.parFilterOld(List(1, 2, 3, 4, 5))(_ % 2 == 0).apply(es).get()

Par.parFilter(List(1, 2, 3, 4, 5))(_ % 2 == 0).apply(es).get()

Par
  .map3(
    Par.lazyUnit(1),
    Par.lazyUnit(2),
    Par.lazyUnit(3)
  )(_ + _ + _)
  .apply(es)
  .get()

Examples
  .wordCount(
    Seq(
      "I ate a dog",
      "That is right"
    )
  )
  .apply(es)
  .get()

val hello: Par[String] = Par.fork {
  Par.unit {
    Thread.sleep(20)
    "hello"
  }
}

val world: Par[String] = Par.fork {
  Par.unit {
    Thread.sleep(40)
    "world"
  }
}

val helloWorldOld = Par.map2Old(hello, world)(_ + " " + _)
helloWorldOld(es).get(30, TimeUnit.MILLISECONDS)

// .get(30, ...) will throw a timeout exception
val helloWorld = Par.map2(hello, world)(_ + " " + _)
helloWorld(es).get(70, TimeUnit.MILLISECONDS)
