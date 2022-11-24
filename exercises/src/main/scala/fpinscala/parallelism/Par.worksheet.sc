import fpinscala.parallelism._
import java.util.concurrent.{ExecutorService, Executors}

val es: ExecutorService = Executors.newCachedThreadPool()

Par.parFilterOldOld(List(1, 2, 3, 4, 5))(_ % 2 == 0).apply(es).get()

Par.parFilterOld(List(1, 2, 3, 4, 5))(_ % 2 == 0).apply(es).get()

Par.parFilter(List(1, 2, 3, 4, 5))(_ % 2 == 0).apply(es).get()

Par.map3(
  Par.lazyUnit(1),
  Par.lazyUnit(2),
  Par.lazyUnit(3)
)(_ + _ + _).apply(es).get()

Examples.wordCount(
  Seq(
    "I ate a dog",
    "That is right"
  )
).apply(es).get()

1 + 3

2 + 4
