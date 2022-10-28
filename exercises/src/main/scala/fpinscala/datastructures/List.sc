import fpinscala.datastructures.{List, Nil}

List.drop(List(1, 2, 3, 4, 5, 6), 3)

List.drop(List(1, 2, 3, 4, 5, 6), 0)

List.dropWhile(List(1, 3, 5, 4, 5, 6, 7), (i: Int) => i % 2 == 1)

val ints = List(1 to 99999: _*)

List.reverse(ints)

List.tail(ints)

List.tail(List.reverse(ints))

List.reverse(List.tail(List.reverse(ints)))

List.init(ints)

val xs = List(1.0, 2.0, 3.0, 0.0, 4.0, 5.0)

List.product2(xs)

List.product2Lazy(xs)

List.reverseViaFoldLeft(xs)

List.filter(List(1, 2, 3, 4, 5)) { _ % 2 == 0}

List.flatMap(List(1, 2, 3, 4)) {
  _ match {
    case x if x % 2 == 0 => Nil
    case x => List(x, x)
  }
}

List.zip(List(1, 2, 3), List(3, 2, 1))

List.zipWith(List(1, 2, 3), List(3, 2, 1))(_ + _)

List.hasSubsequence(List(1, 2, 3, 4), List(1, 2))
List.hasSubsequence(List(1, 2, 3, 4), List(2, 3))
List.hasSubsequence(List(1, 2, 3, 4), List(4))
List.hasSubsequence(List(1, 2, 3, 4), Nil)
List.hasSubsequence(Nil, Nil)
List.hasSubsequence(List(1, 2, 3, 4), List(1, 2, 4))
List.hasSubsequence(Nil, List(1, 2, 4))