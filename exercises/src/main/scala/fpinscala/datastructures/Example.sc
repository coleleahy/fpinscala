val o1: Option[Int] = Option(1)
val o2: Option[Int] = Option(2)
val o3: Option[Int] = Option(3)

val os: List[Option[Int]] = List(o1, o2, o3)

val sequenced: Option[List[Int]] =
  for {
    i1 <- o1
    i2 <- o2
    i3 <- o3
  } yield List(i1, i2, i3)

val sequencedVerbose: Option[List[Int]] =
  o1.flatMap { i1 =>
    o2.flatMap { i2 =>
      o3.map { i3 =>
        List(i1, i2, i3)
      }
    }
  }