import fpinscala.gettingstarted.MyModule

MyModule.main(Array.empty[String])

MyModule.abs(-33)

(1 to 10).foreach { i =>  println(MyModule.fib(i)) }

import fpinscala.gettingstarted.TestFib

TestFib.main(Array.empty[String])

7 / 2

7.0 / 2

import fpinscala.gettingstarted.PolymorphicFunctions

PolymorphicFunctions.isSorted(
  Array(1),
  (x: Int, y: Int) => x <= y
)

