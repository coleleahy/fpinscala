# What is functional programming (FP)?

* A function has a _side effect_ if it does something other than just return a result.

* Examples of side effects:
  - Modify loop variable
  - Mutate data structure
  - Set field on object
  - Read input
  - Print output
  - Invoke API
  - Throw exception

* In contrast with _procedural_ and _object-oriented_ programming, _functional_ programming
  favors _pure_ functions, i.e. functions without side effects.

* Downsides of programs using impure functions:
  - Harder to reason about (not _referentially transparent_, so can't substitute to simplify)
  - Less reusable/modular/composable (not all clients will want _that_ side effect)
  - Difficult to parallelize (concurrent mutation of shared variable, deadlock, etc.)
  - Difficult to unit test (suite shouldn't _actually_ call the 3rd-party API)

* How to render an impure function pure? _Reify_ the would-be side effect as an object
  in its own right, to be processed/evaluated/interpreted/invoked elsewhere.

* Examples of reifying a would-be side effect:
  - Modify loop variable -> Track it as an argument (`factorial(n - 1, n * valueSoFar)`)
  - Set field on object -> Return new (immutable) object (`randomNumberGen.copy(seed = newSeed)`)
  - Invoke API -> Return invocation object (`Charge(paymentsAPI, creditCardInfo)`) 
  - Throw exception -> Return exception object (`Either[PaymentConfirmation, Exception]`)

* Hallmarks of functional programming:
  - Reification (of side effects, functions, and state)
  - Higher-order functions (`someInts.reduce(addTwoInts)`)
  - Function "combinators" (`addTwoInts.andThen(multiplyByThree)`)
  - Immutable data structures and data sharing (`someInts.prepended(42)`)
  - Pattern matching over ADTs (`BinaryTreeNode` can be `Leaf(value)` or `Branch(left, right)`)
  - Recursion (esp. tail recursion to avoid stack overflow)
  - Laziness (of arguments and transformations)

* A major theme in FP -- tying together reification, HOFs, combinators, and laziness -- is to
  program in a way that separates the concerns of _description_ and _evaluation_. This enables
  a _declarative_ style of programming where you simply describe _what_ you want, and let an
  evaluation engine decide _how_ optimally to reach that goal.

* Examples of declarative style, enabled by separation of description and evaluation:
  - Transforming tabular data with SQL (`employees.groupBy(department).agg(max(salary)).where(department === "Sales")`)
  - Transforming infinite streams (`Stream.from(0).map(multiplyByThree).filter(isGreaterThanTen)`)