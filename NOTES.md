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
  - Less compile-time safety (compiler only checks type signatures, not side effects)
  - Less reusable/modular/composable (not all clients will want _that_ side effect)
  - Difficult to parallelize (concurrent mutation of shared variable, deadlock, etc.)
  - Difficult to unit test (suite shouldn't _actually_ call the 3rd-party API)

* How to render an impure function pure? _Reify_ the would-be side effect as an object
  in its own right, to be processed/evaluated/interpreted/invoked elsewhere (separation of
  concerns).

* Examples of reifying a would-be side effect:
  - Modify loop variable -> Track it as an argument (`factorial(n - 1, n * valueSoFar)`)
  - Set field on object -> Return new (immutable) object (`randomNumberGen.copy(seed = newSeed)`)
  - Invoke API -> Return invocation object (`Charge(paymentsAPI, creditCardInfo)`) 
  - Throw exception -> Return exception object (`Either[Exception, PaymentConfirmation]`)

* Hallmarks of functional programming:
  - Reification (of side effects, functions, and state)
  - Higher-order functions (`reduce` in `someIntegers.reduce(addTwoIntegers)`)
  - Combinator functions (`andThen` in `addTwoIntegers.andThen(findSquareRoot)`)
  - Immutable data structures, data sharing (`someIntegers.prepended(42)`)
  - Algebraic data types, pattern matching (`BinaryTreeNode` can be `Leaf(value)` or `Branch(left, right)`)
  - Ad-hoc polymorphism (typeclasses) vs. subtype polymorphism (inheritance)
  - Laziness (of arguments and transformations)
  - Recursion (esp. tail recursion to avoid stack overflow)

* A major theme in FP is that diverse problems, in unrelated domains, can often be modeled
  in a way that reveals a fundamental "functional" structure that they share in common, and in light
  of which they can be solved using basic FP idioms (`map`, `flatMap`, `traverse`, and so on).

* Another major theme in FP -- tying together reification, HOFs, combinators, and laziness -- is to
  program in a way that separates the concerns of _description_ and _evaluation_. This enables
  a _declarative_ style of programming where you simply describe _what_ you want, and let an
  evaluation engine decide _how_ optimally to reach that goal. The _what_ parts can be composed in
  a modular (readable) way without performance penalties. For example:
  - SQL transformations (`employees.groupBy(department).agg(max(salary)).where(department === "Sales")`)
  - Stream transformations (`Stream.from(0).map(multiplyByThree).filter(isGreaterThanTen)`)
  - Property-based testing (`forall { (s: String) => s.reverse.reverse == s } && forAll { (i: Int) i - 1 + 1 == i}`)

* _Typeclasses_ (ad-hoc polymorphism) are another major theme in FP. While this theme is somewhat
  orthogonal to the idea of programming with pure functions, it's nevertheless aligned with the goal
  of writing modules amenable to _reuse_ and _composition_. Typeclasses package ancillary behaviors
  _separately_ from the core class, rather than baking them in by inheritance. Users of the class
  can choose which behaviors to opt into as needed. The class thus carries fewer dependencies,
  improving reusability. Furthermore, code can depend only on the required behaviors, but in a way
  that's more flexible than depending on an interface: classes that are "closed" for modification
  (e.g. language built-ins or third-party libraries) are still "open" for extension with new behaviors
  even though their source can't be modified. The code can thus be reused widely. Finally, typeclasses
  can support _law-like reasoning_ about your code, which aligns with FP's emphasis on mathematics-like
  functions.

* Loss of efficiency is a frequent pitfall that must be mitigated. For instance when composing sequences
  of HOFs over a collection (`myList.map(f).flatMap(g).takeWhile(p)`) we should avoid making multiple
  passes that do a number of operations proportional to the size of the original collection. (This is
  why lazy streams, rather than strict lists, are so popular in FP.) Likewise, when reifying state,
  we lose the efficiency we would have had if we could just mutate stateful variables in-place; instead
  we need to create an entirely new copy of the state object to represent the "next" state, thus making
  more work for the garbage collector.
* composing HOFs (but this can be mitigated in some cases,
  e.g. by using lazy collections like streams instead of strict collections like lists), and when