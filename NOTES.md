# Functional programming

## What is functional programming (FP)?

* A function has a _side effect_ if it does something other than just return a result.

* Examples of side effects:
  - Modify loop variable
  - Mutate data structure
  - Set field on object
  - Throw exception
  - Read input
  - Print output
  - Invoke API

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
  - Set field on object -> Return new (immutable) object (`anObject.copy(aField = aValue)`)
  - Throw exception -> Return exception object (`Either[Value, Exception]`)
  - Invoke API -> Return invocation object (`Invocation(anAPI, anArgument)`) 

* Hallmarks of functional programming:
  - Reification (of side effects, program state, and functions)
  - Higher-order functions (`someInts.reduce(addTwoInts)`)
  - Function combinators (`addTwoInts.andThen(multiplyByThree)`)
  - Immutable data structures (and data sharing)
  - Pattern matching over ADTs (`BinaryTree` can be `Leaf(v)` or `Branch(l, r)`)
  - Recursion (esp. tail recursion)
  - Laziness (of arguments and transformations)

* A major theme in FP -- tying together reification, HOFs, combinators, and laziness -- is to
  program in a way that separates the concerns of _description_ and _evaluation_.