import fpinscala.state._

val rng = RNG.Simple(42)

RNG.nonNegativeInt(rng)._1

RNG.double(rng)._1

RNG.ints(4)(rng)._1

RNG.intsImperative(4)(rng)._1

RNG.sequence(
  RNG.unit(42) ::
    RNG.unit(66) ::
    RNG.unit(39) ::
    Nil
) { rng }

RNG.flatMap(RNG.int) { n => RNG.nonNegativeLessThan(n) }.apply(rng)

Candy
  .simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn))
  .run(Machine(locked = true, candies = 5, coins = 10))
  ._1