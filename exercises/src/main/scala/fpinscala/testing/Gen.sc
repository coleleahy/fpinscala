import fpinscala.state._
import fpinscala.testing._

val rng = RNG.Simple(100L)

Gen.unit("a").sample.run(rng)

Gen.string(7).sample.run(rng)

Prop.forAll(Gen.string(5))(_.contains("W")).run(RNG.Simple(1), 5)