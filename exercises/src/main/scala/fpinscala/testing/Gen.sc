import fpinscala.state._
import fpinscala.testing._

val rng = RNG.Simple(100L)

Gen.unit("a").sample.run(rng)

Gen.string(7).sample.run(rng)

Prop.forAll(Gen.string(5))(_.length > 1).run(50, RNG.Simple(1))

Prop.forAll(Gen.string(5))(_.length > 1).run(50, RNG.Simple(42))

Prop.forAll(Gen.string(5))(_.length > 1).run(50, RNG.Simple(666))