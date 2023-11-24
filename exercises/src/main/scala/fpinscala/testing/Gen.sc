import fpinscala.state._
import fpinscala.testing._

val rng = RNG.Simple(100L)

Gen.unit("a").sample.run(rng)

Gen.string(7).sample.run(rng)

val prop = Prop.forAll(Gen.string(5))(_.length > 1)

prop.run(50, RNG.Simple(1), None, 0)

prop.run(50, RNG.Simple(42), None, 0)

prop.run(50, RNG.Simple(666), None, 0)

val compoundProp =
  Prop.forAll(Gen.string(5))(_.hashCode < 1000000) &&
    (Prop.forAll(Gen.string(10))(_.hashCode < 1000000000) || Prop.forAll(Gen.string(20))(_.length < 19))

compoundProp.run(50, RNG.Simple(1), Some(""), 0)

compoundProp.run(50, RNG.Simple(42), Some(""), 0)

compoundProp.run(50, RNG.Simple(666), Some(""), 0)

val otherCompoundProp =
  Prop.forAll(Gen.string(5)) { s => s.reverse.reverse == s} &&
    Prop.forAll(Gen.unit(42)) { i => i + 1 - 1 == i }

otherCompoundProp.run(50, RNG.Simple(1), None, 0)
