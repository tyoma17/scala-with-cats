package chapter2_monoids_and_semigroups

object TheTruthAboutMonoids extends App {

  implicit val orBooleanMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = false
    override def combine(x: Boolean, y: Boolean): Boolean = x || y
  }

  implicit val andBooleanMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = true
    override def combine(x: Boolean, y: Boolean): Boolean = x && y
  }

  println(MonoidLaws.identityLaw(true)(orBooleanMonoid))
  println(MonoidLaws.identityLaw(false)(orBooleanMonoid))
  println(MonoidLaws.associativeLaw(true, true, true)(orBooleanMonoid))
  println(MonoidLaws.associativeLaw(true, false, true)(orBooleanMonoid))
  println(MonoidLaws.associativeLaw(false, true, true)(orBooleanMonoid))
  println(MonoidLaws.associativeLaw(false, false, false)(orBooleanMonoid))

  println(MonoidLaws.identityLaw(true)(andBooleanMonoid))
  println(MonoidLaws.identityLaw(false)(andBooleanMonoid))
  println(MonoidLaws.associativeLaw(true, true, true)(andBooleanMonoid))
  println(MonoidLaws.associativeLaw(true, false, false)(andBooleanMonoid))
  println(MonoidLaws.associativeLaw(false, true, false)(andBooleanMonoid))
  println(MonoidLaws.associativeLaw(false, false, false)(andBooleanMonoid))

  implicit def setUnionMonoid[A]: Monoid[Set[A]] =
    new Monoid[Set[A]] {
      def combine(a: Set[A], b: Set[A]) = a union b
      def empty = Set.empty[A]
    }

  val s1 = Set(1, 2, 3)
  val s2 = Set(3, 4, 5)
  val sUnion = Set(1, 2, 3, 4, 5)
  println(MonoidLaws.identityLaw(s1)(setUnionMonoid[Int]))
  println(MonoidLaws.identityLaw(s2)(setUnionMonoid[Int]))
  println(MonoidLaws.associativeLaw(s1, s1, s1)(setUnionMonoid[Int]))
  println(MonoidLaws.associativeLaw(s2, s2, s2)(setUnionMonoid[Int]))
  println(MonoidLaws.associativeLaw(s1, s2, sUnion)(setUnionMonoid[Int]))
  println(MonoidLaws.associativeLaw(s2, s1, sUnion)(setUnionMonoid[Int]))

  val intSetMonoid = Monoid[Set[Int]]
}
