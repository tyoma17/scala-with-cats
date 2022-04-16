package chapter2_monoids_and_semigroups

object MonoidsInCats extends App {

  import cats.Monoid

  // Monoid instance

  import cats.instances.string.catsKernelStdMonoidForString
  Monoid[String].combine("Hi ", "there")
  // res0: String = "Hi there"
  Monoid[String].empty
  // res1: String = ""

  import cats.Semigroup

  // Semigroup instance
  Semigroup[String].combine("Hi ", "there")
  // res4: String = "Hi there"

  val a = Option(22)
  // a: Option[Int] = Some(22)
  val b = Option(20)
  // b: Option[Int] = Some(20)

  import cats.instances.option.catsKernelStdMonoidForOption
  import cats.instances.int.catsKernelStdGroupForInt
  Monoid[Option[Int]].combine(a, b)
  // res6: Option[Int] = Some(42)

  import cats.syntax.semigroup._
  a |+| b

}
