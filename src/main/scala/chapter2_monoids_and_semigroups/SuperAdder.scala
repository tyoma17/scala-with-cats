package chapter2_monoids_and_semigroups
import cats.instances.int.catsKernelStdGroupForInt
import cats.Monoid
import cats.implicits.catsSyntaxOptionId
import cats.syntax.semigroup._

object SuperAdder {
//  def add[A](items: List[A])(implicit monoid: Monoid[A]): A =
//    items.foldLeft(monoid.empty)(_ |+| _)

  // shorter version
  def add[A: Monoid](items: List[A]): A = // 'A: Monoid' is context bound syntax. A has-a Monoid[A]
    items.foldLeft(Monoid[A].empty)(_ |+| _)
}

object Demo extends App {
  println(SuperAdder.add(List(1, 2, 3, 4)))
  import cats.instances.option.catsKernelStdMonoidForOption
  println(SuperAdder.add(List(1.some, 2.some, 3.some, 4.some)))
}
