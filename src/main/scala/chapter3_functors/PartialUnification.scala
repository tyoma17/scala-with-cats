package chapter3_functors

object PartialUnification extends App {
  import cats.instances.function._ // for Functor
  import cats.syntax.functor._     // for map
  val func1 = (x: Int) => x.toDouble
  val func2 = (y: Double) => y.toString
  val func3 = func1.map(func2)

  val func3a: Int => String =
    a => func2(func1(a))

  val func3b: Int => String =
    func2.compose(func1)

  // Hypothetical example. This won't actually compile:
  import cats.syntax.contravariant._ // for contramap
//  val func3c: Int => String =
//    func2.contramap(func1)
  type <=[B, A] = A => B
  val func2b: String <= Double = func2
  val func3d: String <= Int    = func2b.contramap(func1)
}
