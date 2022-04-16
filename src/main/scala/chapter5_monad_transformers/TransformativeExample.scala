package chapter5_monad_transformers

object TransformativeExample extends App {

  import cats.data.OptionT
  type ListOption[A] = OptionT[List, A]

  import cats.instances.list._     // for Monad
  import cats.syntax.applicative._ // for pure

  val result1: ListOption[Int] = OptionT(List(Option(10)))
  // result1: ListOption[Int] = OptionT(List(Some(10)))
  val result2: ListOption[Int] = 32.pure[ListOption]
  // result2: ListOption[Int] = OptionT(List(Some(32)))
  val result3: ListOption[Int] = OptionT(List(None: Option[Nothing]))
  // result3: ListOption[Int] = OptionT(List(None))
  val result4: ListOption[Int] = OptionT.none[List, Int]
  // result4: ListOption[Int] = OptionT(List(None))

  val res1: ListOption[Int] = result1.flatMap(x => result2.map(y => x + y))
  // res1: OptionT[List, Int] = OptionT(List(Some(42)))

}
