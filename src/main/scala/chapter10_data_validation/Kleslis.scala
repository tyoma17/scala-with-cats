package chapter10_data_validation

object Kleslis extends App {

  import cats.data.Kleisli
  import cats.instances.list._ // for Monad

  val step1: Kleisli[List, Int, Int] =
    Kleisli(x => List(x + 1, x - 1))

  val step2: Kleisli[List, Int, Int] =
    Kleisli(x => List(x, -x))

  val step3: Kleisli[List, Int, Int] =
    Kleisli(x => List(x * 2, x / 2))

  val pipeline = step1 andThen step2 andThen step3

  pipeline.run(20)
}
