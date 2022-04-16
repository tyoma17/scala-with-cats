package chapter4_monads

object MonadSyntax extends App {

  import cats.instances.option._   // for Monad
  import cats.instances.list._     // for Monad
  import cats.syntax.applicative._ // for pure

  1.pure[Option]
  // res5: Option[Int] = Some(1)
  1.pure[List]
  // res6: List[Int] = List(1)

  import cats.Monad
  import cats.syntax.functor._ // for map
  import cats.syntax.flatMap._ // for flatMap

//  def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
//    a.flatMap(x => b.map(y => x * x + y * y))

  def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
    for {
      x <- a
      y <- b
    } yield x*x + y*y

  sumSquare(Option(3), Option(4))
  // res7: Option[Int] = Some(25)
  sumSquare(List(1, 2, 3), List(4, 5))
  // res8: List[Int] = List(17, 26, 20, 29, 25, 34)
}
