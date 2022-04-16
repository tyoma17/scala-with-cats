package chapter6_semigroupal_and_applicative

import cats.{Applicative, Monad, ~>}

object Parallel2 extends App { // to distinguish from Cats' Semigroupal

  import cats.Semigroupal
  import cats.instances.either._ // for Semigroupal

  type ErrorOr[A] = Either[Vector[String], A]
  val error1: ErrorOr[Int] = Left(Vector("Error 1"))
  val error2: ErrorOr[Int] = Left(Vector("Error 2"))

  Semigroupal[ErrorOr].product(error1, error2)
  // res0: ErrorOr[(Int, Int)] = Left(Vector("Error 1"))

  import cats.syntax.apply._ // for tupled

  (error1, error2).tupled
  // res1: ErrorOr[(Int, Int)] = Left(Vector("Error 1"))

  import cats.syntax.parallel._ // for parTupled
  import cats.instances.vector._

  val res2 = (error1, error2).parTupled
  // res2: ErrorOr[(Int, Int)] = Left(Vector("Error 1", "Error 2"))

  type ErrorOrList[A] = Either[List[String], A]
  val errStr1: ErrorOrList[Int] = Left(List("error 1"))
  val errStr2: ErrorOrList[Int] = Left(List("error 2"))

  import cats.instances.list._
  (errStr1, errStr2).parTupled
  // res3: ErrorOrList[(Int, Int)] = Left(List("error 1", "error 2"))

  val success1: ErrorOr[Int] = Right(1)
  val success2: ErrorOr[Int] = Right(2)
  val addTwo                 = (x: Int, y: Int) => x + y

  (error1, error2).parMapN(addTwo)
  // res4: ErrorOr[Int] = Left(Vector("Error 1", "Error 2"))
  (success1, success2).parMapN(addTwo)
  // res5: ErrorOr[Int] = Right(3)

  trait Parallel[M[_]] {
    type F[_]

    def applicative: Applicative[F]
    def monad: Monad[M]
    def parallel: ~>[M, F]
  }

  import cats.arrow.FunctionK

  object optionToList extends FunctionK[Option, List] {
    def apply[A](fa: Option[A]): List[A] =
      fa match {
        case None    => List.empty[A]
        case Some(a) => List(a)
      }
  }

  optionToList(Some(1))
  // res6: List[Int] = List(1)
  optionToList(None)
  // res7: List[Nothing] = List()

  // Parallel list
  import cats.instances.list._

  val res8 = (List(1, 2), List(3, 4)).tupled
  // res8: List[(Int, Int)] = List((1, 3), (1, 4), (2, 3), (2, 4))
  val res9 = (List(1, 2), List(3, 4)).parTupled
  // res9: List[(Int, Int)] = List((1, 3), (2, 4))
}
