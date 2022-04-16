package chapter5_monad_transformers

import scala.concurrent.Future

object MonadTransformersInCats extends App {

  import cats.data.OptionT
  type ListOption[A] = OptionT[List, A]

  // Alias Either to a type constructor with one parameter:
  type ErrorOr[A]       = Either[String, A]
  type ErrorOrOption[A] = OptionT[ErrorOr, A]

  import cats.syntax.applicative._
  import cats.instances.either._ // for Monad
  val a = 10.pure[ErrorOrOption]
  // a: ErrorOrOption[Int] = OptionT(Right(Some(10)))
  val b = 32.pure[ErrorOrOption]
  // b: ErrorOrOption[Int] = OptionT(Right(Some(32)))
  val c = for {
    a <- a
    b <- b
  } yield a + b
  // c: OptionT[ErrorOr, Int] = OptionT(Right(Some(42)))

  import cats.data.EitherT
  type FutureEither[A]       = EitherT[Future, String, A]
  type FutureEitherOption[A] = OptionT[FutureEither, A]

  import cats.instances.future._ // for Monad
  import scala.concurrent.ExecutionContext.Implicits.global
  val futureEitherOr: FutureEitherOption[Int] =
    for {
      a <- 10.pure[FutureEitherOption]
      b <- 32.pure[FutureEitherOption]
    } yield a + b
  // futureEitherOr: FutureEitherOption[Int] = OptionT(
  //   EitherT(Future(Success(Right(Some(42)))))
  // )

  import cats.instances.option._
  123.pure[EitherT[Option, String, *]] // Kind Projector

  // Constructing and Unpacking Instances

  // Create using apply:
  val errorStack1 = OptionT[ErrorOr, Int](Right(Some(10)))
  // errorStack1: OptionT[ErrorOr, Int] = OptionT(Right(Some(10)))
  val errorStack2 = 32.pure[ErrorOrOption]
  // errorStack2: ErrorOrOption[Int] = OptionT(Right(Some(32)))

  // Extracting the untransformed monad stack:
  val res4 = errorStack1.value
  // res4: ErrorOr[Option[Int]] = Right(Some(10))

  // Mapping over the Either in the stack:
  val res5 = errorStack2.value.map(_.getOrElse(-1))
  // res5: Either[String, Int] = Right(32)

  val intermediate = futureEitherOr.value
  // intermediate: FutureEither[Option[Int]] = EitherT(
  //   Future(Success(Right(Some(42))))
  // )

  val stack = intermediate.value
  // stack: Future[Either[String, Option[Int]]] = Future(Success(Right(Some(42))))

  import scala.concurrent.Await
  import scala.concurrent.duration._
  Await.result(stack, 1.second)
  // res7: Either[String, Option[Int]] = Right(Some(42))


}
