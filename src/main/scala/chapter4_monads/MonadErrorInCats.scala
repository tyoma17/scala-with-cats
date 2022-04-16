package chapter4_monads

object MonadErrorInCats extends App {

  import cats.MonadError
  import cats.instances.either.catsStdInstancesForEither // for MonadError

  type ErrorOr[A] = Either[String, A]
  val monadError = MonadError[ErrorOr, String]

  val success = monadError.pure(42)
  // success: ErrorOr[Int] = Right(42)
  val failure = monadError.raiseError("Badness")
  // failure: ErrorOr[Nothing] = Left("Badness")

  monadError.handleErrorWith(failure) {
    case "Badness" =>
      monadError.pure("It's ok")

    case _ =>
      monadError.raiseError("It's not ok")
  }
  // res0: ErrorOr[String] = Right("It's ok")

  monadError.handleError(failure) {
    case "Badness" => 42
    case _ => -1
  }
  // res1: ErrorOr[Int] = Right(42)

  monadError.ensure(success)("Number too low!")(_ > 1000)
  // res2: ErrorOr[Int] = Left("Number too low!")

  import cats.syntax.applicative._      // for pure
  import cats.syntax.applicativeError._ // for raiseError etc
  import cats.syntax.monadError._       // for ensure

  val success2 = 42.pure[ErrorOr]
  // success: ErrorOr[Int] = Right(42)
  val failure2 = "Badness".raiseError[ErrorOr, Int]
  // failure: ErrorOr[Int] = Left("Badness")
  failure2.handleErrorWith{
    case "Badness" =>
      256.pure

    case _ =>
      ("It's not ok").raiseError
  }
  // res4: ErrorOr[Int] = Right(256)
  success2.ensure("Number to low!")(_ > 1000)
  // res5: ErrorOr[Int] = Left("Number to low!")

  // Instances of MonadError
  import scala.util.Try
  import cats.instances.try_._ // for MonadError
  val exn: Throwable =
    new RuntimeException("It's all gone wrong")

  exn.raiseError[Try, Int]
  // res6: Try[Int] = Failure(java.lang.RuntimeException: It's all gone wrong)

  def validateAdult[F[_]](age: Int)(implicit me: MonadError[F, Throwable]): F[Int] = {
    me.ensure(age.pure[F])(new IllegalArgumentException("Age must be greater than or equal to 18"))(_ >= 18)
//    if(age >= 18) age.pure[F]
//    else new IllegalArgumentException("Age must be greater than or equal to 18").raiseError[F, Int]
  }

  val res7 = validateAdult[Try](18)
  println(res7)
  // res7: Try[Int] = Success(18)
  val res8 = validateAdult[Try](8)
  println(res8)
  // res8: Try[Int] = Failure(
  // java.lang.IllegalArgumentException: Age must be greater than
  // or equal to 18
  // )
  type ExceptionOr[A] = Either[Throwable, A]
  val res9 = validateAdult[ExceptionOr](-1)
  println(res9)
  // res9: ExceptionOr[Int] = Left(
  //   java.lang.IllegalArgumentException: Age must be greater than or equal to 18
  // )
}
