package chapter6_semigroupal_and_applicative

object FormValidation extends App {

  import cats.syntax.either._

  def parseInt(str: String): Either[String, Int] =
    Either.catchOnly[NumberFormatException](str.toInt).
      leftMap(_ => s"Couldn't read $str")

  val res0 = for {
    a <- parseInt("a")
    b <- parseInt("b")
    c <- parseInt("c")
  } yield a + b + c
  // res0: Either[String, Int] = Left("Couldn't read a")


}
