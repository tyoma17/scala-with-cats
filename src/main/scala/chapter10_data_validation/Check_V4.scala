package chapter10_data_validation

import cats.Semigroup
import cats.data.Validated

trait Check_V4[E, A, B] {
  import Check_V4._
  def apply(in: A)(implicit s: Semigroup[E]): Validated[E, B]

  def flatMap[C](f: B => Check_V4[E, A, C]) =
    FlatMap[E, A, B, C](this, f)

  def map[C](f: B => C): Check_V4[E, A, C] =
    Map[E, A, B, C](this, f)

  def andThen[C](that: Check_V4[E, B, C]): Check_V4[E, A, C] =
    AndThen[E, A, B, C](this, that)
}

object Check_V4 {
  final case class Map[E, A, B, C](check: Check_V4[E, A, B], func: B => C) extends Check_V4[E, A, C] {

    def apply(in: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check(in).map(func)
  }

  final case class FlatMap[E, A, B, C](check: Check_V4[E, A, B], func: B => Check_V4[E, A, C])
      extends Check_V4[E, A, C] {

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check(a).withEither(_.flatMap(b => func(b)(a).toEither))
  }

  final case class AndThen[E, A, B, C](check1: Check_V4[E, A, B], check2: Check_V4[E, B, C]) extends Check_V4[E, A, C] {

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check1(a).withEither(_.flatMap(b => check2(b).toEither))
  }

  final case class Pure[E, A, B](func: A => Validated[E, B]) extends Check_V4[E, A, B] {

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, B] =
      func(a)
  }

  final case class PurePredicate[E, A](pred: Predicate[E, A]) extends Check_V4[E, A, A] {

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
      pred(a)
  }

  def apply[E, A](pred: Predicate[E, A]): Check_V4[E, A, A] =
    PurePredicate(pred)

  def apply[E, A, B](func: A => Validated[E, B]): Check_V4[E, A, B] =
    Pure(func)
}

object Demo_V4 extends App {
  import cats.data.{NonEmptyList, Validated}

  type Errors = NonEmptyList[String]

  def error(s: String): NonEmptyList[String] =
    NonEmptyList(s, Nil)

  def longerThan(n: Int): Predicate[Errors, String] =
    Predicate.lift(error(s"Must be longer than $n characters"), str => str.size > n)

  val alphanumeric: Predicate[Errors, String] =
    Predicate.lift(error(s"Must be all alphanumeric characters"), str => str.forall(_.isLetterOrDigit))

  def contains(char: Char): Predicate[Errors, String] =
    Predicate.lift(error(s"Must contain the character $char"), str => str.contains(char))

  def containsOnce(char: Char): Predicate[Errors, String] =
    Predicate.lift(error(s"Must contain the character $char only once"), str => str.filter(c => c == char).size == 1)

  // A username must contain at least four characters
  // and consist entirely of alphanumeric characters

  val checkUsername: Check_V4[Errors, String, String] =
    Check_V4(longerThan(3) and alphanumeric)

  // An email address must contain a single `@` sign.
  // Split the string at the `@`.
  // The string to the left must not be empty.
  // The string to the right must be
  // at least three characters long and contain a dot.

  import cats.syntax.apply._     // for mapN
  import cats.syntax.validated._ // for valid and invalid

  val splitEmail: Check_V4[Errors, String, (String, String)] =
    Check_V4(_.split('@') match {
      case Array(name, domain) =>
        (name, domain).validNel[String]

      case _ =>
        "Must contain a single @ character".invalidNel[(String, String)]
    })

  val checkLeft: Check_V4[Errors, String, String] =
    Check_V4(longerThan(0))

  val checkRight: Check_V4[Errors, String, String] =
    Check_V4(longerThan(3) and contains('.'))

  val joinEmail: Check_V4[Errors, (String, String), String] =
    Check_V4 { case (l, r) =>
      (checkLeft(l), checkRight(r)).mapN(_ + "@" + _)
    }

  val checkEmail: Check_V4[Errors, String, String] =
    splitEmail andThen joinEmail

  final case class User(username: String, email: String)

  def createUser(username: String, email: String): Validated[Errors, User] =
    (checkUsername(username), checkEmail(email)).mapN(User)

  createUser("Noel", "noel@underscore.io")
  // res5: Validated[Errors, User] = Valid(User("Noel", "noel@underscore.io"))
  createUser("", "dave@underscore.io@io")
  // res6: Validated[Errors, User] = Invalid(
  //   NonEmptyList(
  //     "Must be longer than 3 characters",
  //     List("Must contain a single @ character")
  //   )
  // )
}
