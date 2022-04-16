package chapter10_data_validation

import cats.Semigroup

object CheckDatatype extends App {

//  type Check[A] = A => Either[String, A]
//  type Check[E, A] = A => Either[E, A]

  trait Check_V1[E, A] {
    def apply(value: A): Either[E, A] = Right(value)

    def and(that: Check_V2[E, A]): Check_V2[E, A] =
      ???

    // other methods...
  }

  import cats.syntax.monoid._
  import cats.syntax.either._

  sealed trait Check_V2[E, A] {
    import Check_V2._

    def and(that: Check_V2[E, A]): Check_V2[E, A] =
      And(this, that)

    def apply(a: A)(implicit s: Semigroup[E]): Either[E, A] =
      this match {
        case Pure(func) =>
          func(a)

        case And(left, right) =>
          (left(a), right(a)) match {
            case (Left(e1), Left(e2)) => (e1 |+| e2).asLeft
            case (Left(e), Right(_))  => e.asLeft
            case (Right(_), Left(e))  => e.asLeft
            case (Right(_), Right(_)) => a.asRight
          }
      }
  }

  object Check_V2 {
    final case class And[E, A](left: Check_V2[E, A], right: Check_V2[E, A]) extends Check_V2[E, A]

    final case class Pure[E, A](func: A => Either[E, A]) extends Check_V2[E, A]

    def pure[E, A](f: A => Either[E, A]): Check_V2[E, A] =
      Pure(f)
  }

  val a: Check_V2[List[String], Int] =
    Check_V2.pure { v =>
      if (v > 2) v.asRight
      else List("Must be > 2").asLeft
    }

  val b: Check_V2[List[String], Int] =
    Check_V2.pure { v =>
      if (v < -2) v.asRight
      else List("Must be < -2").asLeft
    }

  val checkAnd: Check_V2[List[String], Int] =
    a and b

  import cats.instances.list._
  val checkRes = checkAnd(0)
 // Left(List(Must be > 2, Must be < -2))
  import cats.syntax.apply._
  import cats.data.Validated
  import cats.data.Validated._   // for Valid and Invalid
  sealed trait Check_V3[E, A] {
    import Check_V3._

    def and(that: Check_V3[E, A]): Check_V3[E, A] =
      And(this, that)

    def or(that: Check_V3[E, A]): Check_V3[E, A] =
      Or(this, that)

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
      this match {
        case Pure(func) =>
          func(a)

        case And(left, right) =>
          (left(a), right(a)).mapN((_, _) => a)

        case Or(left, right) =>
          left(a) match {
            case Valid(a)    => Valid(a)
            case Invalid(e1) =>
              right(a) match {
                case Valid(a) => Valid(a)
                case Invalid(e2) => Invalid(e1 |+| e2)
              }
          }
      }
  }

  object Check_V3 {
    final case class And[E, A](left: Check_V3[E, A], right: Check_V3[E, A]) extends Check_V3[E, A]
    final case class Or[E, A](left: Check_V3[E, A], right: Check_V3[E, A]) extends Check_V3[E, A]
    final case class Pure[E, A](func: A => Validated[E, A])                 extends Check_V3[E, A]
  }
}
