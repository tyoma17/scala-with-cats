package chapter10_data_validation

import cats.Semigroup
import cats.syntax.either._    // for asLeft and asRight
import cats.syntax.semigroup._ // for |+|

final case class CheckF[E, A](func: A => Either[E, A]) {
  def apply(a: A): Either[E, A] =
    func(a)

  def and(that: CheckF[E, A])(implicit s: Semigroup[E]): CheckF[E, A] =
    CheckF { a =>
      (this(a), that(a)) match {
        case (Left(e1), Left(e2)) => (e1 |+| e2).asLeft
        case (Left(e), Right(_))  => e.asLeft
        case (Right(_), Left(e))  => e.asLeft
        case (Right(_), Right(_)) => a.asRight
      }
    }
}

object DemoCheckF extends App {
  import cats.instances.list._ // for Semigroup

  val a: CheckF[List[String], Int] =
    CheckF { v =>
      if(v > 2) v.asRight
      else List("Must be > 2").asLeft
    }

  val b: CheckF[List[String], Int] =
    CheckF { v =>
      if(v < -2) v.asRight
      else List("Must be < -2").asLeft
    }

  val check: CheckF[List[String], Int] =
    a and b

  check(5)
  // res5: Either[List[String], Int] = Left(List("Must be < -2"))
  check(0)
  // res6: Either[List[String], Int] = Left(List("Must be > 2", "Must be < -2"))

  val a2: CheckF[Nothing, Int] =
    CheckF(v => v.asRight)

  val b2: CheckF[Nothing, Int] =
    CheckF(v => v.asRight)

//  val check2 = a2 and b2
  // error: could not find implicit value for parameter s: cats.Semigroup[Nothing]
  //   a and b
  //   ^^^^^^^
}