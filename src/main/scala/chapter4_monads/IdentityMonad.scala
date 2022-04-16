package chapter4_monads

import cats.Id
import chapter4_monads.MonadSyntax.sumSquare

object IdentityMonad extends App {

  // sumSquare(3, 4)
  // error: no type parameters for method sumSquare: (a: F[Int], b: F[Int])(implicit evidence$1: cats.Monad[F])F[Int] exist so that it can be applied to arguments (Int, Int)
  //  --- because ---
  // argument expression's type is not compatible with formal parameter type;
  //  found   : 3
  //  required: ?F[Int]
  // error: type mismatch;
  //  found   : Int(3)
  //  required: F[Int]
  // error: type mismatch;
  //  found   : Int(4)
  //  required: F[Int]

  sumSquare(3: Id[Int], 4: Id[Int])
  // res1: Id[Int] = 25

  val res2 = "Dave" : Id[String]
  // res2: Id[String] = "Dave"
  val res3 = 123 : Id[Int]
  // res3: Id[Int] = 123
  val res4 = List(1, 2, 3) : Id[List[Int]]
  // res4: Id[List[Int]] = List(1, 2, 3)

  import cats.Monad
  val a = Monad[Id].pure(3)
  // a: Id[Int] = 3
  val b = Monad[Id].flatMap(a)(_ + 1)
  // b: Id[Int] = 4

  import cats.syntax.functor._
  import cats.syntax.flatMap._
  for {
    x <- a
    y <- b
  } yield x + y
  // res5: Id[Int] = 7

  // Monadic secret identities
  import cats.Id

  def pure[A](value: A): Id[A] = value

  def map[A, B](initial: Id[A])(func: A => B): Id[B] =
    func(initial)

  def flatMap[A, B](initial: Id[A])(func: A => Id[B]): Id[B] =
    func(initial)
}
